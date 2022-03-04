package app

import cats.effect._
import cats.implicits._
import com.monovore.decline._
import com.monovore.decline.effect._

import scala.collection.JavaConverters._

import org.ergoplatform.{ErgoAddress, Pay2SAddress, P2PKAddress}
import org.ergoplatform.appkit._

import configs.node.PaydayNodeConfig
import configs.parameters.PaydayParameters
import contracts.{GuapSwapProtocolFeeContract}
import protocol.PaydayUtils

import sigmastate.{Values, SType}
import sigmastate.Values.{EvaluatedValue, ErgoTree}
import sigmastate.serialization.ErgoTreeSerializer
import sigmastate.interpreter.ContextExtension
import sigmastate.utxo.Deserialize

import special.collection.Coll
import special.sigma.SigmaProp

import java.{util => ju}
import scala.util.{Try, Success, Failure}

/**
  * Object that defines the commands availble for the CLI interface and for interacting with the Ergo blockchain
  */
object PaydayAppCommands {

    /**
      * Object constains definitions for the Payday CLI commands and arguments
      */
    object PaydayCli {

        // Type representation of the different commands available for the CLI
        case class Payday(protocolFeeAddress: String, onetime: Boolean)
        case class List(protocolFeeAddress: String)

        // Defining proxy_address argument, which is used by different commands
        val protocolFeeAddressArgumentOpts: Opts[String] = {
            Opts.argument[String](metavar = "protocol_fee_address")
        }

        // Payday command
        val paydaySubCommandOpts: Opts[Payday] = {
            Opts.subcommand(name = "payday", help = "Sends protocol fee funds to implementor addresses.") {
                
                // Defining --onetime flag for running swap once instead of continuously
                val onetimeFlagOpts: Opts[Boolean] = {
                    Opts.flag(long = "onetime", help = "Perform a one time payday with boxes at the protocol fee address.").orFalse
                }
                
                (protocolFeeAddressArgumentOpts, onetimeFlagOpts).mapN(Payday)
            }
        }

        // List command
        val listSubCommandOpts: Opts[List] = {
            Opts.subcommand(name = "list", help = "List all eUTXOs at the protocol fee address.") {
                (protocolFeeAddressArgumentOpts).map(List)
            }
        }

        // Logs command => just print logs file???

        // Payday CLI title
        val paydayTitle: String = {
            """|    
               |   ____                 _                ____ _     ___  
               |  |  _ \ __ _ _   _  __| | __ _ _   _   / ___| |   |_ _|
               |  | |_) / _` | | | |/ _` |/ _` | | | | | |   | |    | | 
               |  |  __/ (_| | |_| | (_| | (_| | |_| | | |___| |___ | | 
               |  |_|   \__,_|\__, |\__,_|\__,_|\__, |  \____|_____|___|
               |              |___/             |___/                  
               |
               |""".stripMargin
        }
    }

    /**
      * Object containts the function used to interact with the Ergo blockchain based on the Payday CLI input command and arguments.
      */
    object PaydayInteractions {

        /**
          * Payday transaction.
          *
          * @param ergoClient
          * @param protocolFeeAddress
          * @param unlockedSecretStorage
          * @return Payday transaction ID string.
          */
        def paydayOneTime(ergoClient: ErgoClient, protocolFeeAddress: String, unlockedSecretStorage: SecretStorage): String = {
            
            // Generate blockchain context
            val paydayTxId: String = ergoClient.execute((ctx: BlockchainContext) => {
                
                // Convert protocol fee contract P2S address to an Address
                val protocolFeeContractAddress: Address = Address.create(protocolFeeAddress)

                // Search for all the protocol fee boxes
                val protocolFeeBoxes:   List[InputBox]      =   ctx.getUnspentBoxesFor(protocolFeeContractAddress, 0, 100).asScala.toList
                val totalFees:          Long                =   protocolFeeBoxes.foldLeft(0L)((acc, feebox) => acc + feebox.getValue())
                val inputs:             ju.List[InputBox]   =   seqAsJavaList(protocolFeeBoxes)
                val GuapSwapMinerFee:   Long                =   PaydayUtils.convertMinerFee(PaydayUtils.GUAPSWAP_MINER_FEE_2)
                val splitValue:         Long                =   (1.toLong * (totalFees - GuapSwapMinerFee)) / 3.toLong
                
                
                // User PK address
                val jesperPK: Address = Address.create(PaydayUtils.JESPER_PK)
                val georgePK: Address = Address.create(PaydayUtils.GEORGE_PK)
                val lucaPK: Address = Address.create(PaydayUtils.LUCA_PK)
                val protocolFeeContract: ErgoContract = PaydayUtils.getProtocolFeeErgoContract(ctx, GuapSwapProtocolFeeContract.getScript)

                // Create tx builder
                val txBuilder: UnsignedTransactionBuilder = ctx.newTxBuilder();

                // Create output box for Jesper
                val jesperBox: OutBox = txBuilder.outBoxBuilder()
                    .value(splitValue)
                    .contract(ctx.newContract(jesperPK.asP2PK().script))
                    .build();

                // Create output box for George
                val georgeBox: OutBox = txBuilder.outBoxBuilder()
                    .value(splitValue)
                    .contract(ctx.newContract(georgePK.asP2PK().script))
                    .build();

                // Create output box for Luca
                val lucaBox: OutBox = txBuilder.outBoxBuilder()
                    .value(splitValue)
                    .contract(ctx.newContract(lucaPK.asP2PK().script))
                    .build();

                // Create prover
                val prover: ErgoProver = ctx.newProverBuilder()
                    .withSecretStorage(unlockedSecretStorage)                   
                    .build();

                // Create unsigned transaction
                val unsignedPaydayTx: UnsignedTransaction = txBuilder.boxesToSpend(inputs)
                    .outputs(jesperBox, georgeBox, lucaBox)
                    .fee(GuapSwapMinerFee)
                    .sendChangeTo(protocolFeeContractAddress.getErgoAddress())
                    .build();
                
                // Sign transaction
                val signedPaydayTx: SignedTransaction = prover.sign(unsignedPaydayTx)
                val paydayTxId: String = ctx.sendTransaction(signedPaydayTx)
                paydayTxId

            })

            // Remove quotation marks from string.
            paydayTxId.replaceAll("\"", "")
        }

        /**
          * Launch the automatic swap
          *
          * @param ergoClient
          * @param parameters
          * @param protocolFeeAddress
          * @param unlockedSecretStorage
          */
        def paydayAutomatic(ergoClient: ErgoClient, parameters: PaydayParameters, protocolFeeAddress: String, unlockedSecretStorage: SecretStorage): Unit = {
            
            // Print notification statement
            println(Console.BLUE + "========== Program will run INDEFINITELY and will NOT ask for confirmation to SIGN the TX. To TERMINATE execution, close the terminal session. ==========" + Console.RESET)

            // Convert swap interval into milliseconds
            val milliSecondsPerMinute: Long = 1000 * 60
            val minutesPerHour: Long = 60
            var minutes: Long = minutesPerHour * parameters.paydayProtocolSettings.paydayIntervalInHours
            
            // Set the default swap time interval to be 60 minutes, this gives enough time for block confirmation
            if (minutes < 60) {
                minutes = 60
            }
            
            // Calculate the time based on user config settings
            val time: Long = milliSecondsPerMinute * minutes
            //val test: Long = 15000

            while (true) {

                try {
                    // Print guapswap automatic initiated status message
                    println(Console.YELLOW + "========== Payday AUTOMATIC TX INITIATED ==========" + Console.RESET)
                    val automaticPaydayTxId: String = paydayOneTime(ergoClient, protocolFeeAddress, unlockedSecretStorage)

                    // Perform a swap
                    println(Console.GREEN + "========== Payday AUTOMATIC TX SUCCESSFULL ==========" + Console.RESET)

                    // Print out guapswap save tx status message
                    println(Console.GREEN + "========== Payday AUTOMATIC TX SAVED ==========" + Console.RESET)
                    PaydayUtils.save(automaticPaydayTxId, PaydayUtils.PAYDAY_FILE_PATH)
                            
                    // Print tx link to the user
                    println(Console.BLUE + "========== VIEW Payday AUTOMATIC TX IN THE ERGO-EXPLORER WITH THE LINK BELOW ==========" + Console.RESET)
                    println(PaydayUtils.ERGO_EXPLORER_TX_URL_PREFIX + automaticPaydayTxId)

                } catch {
                    case noNodeConnect: ErgoClientException => noNodeConnect
                    case noProtocolFeeBoxes: IllegalArgumentException => println(Console.RED + "========== NO VALID PROTOCOL FEE BOXES FOUND FOR THE AUTOMATIC Payday TX ==========" + Console.RESET)
                    case error: Throwable => error
                }
            
                // Print warning and put the thread to sleep for the alloted interval of time
                println(Console.BLUE + s"========== AUTOMATIC TX ATTEMPT WILL OCCUR AGAIN WITHIN THE NEXT ${minutes} MINUTES ==========" + Console.RESET)
                Thread.sleep(time)
            }   

        }

        /**
          * List boxes at the given protocol fee address. 
          *
          * @param ergoClient
          * @param protocolFeeAddress
          */
        def paydayList(ergoClient: ErgoClient, protocolFeeAddress: String): Unit = {

            // Generate the blockchain context
            ergoClient.execute((ctx: BlockchainContext) => {
                
                // Convert proxy contract P2S address to an Address
                val protocolFeeContractAddress: Address = Address.create(protocolFeeAddress)

                try {

                    // Search for all the proxy boxes => assumes a max payout of 1/hr for one week (i.e. 168 maxmimum boxes)
                    val protocolFeeBoxes: List[InputBox] = ctx.getUnspentBoxesFor(protocolFeeContractAddress, 0, 168).asScala.toList
                
                    // Print the proxy boxes
                    protocolFeeBoxes.foreach(protocolFeeBox => println(protocolFeeBox.toJson(true)))

                } catch {
                    case noNodeConnect: ErgoClientException => noNodeConnect 
                    case noProtocolFeeBoxes: IndexOutOfBoundsException =>  println(Console.RED + "========== NO PROTOCOL FEE BOXES AT THE GIVEN ADDRESS FOUND ==========" + Console.RESET)
                    case error: Throwable => error
                }
            
            })

        }

    }

}