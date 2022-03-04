import cats.effect._
import cats.implicits._
import com.monovore.decline._
import com.monovore.decline.effect._

import protocol.PaydayUtils
import configs.PaydayConfig
import configs.node.PaydayNodeConfig
import configs.parameters.PaydayParameters

import app.PaydayAppCommands._
import app.PaydayAppCommands.{PaydayCli, PaydayInteractions}

import scala.util.{Try, Success, Failure}
import org.ergoplatform.appkit.{RestApiErgoClient, ErgoClient, SecretStorage, Mnemonic}

import org.ergoplatform.wallet.secrets.{JsonSecretStorage}
import org.ergoplatform.wallet.settings.{SecretStorageSettings, EncryptionSettings}
import org.ergoplatform.appkit.NetworkType

/**
  * Main object of the Payday CLI application.
  */
object PaydayApp extends CommandIOApp(
    name = "payday",
    header = "Payday CLI to retrieve Payday protocol fee funds.",
    version = "1.0.0-beta"
) {

    override def main: Opts[IO[ExitCode]] = {
    

        // Load configuration settings from Payday_config.json
        val configFilePath: String = PaydayUtils.PAYDAY_CONFIG_FILE_PATH
        val configLoadResult: Try[PaydayConfig] = PaydayConfig.load(configFilePath) 
        
        // Check if config file was loaded properly
        if (configLoadResult.isSuccess) {

            // Print title
            println(Console.RED + PaydayCli.paydayTitle + Console.RESET)

            // Print configuration load status
            println(Console.GREEN + "========== CONFIGURATIONS LOADED SUCCESSFULLY ==========" + Console.RESET)

            // Setup Ergo Clients
            val nodeConfig: PaydayNodeConfig = configLoadResult.get.node
            val parameters: PaydayParameters = configLoadResult.get.parameters
            val explorerURL: String = RestApiErgoClient.getDefaultExplorerUrl(nodeConfig.networkType)
            val ergoClient: ErgoClient = RestApiErgoClient.create(nodeConfig.nodeApi.apiUrl, nodeConfig.networkType, nodeConfig.nodeApi.apiKey, explorerURL)
            
            // Check secret storage
            val secretStorage: SecretStorage = PaydayUtils.checkSecretStorage()

            // Parse commands from the command line
            (PaydayCli.paydaySubCommandOpts orElse PaydayCli.listSubCommandOpts).map {
                
                case PaydayCli.Payday(protocolFeeAddress, onetime) => {

                    // Unlock secret storage
                    val unlockedSecretStorage: SecretStorage = PaydayUtils.unlockSecretStorage(secretStorage) match {
                        case Success(unlockedStorage) => unlockedStorage
                        case Failure(exception) => {
                            println("Please try payday again.")
                            throw exception
                        }
                    }

                    if (onetime) {

                        // Print Payday onetime initiated status message
                        println(Console.YELLOW + "========== Payday ONETIME TX INITIATED ==========" + Console.RESET)
                        val onetimePaydayTxId: String = PaydayInteractions.paydayOneTime(ergoClient, protocolFeeAddress, unlockedSecretStorage)

                        // Print out Payday succeeded status message
                        println(Console.GREEN + "========== Payday ONETIME TX SUCCESSFULL ==========" + Console.RESET)

                        // Print out Payday save tx status message
                        println(Console.GREEN + "========== Payday ONETIME TX SAVED ==========" + Console.RESET)
                        PaydayUtils.save(onetimePaydayTxId, PaydayUtils.PAYDAY_FILE_PATH)
                        
                        // Print tx link to the user
                        println(Console.BLUE + "========== VIEW Payday ONETIME TX IN THE ERGO-EXPLORER WITH THE LINK BELOW ==========" + Console.RESET)
                        println(PaydayUtils.ERGO_EXPLORER_TX_URL_PREFIX + onetimePaydayTxId)
                        
                    } else {
                        
                        // Print Payday initiated status message
                        println(Console.YELLOW + "========== Payday AUTOMATIC MODE STARTED ==========" + Console.RESET)
                        PaydayInteractions.paydayAutomatic(ergoClient, parameters, protocolFeeAddress, unlockedSecretStorage)

                    }
                    
                    // Return successful exit code
                    IO(ExitCode.Success)
                }

                case PaydayCli.List(proxyAddress) => {
                    
                    println(Console.YELLOW + "========== Payday LIST INITIATED ==========" + Console.RESET)
                    println(Console.YELLOW + "========== LISTING ALL Payday BOXES WITH THE GIVEN ADDRESS ==========" + Console.RESET)

                    // List boxes at the proxy addres
                    PaydayInteractions.paydayList(ergoClient, proxyAddress)
                    
                    println(Console.GREEN + "========== LISTING COMPLETE ==========" + Console.RESET)

                    // Return successful exit code
                    IO(ExitCode.Success)
                }

            } 

        } else {

            // Print configuration load status
            println(Console.RED + "========== CONFIGURATIONS LOADED UNSUCCESSFULLY ==========" + Console.RESET)

            // Print Failure exeption
            println(configLoadResult.get)

            // Return error exit code
            Opts(IO(ExitCode.Error))
        }
        
    }
}