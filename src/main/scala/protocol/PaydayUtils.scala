package protocol

import scala.collection.immutable.HashMap
import scala.util.{Try, Success, Failure}
import java.io.{File, FileNotFoundException}

import org.ergoplatform.ErgoAddress
import org.ergoplatform.appkit._

import contracts.{GuapSwapProtocolFeeContract}

import special.sigma.SigmaProp
import special.collection.Coll
import sigmastate.{Values}
import java.nio.file.Files
import java.io.FileWriter
import org.bouncycastle.util.encoders.UTF8
import java.nio.charset.Charset
import org.apache.commons.io.input.CharSequenceReader
import java.util.Date
import java.time.LocalDateTime
import java.time.ZoneId

/**
  * Object representing constants and methods relevant to Payday.
  */
object PaydayUtils {

  // Constant representing the storage location within the project repository of the Payday_config.json file and Payday_proxy.json file
  final val PAYDAY_CONFIG_FILE_PATH:  String = "storage/payday_config.json"
  final val PAYDAY_FILE_PATH:   String = "storage/payday.log"

  // Ergo Explorer URL
  final val ERGO_EXPLORER_TX_URL_PREFIX: String = "https://explorer.ergoplatform.com/en/transactions/"

  // Default public node URL, from Ergo Platform
  final val DEFAULT_ERGOPLATFORM_MAINNET_API_URL: String = "http://213.239.193.208:9053/"
  final val DEFAULT_ERGOPLATFORM_TESTNET_API_URL: String = "http://213.239.193.208:9052/"

  // Default GetBlok TESTNET node URL
  final val DEFAULT_GETBLOK_TESTNET_STRATUM_URL: String = "http://ergo-testnet.getblok.io:3056"
  final val DEFAULT_GETBLOK_TESTNET_API_URL: String = "http://ergo-node-devnet.getblok.io:9052/"
  
  // Default secret storage directory
  final val DEFAULT_SECRET_STORAGE_DIRECTORY: String = "storage/.secretStorage"
  final val DEFAULT_PROTOCOL_MINER_FEE:       Double = 0.002D

  // Test address
  final val MAINNET_TEST_ADDRESS: String = "9g462nhKH6kzQetBobihiD9x225SZcVz2cbJabgUzXRHNmTRb52"

  // Different protocol fee mining fees
  final val GUAPSWAP_MINER_FEE_2: Double = 0.002
  final val GUAPSWAP_MINER_FEE_5: Double = 0.005

  // Founder PKs
  final val JESPER_PK:  String = "9hy9jt1Vuq3fZr4rSYAUqo1r2dAJBBdazV6cL8FNuBQEvM6wXfR"
  final val GEORGE_PK:  String = "9hA5gTKrx1YsTDjYiSnYqsAWawMq1GbvaemobybpCZ8qyHFBXKF"
  final val LUCA_PK:    String = "9ej8AEGCpNxPaqfgisJTU2RmYG91bWfK1hu2xT34i5Xdw4czidX"

  /**
    * Convert from ERGs to nanoERGs
    *
    * @param erg
    * @return Converted ERG value in nanoErgs.
    */
  def ergToNanoErg(erg: Double): Long = {
    val fraction: (Long, Long) = decimalToFraction(erg)
    val nanoergs: Long = (fraction._1 * Parameters.OneErg) / fraction._2
    nanoergs
  }

  /**
    * Calculate the miner fee in nanoERGs
    *
    * @param minerFee
    * @return Miner fee in nanoERGs.
    */
  def convertMinerFee(minerFee: Double): Long = {
    val minerFeeNanoErgs = ergToNanoErg(minerFee)
    
    // Force miner fee to be > minimum box value required by Ergo blockchain.
    if (minerFeeNanoErgs < Parameters.MinFee) {
        ergToNanoErg(DEFAULT_PROTOCOL_MINER_FEE)
    } else {
        minerFeeNanoErgs
    }
  }
  
  /**
    * Method to convert a decimal number to a rational fraction.
    *
    * @param number
    * @return Tuple of the numerator and denominator representing the decimal number.
    */
  def decimalToFraction(number: Double): (Long, Long) = {
    
    // Ignore if zero
    if (number == 0.0) {
      (0.toLong, 1.toLong)
    } else {
      // Format the number correctly for calculation such that there are no trailing zeros. 
      val bigdecimalNumber: BigDecimal = BigDecimal.apply(number).underlying().stripTrailingZeros()
      val bigdecimalToDouble: Double = bigdecimalNumber.doubleValue()
      val listMatch: List[String] = bigdecimalToDouble.toString.split("\\.").toList
      
      // Get the fractional representation of the decimal number
      val fractionTuple: (Long, Long) = listMatch match {
        case List(whole, fractional) => {
          val numDecimals: Double = fractional.length().toDouble
          val denominator: Long = Math.pow(10D, numDecimals).toLong
          val numerator: Long = whole.toLong * denominator + fractional.toLong
          (numerator, denominator)
        }
      }

      fractionTuple
    }
  }

  /**
    * Load secret storage
    * 
    * @return A Try[SecretStorage] statement containing the secret storage or the exception to handle.
    */
  def loadSecretStorage(): Try[SecretStorage] = Try {
   
    println(Console.YELLOW + "========== LOADING SECRET STORAGE ==========" + Console.RESET)
  
    val secretDirectory: File = new File(DEFAULT_SECRET_STORAGE_DIRECTORY)
    
    // Check if directory exists
    if (secretDirectory.isDirectory()) {

      // List files in the directory
      val files: Array[File] = secretDirectory.listFiles()

      // Check if there are files that exist in the directory
      if (files.length == 0) {
        throw new FileNotFoundException
      } else {
        SecretStorage.loadFrom(files(0))
      }

    } else {
      throw new FileNotFoundException
    }
    
  }

  /**
    * Generate secret storage
    *
    * @return The generated secret storage
    */
  def generateSecretStorage(): SecretStorage = {
    println(Console.YELLOW + "========== GENERATING SECRET STORAGE ==========" + Console.RESET)
    println(Console.YELLOW + "========== PLEASE CREATE A PASSWORD FOR SECRET STORAGE ==========" + Console.RESET)
    val password: String = System.console().readPassword().mkString
    
    val mnemonicPhrase: Array[Char] = Mnemonic.generateEnglishMnemonic().toCharArray()
    val mnemonic: Mnemonic = Mnemonic.create(SecretString.create(mnemonicPhrase), SecretString.empty())
    val generatedSecretStorage: SecretStorage = SecretStorage.createFromMnemonicIn(DEFAULT_SECRET_STORAGE_DIRECTORY, mnemonic, password)
    
    println(Console.GREEN + "========== SECRET STORAGE CREATED ==========" + Console.RESET)
    println(Console.BLUE + "========== YOUR MNEMONIC AND SECRET STORAGE ARE THE FOLLOWING ==========" + Console.RESET)
    println("Mnemonic Phrase: " + mnemonic.getPhrase().toStringUnsecure())
    println("Secret Storage Directory: " + generatedSecretStorage.getFile().getPath())
    
    generatedSecretStorage
  
  }

  /**
    * Unlock the locked secret storage.
    *
    * @param lockedSecretStorage
    * @return Unlocked secret storage.
    */
  def unlockSecretStorage(lockedSecretStorage: SecretStorage): Try[SecretStorage] = Try {
    println(Console.YELLOW + "===== PLEASE ENTER YOUR ENCRYPTION PASSWORD TO UNLOCK SECRET STORAGE" + Console.RESET)
    val passPhrase: String = System.console().readPassword().mkString
    
    // Unlock secret storage
    println(Console.YELLOW + "========== UNLOCKING SECRET STORAGE ==========")
    try {
        lockedSecretStorage.unlock(passPhrase)
    } catch {
        case exception: RuntimeException => {
            println(Console.RED + "========== WRONG PASSWORD ==========" + Console.RESET)
            throw exception
        }                   
    }
    println(Console.GREEN + "========== SECRET STORAGE UNLOCKED ==========")
    lockedSecretStorage
  }

  /**
    * Check secret storage
    *
    * @return The loaded or generated secret storage
    */
  def checkSecretStorage(): SecretStorage = {
    
    // Check and load secret storage
    val checkSecretStorage: SecretStorage = PaydayUtils.loadSecretStorage() match {
        
      case Success(loadedSecretStorage) => {
        println(Console.GREEN + "========== SECRET STORAGE EXISTS ==========" + Console.RESET)
        loadedSecretStorage
      }

      case Failure(exception) => {
          println(Console.RED + "========== SECRET STORAGE DOES NOT EXIST ==========" + Console.RESET)
          
          // Generate secret storage
          val generatedSecretStorage: SecretStorage = generateSecretStorage()
          generatedSecretStorage
      }
    }

    checkSecretStorage
  }

  /**
    * Get the compiled ErgoContract of the protocol fee address.
    *
    * @param ctx
    * @param parameters
    * @return Compiled ErgoContract of protocol fee contract.
    */
  def getProtocolFeeErgoContract(ctx: BlockchainContext, protocolFeeContractScript: String): ErgoContract = {
    
    // Protocol fee contract hard-coded constants  
    val jesperPK:                       ErgoValue[SigmaProp]    =   ErgoValue.of(Address.create(PaydayUtils.JESPER_PK).getPublicKey())
    val georgePK:                       ErgoValue[SigmaProp]    =   ErgoValue.of(Address.create(PaydayUtils.GEORGE_PK).getPublicKey())
    val lucaPK:                         ErgoValue[SigmaProp]    =   ErgoValue.of(Address.create(PaydayUtils.LUCA_PK).getPublicKey())

    // Protocol fee contract compiled ErgoContract
    val protocolFeeErgoContract: ErgoContract = ctx.compileContract(
        ConstantsBuilder.create()
            .item("JesperPK", jesperPK.getValue())
            .item("GeorgePK", georgePK.getValue())
            .item("LucaPK", lucaPK.getValue())
            .build(),
            protocolFeeContractScript
    )
    protocolFeeErgoContract
  }

  /**
    * Save string to file log, with date and time in UTC.
    *
    * @param string
    * @param path
    */
  def save(string: String, path: String): Unit = {

    // Get access to the file
    val file: FileWriter = new FileWriter(path, true)

    // Get the date and time in UTC format
    val dateTime: LocalDateTime = LocalDateTime.now(ZoneId.of("UTC"))

    // Format the time string 
    val date: String = dateTime.toString().split("[T]")(0)
    val time: String = dateTime.toString().split("[T]")(1).split("\\.")(0)

    // Append text to file
    file.append(System.lineSeparator())
    file.append(s"[UTC ${date} ${time}] ${string}")

    // Close the file and io-stream
    file.close()  
  }
    
}
