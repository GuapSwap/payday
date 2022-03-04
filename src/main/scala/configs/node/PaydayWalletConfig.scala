package configs.node

/**
  * Class to represent the wallet configuration.
  *
  * @param mnemonic
  * @param password
  * @param mnemonicPassword
  */
case class PaydayWalletConfig(
    val mnemonic: String,
    val password: String,
    val mnemonicPassword: String
)