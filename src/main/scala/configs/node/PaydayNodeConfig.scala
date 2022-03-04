package configs.node

import org.ergoplatform.appkit.NetworkType

/**
  * Class representing the node configuration.
  *
  * @param nodeApi
  * @param wallet
  * @param networkType
  */
case class PaydayNodeConfig(
    val nodeApi: PaydayApiConfig,
    val wallet: PaydayWalletConfig,
    val networkType: NetworkType
)
