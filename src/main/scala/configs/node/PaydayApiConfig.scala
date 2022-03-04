package configs.node

import protocol.PaydayUtils

/**
  * Class representing the node Api.
  *
  * @param apiUrl
  * @param apiKey
  */
case class PaydayApiConfig(
    val apiUrl: String = PaydayUtils.DEFAULT_ERGOPLATFORM_MAINNET_API_URL,
    val apiKey: String
)

