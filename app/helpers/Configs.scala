package helpers

import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.appkit.NetworkType

object Configs extends ConfigHelper {
  lazy val nodeUrl: String = readKey("node.url").replaceAll("/$", "")
  lazy val networkType: NetworkType = if (readKey("node.networkType").toLowerCase.equals("mainnet")) NetworkType.MAINNET else NetworkType.TESTNET
  lazy val addressEncoder = new ErgoAddressEncoder(networkType.networkPrefix)
  lazy val explorerUrl: String = readKey("explorer.url").replaceAll("/$", "")
  lazy val explorerFront: String = readKey("explorer.front").replaceAll("/$", "")

  lazy val fee: Long = readKey("fee.default").toLong
  lazy val maxFee: Long = readKey("fee.max", "1000000").toLong
  lazy val minBoxErg: Long = readKey("minBoxErg").toLong
  lazy val infBoxVal: Long = readKey("infBoxVal").toLong

  object token{
    lazy val locking: String = readKey("token.locking")
    lazy val staking: String = readKey("token.staking")
    lazy val distribution: String = readKey("token.distribution")
    lazy val configNFT: String = readKey("token.configNFT")
  }
}
