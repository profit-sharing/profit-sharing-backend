package helpers

import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.appkit.{Address, NetworkType}

import java.math.BigInteger

object Configs extends ConfigHelper {
  lazy val nodeUrl: String = readKey("node.url").replaceAll("/$", "")
  lazy val networkType: NetworkType = if (readKey("node.networkType").toLowerCase.equals("mainnet")) NetworkType.MAINNET else NetworkType.TESTNET
  lazy val addressEncoder = new ErgoAddressEncoder(networkType.networkPrefix)
  lazy val explorerUrl: String = readKey("explorer.url").replaceAll("/$", "")
  lazy val explorerFront: String = readKey("explorer.front").replaceAll("/$", "")

  lazy val fee: Long = readKey("fee.default").toLong
  lazy val maxFee: Long = readKey("fee.max", "1000000").toLong
  lazy val feePerByte: Long = readKey("fee.perByte").toLong
  lazy val minBoxErg: Long = readKey("minBoxErg").toLong
  lazy val infBoxVal: Long = readKey("infBoxVal").toLong

  object initializer{
    lazy val address: Address =  Address.create(readKey("initializer.address"))
    lazy val secret: BigInteger = BigInt(readKey("initializer.secret"), 16).bigInteger
    lazy val distributionCount: Long = readKey("initializer.distributionCount").toLong
    lazy val lockingCount: Long = readKey("initializer.lockingCount").toLong
  }

  object incomeMerge{
    lazy val min: Int = readKey("incomeMerge.minBox").toInt
    lazy val max: Int = readKey("incomeMerge.maxBox").toInt
    lazy val interval: Int = readKey("incomeMerge.timeInterval").toInt
    lazy val boxSize: Int = readKey("incomeMerge.boxSize").toInt
    lazy val maxFee: Long = feePerByte * max * boxSize
  }

  object token{
    lazy val locking: String = readKey("token.locking")
    lazy val staking: String = readKey("token.staking")
    lazy val distribution: String = readKey("token.distribution")
    lazy val configNFT: String = readKey("token.configNFT")
  }
}
