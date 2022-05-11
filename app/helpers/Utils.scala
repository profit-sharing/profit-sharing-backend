package helpers

import java.io.{PrintWriter, StringWriter}

import play.api.Logger
import play.api.libs.json._
import org.ergoplatform.appkit.{Address, BlockchainContext, ErgoContract, ErgoType, ErgoValue, JavaHelpers, SignedTransaction}
import special.collection.Coll
import org.ergoplatform.ErgoAddress
import sigmastate.serialization.ErgoTreeSerializer
import scorex.crypto.hash.Digest32


final case class failedTxException(private val message: String = "Tx sending failed") extends Throwable(message)
final case class requestException(private val message: String = "Explorer error") extends Throwable(message)
final case class connectionException(private val message: String = "Network Error") extends Throwable(message)
final case class skipException(private val message: String = "skip") extends Throwable(message)
final case class proveException(private val message: String = "Tx proving failed") extends Throwable(message)
final case class internalException(private val message: String = "something went wrong") extends Throwable(message)
final case class notCoveredException(private val message: String = "funds are not enough for the distribution") extends Throwable(message)


object Utils {
  private val logger: Logger = Logger(this.getClass)

  def getStackTraceStr(e: Throwable): String = {
    val sw = new StringWriter
    val pw = new PrintWriter(sw)
    e.printStackTrace(pw)
    sw.toString
  }

  def getAddress(addressBytes: Array[Byte]): ErgoAddress = {
    val ergoTree = ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(addressBytes)
    Configs.addressEncoder.fromProposition(ergoTree).get
  }

  def generateAddress(contract: ErgoContract): Address ={
    Address.create(Configs.addressEncoder.fromProposition(contract.getErgoTree).get.toString)
  }

  def longListToErgoValue(elements: Array[Long]): ErgoValue[Coll[Long]] = {
    val longColl = JavaHelpers.SigmaDsl.Colls.fromArray(elements)
    ErgoValue.of(longColl, ErgoType.longType())
  }

  def getContractScriptHash(contract: ErgoContract): Digest32 = {
    scorex.crypto.hash.Blake2b256(contract.getErgoTree.bytes)
  }

  def JsonToTransaction(txJson: JsValue, ctx: BlockchainContext): SignedTransaction ={
    val inputs = (txJson \ "inputs").as[JsValue].toString().replaceAll("id", "boxId")
    val outputs = (txJson \ "outputs").as[JsValue].toString().replaceAll("id", "boxId").replaceAll("txId", "transactionId")
    val dataInputs = (txJson\ "dataInputs").as[JsValue].toString()
    val id = (txJson \ "id").as[String]
    val newJson = s"""{
          "id" : "$id",
          "inputs" : $inputs,
          "dataInputs" : $dataInputs,
          "outputs" : $outputs
          }"""
    ctx.signedTxFromJson(newJson.replaceAll("null", "\"\""))
  }
}
