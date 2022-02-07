package helpers

import java.io.{PrintWriter, StringWriter}
import javax.inject.{Inject, Singleton}
import io.circe.{Json => ciJson}
import network.Explorer

import play.api.Logger
import play.api.libs.json._
import org.ergoplatform.appkit.{Address, BlockchainContext, ErgoContract, ErgoType, ErgoValue, InputBox, JavaHelpers, SignedTransaction}
import special.collection.Coll
import org.ergoplatform.ErgoAddress
import sigmastate.serialization.ErgoTreeSerializer
import scorex.crypto.hash.Digest32
import collection.JavaConverters._


final case class failedTxException(private val message: String = "Tx sending failed") extends Throwable(message)
final case class requestException(private val message: String = "Explorer error") extends Throwable(message)
final case class connectionException(private val message: String = "Network Error") extends Throwable(message)
final case class skipException(private val message: String = "skip") extends Throwable(message)
final case class proveException(private val message: String = "Tx proving failed") extends Throwable(message)
final case class internalException(private val message: String = "something went wrong") extends Throwable(message)


@Singleton
class Utils @Inject()(explorer: Explorer) {
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

  def getContractAddress(contract: ErgoContract): String = {
    val ergoTree = contract.getErgoTree
    Configs.addressEncoder.fromProposition(ergoTree).get.toString
  }

  def JsonToTransaction(txJson: JsValue, ctx: BlockchainContext): SignedTransaction ={
    val inputs = (txJson \ "inputs").as[JsValue].toString().replaceAll("id", "boxId")
    val outputs = (txJson \ "outputs").as[JsValue].toString().replaceAll("id", "boxId").replaceAll("txId", "transactionId")
    val dataInputs = (txJson\ "dataInputs").as[JsValue].toString()
    val id = (txJson \ "id").as[String]
    val newJson = ciJson.fromFields(List(
      ("id", ciJson.fromString(id)),
      ("inputs", ciJson.fromString(inputs)),
      ("dataInputs", ciJson.fromString(dataInputs)),
      ("outputs", ciJson.fromString(outputs))
    )).toString()
    ctx.signedTxFromJson(newJson.replaceAll("null", "\"\""))
  }

  /**
   * Finds the last version of a self-replicating box in the network
   * @param address The required box address
   * @param box first unspent box
   * @return The last box related to this self-replicating box considering the mempool
   */
  def findLastMempoolBoxFor(address: String, box: InputBox, ctx: BlockchainContext): InputBox = try {
    val mempool = Json.parse(explorer.getUnconfirmedTxByAddress(address).toString())
    var outBox = box
    val txs = (mempool \ "items").as[List[JsValue]]
    var txMap: Map[String, JsValue] = Map()
    txs.foreach(txJson => {
      val id = ((txJson \ "inputs").as[List[JsValue]].head \ "id").as[String]
      txMap += (id -> txJson)
    })
    val keys = txMap.keys.toSeq
    while (keys.contains(outBox.getId.toString)) {
      val txJson = txMap(outBox.getId.toString)
      val tmpTx = JsonToTransaction(txJson, ctx)
      outBox = tmpTx.getOutputsToSpend.asScala.filter(box => Configs.addressEncoder.fromProposition(box.getErgoTree).toString == address).head
    }
    outBox
  } catch {
    case e: connectionException => throw e
    case e: JsResultException =>
      logger.error(e.getMessage)
      throw internalException()
    case e: Throwable =>
      logger.error(getStackTraceStr(e))
      throw internalException()
  }
}
