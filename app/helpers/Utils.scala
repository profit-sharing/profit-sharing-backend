package helpers

import java.io.{PrintWriter, StringWriter}
import javax.inject.{Inject, Singleton}
import io.circe.{Json => ciJson}

import java.util.Calendar
import play.api.Logger
import play.api.libs.json._

import scala.collection.JavaConverters._
import network.{Client, Explorer}
import org.ergoplatform.appkit.{Address, BlockchainContext, ErgoClientException, ErgoContract, ErgoType, ErgoValue, InputBox, JavaHelpers, SignedTransaction}
import special.collection.Coll
import org.ergoplatform.ErgoAddress
import sigmastate.serialization.ErgoTreeSerializer
import scorex.crypto.hash.Digest32


final case class failedTxException(private val message: String = "Tx sending failed") extends Throwable(message)
final case class requestException(private val message: String = "Explorer error") extends Throwable(message)
final case class connectionException(private val message: String = "Network Error") extends Throwable(message)
final case class parseException(private val message: String = "Parsing failed") extends Throwable(message)
final case class skipException(private val message: String = "skip") extends Throwable(message)
final case class proveException(private val message: String = "Tx proving failed") extends Throwable(message)
final case class internalException(private val message: String = "something went wrong") extends Throwable(message)


@Singleton
class Utils @Inject()(client: Client, explorer: Explorer) {
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
    val newJson = s"""{
          "id" : "${id}",
          "inputs" : ${inputs},
          "dataInputs" : ${dataInputs},
          "outputs" : ${outputs}
          }"""
    ctx.signedTxFromJson(newJson.replaceAll("null", "\"\""))
  }

  /**
   * creates a box for the specified address and amount
   * @return input box List, is covered, covered amount
   */
  def getCoveringBoxesWithMempool(paymentAddress: String, amount: Long): (List[InputBox], Boolean, Long) = try{
    val cover = client.getCoveringBoxesFor(Address.create(paymentAddress), amount)
    if(cover.isCovered) (cover.getBoxes.asScala.toList, true, cover.getCoveredAmount)
    else {
      var boxes: List[InputBox] = cover.getBoxes.asScala.toList
      val mempool = Json.parse(explorer.getUnconfirmedTxByAddress(paymentAddress).toString())
      val txs = (mempool \ "items").as[List[JsValue]]
      var totalValue: Long = cover.getCoveredAmount
      txs.foreach(txJson => {
        client.getClient.execute(ctx => {
          val tx = JsonToTransaction(txJson, ctx)
          val selectedBoxes: List[InputBox] = tx.getOutputsToSpend.asScala.toList
            .filter(box => Configs.addressEncoder.fromProposition(box.getErgoTree).get == Address.create(paymentAddress).getErgoAddress)
          if(totalValue < amount) {
            boxes = boxes ++ selectedBoxes
            totalValue = totalValue + selectedBoxes.map(_.getValue).reduce((x,y)=> x + y)
          }
        })
      })
      (boxes, totalValue >= amount, totalValue)
    }
  } catch{
    case e: connectionException => throw e
    case e: JsResultException =>
      logger.error(e.getMessage)
      throw internalException()
    case e: Throwable =>
      logger.error(getStackTraceStr(e))
      throw internalException()
  }

  def currentTime: Long = Calendar.getInstance().getTimeInMillis / 1000

  def getTransactionFrontLink(txId: String): String = Configs.explorerFront + "/en/transactions/" + txId
}
