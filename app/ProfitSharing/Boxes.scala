package ProfitSharing

import helpers.{Configs, Utils, connectionException, internalException}
import models.{Distribution, Ticket}
import network.{Client, Explorer}
import org.ergoplatform.ErgoAddress
import org.ergoplatform.appkit.impl.ErgoTreeContract
import org.ergoplatform.appkit.{BlockchainContext, ErgoId, ErgoToken, ErgoValue, InputBox, OutBox, UnsignedTransactionBuilder}
import play.api.Logger
import play.api.libs.json._

import scala.collection.JavaConverters._
import javax.inject.{Inject, Singleton}

@Singleton
class Boxes@Inject()(client: Client, contracts: Contracts, explorer: Explorer) {
  private val logger: Logger = Logger(this.getClass)

  /**
   * @return Some list of income boxes ready to merge
   *         each list contains boxes related to one token type or ERG
   */
  def getIncomes: List[List[InputBox]] ={
    val boxes = client.getAllUnspentBox(contracts.incomeAddress)
    var result: List[List[InputBox]] = List()
    val ergIncomes = boxes.filter(_.getTokens.size() == 0)
    if(ergIncomes.size >= Configs.incomeMerge.min) result = result :+ ergIncomes.take(Configs.incomeMerge.min)

    val tokenIncomes = boxes.filter(_.getTokens.size() > 0)
    val tokens = tokenIncomes.map(_.getTokens.get(0).getId).distinct
    for(token <- tokens){
      val tokenIncome = tokenIncomes.filter(_.getTokens.get(0).getId == token)
      if(tokenIncome.size >= Configs.incomeMerge.min) result = result :+ tokenIncome
    }
    result
  }

  /**
   * @return initial config box
   */
  def createConfig(txB: UnsignedTransactionBuilder): OutBox ={
    txB.outBoxBuilder()
      .value(Configs.fee*2)
      .contract(contracts.config)
      .tokens(new ErgoToken(Configs.token.configNFT, 1),
        new ErgoToken(Configs.token.distribution, Configs.initializer.distributionCount),
        new ErgoToken(Configs.token.locking, Configs.initializer.lockingCount))
      .registers(Utils.longListToErgoValue(Array(1, Configs.initializer.minErgShare, Configs.initializer.minTokenShare, 0, 0, Configs.fee, Configs.initializer.minTicketValue, Configs.minBoxErg)))
      .build()
  }

  /**
   * Finds the last version of a self-replicating box in the network
   * @param address The required box address
   * @param box first unspent box
   * @return The last box related to this self-replicating box considering the mempool
   */
  def findLastMempoolBoxFor(address: String, box: InputBox, ctx: BlockchainContext): InputBox = try {
    val mempool = explorer.getUnconfirmedTxByAddress(address)
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
      val tmpTx = Utils.JsonToTransaction(txJson, ctx)
      outBox = tmpTx.getOutputsToSpend.asScala.filter(box => Configs.addressEncoder.fromProposition(box.getErgoTree).toString == address).head
    }
    outBox
  } catch {
    case e: connectionException => throw e
    case e: JsResultException =>
      logger.error(e.getMessage)
      throw internalException()
    case e: Throwable =>
      logger.error(Utils.getStackTraceStr(e))
      throw internalException()
  }

  /**
   * @param box the id of the box that is going to be checked
   * @return True if the specified box is in mempool, and False otherwise
   */
  def isBoxInMemPool(box: InputBox) : Boolean = {
    try {
      val address = Utils.getAddress(box.getErgoTree.bytes)
      val transactions = Json.parse(explorer.getTxsInMempoolByAddress(address.toString).toString())
      if (transactions != null) {
        (transactions \ "items").as[List[JsValue]].exists(tx =>{
          if((tx \ "inputs").as[JsValue].toString().contains(box.getId.toString)) true
          else false
        })
      } else {
        false
      }
    } catch {
      case e: JsResultException =>
        logger.error(e.getMessage)
        throw internalException()
      case e: Throwable =>
        logger.error(Utils.getStackTraceStr(e))
        throw internalException()
    }
  }

  /**
   * @return last config box in the network (considering the mempool)
   */
  def findConfig(ctx: BlockchainContext): InputBox ={
    val configJson = (explorer.getUnspentTokenBoxes(Configs.token.configNFT, 0, 10) \ "items").as[List[JsValue]].head
    val configBox = ctx.getBoxesById((configJson \ "boxId").as[String]).head
    findLastMempoolBoxFor(contracts.configAddress.toString, configBox, ctx)
  }

  /**
   * @return the list of all distribution boxes in network, sorted by its checkpoint
   */
  def findDistributions(): List[InputBox] ={
    client.getAllUnspentBox(contracts.distributionAddress)
      .filter(box => box.getTokens.size() > 0 &&
        box.getTokens.get(0).getId.toString == Configs.token.distribution  &&
        !isBoxInMemPool(box))
      .sortBy(Distribution(_).checkpoint)
  }

  /**
   * @param checkpoint ticket checkpoint
   * @return the list of all ticket boxes with the required checkpoint
   */
  def findTickets(checkpoint: Long): List[InputBox] ={
    client.getAllUnspentBox(contracts.ticketAddress)
      .filter(box => box.getTokens.size() > 0 &&
        box.getTokens.get(0).getId.toString == Configs.token.locking &&
        !isBoxInMemPool(box) &&
        Ticket(box).checkpoint == checkpoint)
  }

  /**
   * @return The new config box created with new setting
   */
  def getConfig(txB: UnsignedTransactionBuilder, value: Long, distCount: Long, lockingCount: Long, r4: Array[Long]): OutBox ={
    txB.outBoxBuilder()
      .value(value)
      .contract(contracts.config)
      .tokens(new ErgoToken(Configs.token.configNFT, 1),
        new ErgoToken(Configs.token.distribution, distCount),
        new ErgoToken(Configs.token.locking, lockingCount))
      .registers(Utils.longListToErgoValue(r4))
      .build()
  }

  /**
   * @return The newly created ticket with locking staking tokens
   */
  def getTicket(txB: UnsignedTransactionBuilder, value: Long, stake: Long, address: ErgoAddress,  r4: Array[Long] ,reservedTokenId: ErgoId): OutBox ={
    txB.outBoxBuilder()
      .value(value)
      .contract(contracts.ticket)
      .tokens(new ErgoToken(Configs.token.locking, 1), new ErgoToken(Configs.token.staking, stake))
      .registers(Utils.longListToErgoValue(r4),
        ErgoValue.of(new ErgoTreeContract(address.script).getErgoTree.bytes),
        ErgoValue.of(reservedTokenId.getBytes))
      .build()
  }

  /**
   * @return The new created income with aggregated or remainder income
   */
  def getIncome(txB: UnsignedTransactionBuilder, value: Long, tokenCount: Long = 0, tokenId: String = ""): OutBox ={
    val box = txB.outBoxBuilder()
      .value(value)
      .contract(contracts.income)
    if (tokenCount > 0) box.tokens(new ErgoToken(tokenId, tokenCount))
    box.build()
  }

  /**
   * @return The newly created distribution out of income
   */
  def getDistribution(txB: UnsignedTransactionBuilder, value: Long, checkpoint: Long, fee: Long, ticketCount: Long,
                      ergShare: Long, tokenShare: Long = 0, tokenCount: Long = 0, tokenId: String = ""): OutBox ={
    var box = txB.outBoxBuilder()
      .value(value)
      .contract(contracts.distribution)
      .registers(Utils.longListToErgoValue(Array(checkpoint, ergShare, tokenShare, fee)), ErgoValue.of(ticketCount))
    if(tokenCount== 0) box = box.tokens(new ErgoToken(Configs.token.distribution, 1))
    else box = box.tokens(new ErgoToken(Configs.token.distribution, 1),
      new ErgoToken(tokenId, tokenCount))
    box.build()
  }
}
