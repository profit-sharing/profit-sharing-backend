package ProfitSharing

import helpers.{Configs, Utils}
import network.{Client, Explorer}
import org.ergoplatform.ErgoAddress
import org.ergoplatform.appkit.impl.ErgoTreeContract
import org.ergoplatform.appkit.{BlockchainContext, ErgoId, ErgoToken, ErgoValue, InputBox, OutBox, UnsignedTransactionBuilder}
import play.api.libs.json._

import javax.inject.{Inject, Singleton}

@Singleton
class Boxes@Inject()(client: Client, utils: Utils, contracts: Contracts, explorer: Explorer) {

  /**
   * @return Some list of income boxes ready to merge
   *         each list contains boxes related to one token type or ERG
   */
  def getIncomes: List[List[InputBox]] ={
    val boxes = client.getAllUnspentBox(contracts.incomeAddress)
    var result: List[List[InputBox]] = List()
    val ergIncomes = boxes.filter(_.getTokens.size() == 0)
    if(ergIncomes.size >= Configs.incomeMerge.min) result = result :+ ergIncomes.take(Configs.incomeMerge.min)

    val tokens = boxes.filter(_.getTokens.size() > 0).map(_.getTokens.get(0).getId).distinct
    for(token <- tokens){
      val tokenIncomes = boxes.filter(_.getTokens.size() > 0).filter(_.getTokens.get(0).getId == token)
      if(tokenIncomes.size >= Configs.incomeMerge.min) result = result :+ tokenIncomes
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
      .registers(utils.longListToErgoValue(Array(1, Configs.initializer.minErgShare, Configs.initializer.minTokenShare, 0, 0, Configs.fee, Configs.initializer.minTicketValue, Configs.minBoxErg)))
      .build()
  }

  /**
   * @return last config box in the network (considering the mempool)
   */
  def findConfig(ctx: BlockchainContext): InputBox ={
    val configJson = (explorer.getUnspentTokenBoxes(Configs.token.configNFT, 0, 10) \ "items").as[List[JsValue]].head
    val configBox = ctx.getBoxesById((configJson \ "boxId").as[String]).head
    utils.findLastMempoolBoxFor(contracts.configAddress.toString, configBox, ctx)
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
      .registers(utils.longListToErgoValue(r4))
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
      .registers(utils.longListToErgoValue(r4),
        ErgoValue.of(new ErgoTreeContract(address.script).getErgoTree.bytes),
        ErgoValue.of(reservedTokenId.getBytes))
      .build()
  }
}
