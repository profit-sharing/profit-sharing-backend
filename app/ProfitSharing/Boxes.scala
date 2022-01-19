package ProfitSharing

import helpers.{Configs, Utils}
import network.Client
import org.ergoplatform.appkit.{ErgoToken, InputBox, OutBox, UnsignedTransactionBuilder}

import javax.inject.{Inject, Singleton}

@Singleton
class Boxes@Inject()(client: Client, utils: Utils, contracts: Contracts) {

  def getIncomes: List[List[InputBox]] ={
    val boxes = client.getAllUnspentBox(contracts.incomeAddress)
    var result: List[List[InputBox]] = List()
    val ergIncomes = boxes.filter(_.getTokens.size() == 0)
    if(ergIncomes.size >= Configs.incomeMerge.min) result = result :+ ergIncomes.take(Configs.incomeMerge.min)
    else{
      val tokens = ergIncomes.filter(_.getTokens.size() > 0).map(_.getTokens.get(0).getId).distinct
      for(token <- tokens){
        val tokenIncomes = boxes.filter(_.getTokens.get(0).getId == token)
        if(tokenIncomes.size >= Configs.incomeMerge.min) result = result :+ tokenIncomes
      }
    }
    result
  }

  def createConfig(txB: UnsignedTransactionBuilder, configNFT: String, distributionToken: String, lockingToken: String): OutBox ={
    txB.outBoxBuilder()
      .value(Configs.fee*2)
      .contract(contracts.config)
      .tokens(new ErgoToken(configNFT, 1),
        new ErgoToken(distributionToken, Configs.initializer.distributionCount),
        new ErgoToken(lockingToken, Configs.initializer.lockingCount))
      .registers(utils.longListToErgoValue(Array(1, 1e9.toLong, 10, 0, 0, Configs.fee, 1e9.toLong, Configs.minBoxErg)))
      .build()
  }
}
