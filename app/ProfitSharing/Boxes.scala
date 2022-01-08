package ProfitSharing

import helpers.{Configs, Utils}
import network.Client
import org.ergoplatform.appkit.InputBox

import javax.inject.{Inject, Singleton}

@Singleton
class Boxes@Inject()(client: Client, utils: Utils, contracts: Contracts) {

  def getIncomes: List[List[InputBox]] ={
    val boxes = client.getAllUnspentBox(contracts.incomeAddress)
    var result: List[List[InputBox]] = null
    val ergIncomes = boxes.filter(_.getTokens.size() == 0)
    if(ergIncomes.size >= Configs.incomeMerge.min) result = result :+ ergIncomes
    else{
      val tokens = ergIncomes.map(_.getTokens.get(0).getId).distinct
      for(token <- tokens){
        val tokenIncomes = boxes.filter(_.getTokens.get(0).getId == token)
        if(tokenIncomes.size >= Configs.incomeMerge.min) result = result :+ tokenIncomes
      }
    }
    result
  }
}
