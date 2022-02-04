package ProfitSharing


import helpers.{Configs, Utils}
import org.scalatest._
import org.scalatest.propspec._
import network.Client
import org.ergoplatform.appkit.{ErgoToken, InputBox}
import org.mockito.Mockito._
import org.scalatest.matchers.should

import scala.collection.immutable._

class ProfitSharingSpec extends AnyPropSpec with should.Matchers{
  val utils = new Utils()
  val client = new Client()
  client.setClient()
  val contracts = new Contracts(client, utils)

  val tokenId1 = "00ee077854471a04fbef18a5a971b50fb39f52fc6f6b3b8d0682ce2c48f6ebef"
  val tokenId2 = "11ee077854471a04fbef18a5a971b50fb39f52fc6f6b3b8d0682ce2c48f6ebef"
  val mockedClient: Client = mock(classOf[Client])
  when(mockedClient.getAllUnspentBox(contracts.incomeAddress)).thenReturn({
    var boxList: List[InputBox] = List()
    client.getClient.execute({ctx =>{
      for(i <- 1 to Configs.incomeMerge.min) {
        val txB = ctx.newTxBuilder()
        boxList = boxList :+
          txB.outBoxBuilder()
            .value(1e9.toLong)
            .contract(contracts.income)
            .build().convertToInputWith(randomId(), 1)
      }
      for(i <- 1 until Configs.incomeMerge.min) {
        val txB = ctx.newTxBuilder()
        boxList = boxList :+
          txB.outBoxBuilder()
            .value((1e9*0.001).toLong)
            .contract(contracts.income)
            .tokens(new ErgoToken(tokenId1, 10))
            .build().convertToInputWith(randomId(), 1)
      }
      for(i <- 1 to Configs.incomeMerge.max) {
        val txB = ctx.newTxBuilder()
        boxList = boxList :+
          txB.outBoxBuilder()
            .value((1e9*0.001).toLong)
            .contract(contracts.income)
            .tokens(new ErgoToken(tokenId2, 10))
            .build().convertToInputWith(randomId(), 1)
      }
    }})
    boxList
  })

  def createBoxObject: Boxes = new Boxes(mockedClient, utils, contracts)

  def randomId(): String ={
    val randomBytes = Array.fill(32)((scala.util.Random.nextInt(256) - 128).toByte)
    randomBytes.map("%02x" format _).mkString
  }

  property("Testing income box merging") {
    val boxes = createBoxObject
    val result = boxes.getIncomes
    print(result)
    result.size should be (2)
    result(0).size should be (Configs.incomeMerge.min)
    result(1).size should be (Configs.incomeMerge.max)
    result(1).head.getTokens.get(0).getId.toString should be (tokenId2)
  }

}
