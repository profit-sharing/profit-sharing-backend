package ProfitSharing
import helpers.Configs
import network.{Client, Explorer}
import org.ergoplatform.appkit.{BlockchainContext, CoveringBoxes, ErgoToken, InputBox}
import org.ergoplatform.appkit.impl.ErgoTreeContract
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito._
import play.api.libs.json.{JsArray, JsNumber, JsObject}

import scala.collection.immutable.List
import collection.JavaConverters._

class MockedEnv (client: Client, contracts: Contracts) {
  private val mockedClient = mock(classOf[Client])
  private val mockedCtx: BlockchainContext = mock(classOf[BlockchainContext])
  private val mockedExplorer: Explorer = mock(classOf[Explorer])
  val tokenId1 = "00ee077854471a04fbef18a5a971b50fb39f52fc6f6b3b8d0682ce2c48f6ebef"
  val tokenId2 = "11ee077854471a04fbef18a5a971b50fb39f52fc6f6b3b8d0682ce2c48f6ebef"

  def getMockedClient: Client = mockedClient
  def getMockedCtx: BlockchainContext = mockedCtx
  def getMockedExplorer: Explorer = mockedExplorer

  def randomId(): String ={
    val randomBytes = Array.fill(32)((scala.util.Random.nextInt(256) - 128).toByte)
    randomBytes.map("%02x" format _).mkString
  }

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
  when(mockedClient.getCoveringBoxesFor(Configs.initializer.address, Configs.fee*8)).thenReturn({
    client.getClient.execute(ctx =>{
      val txB = ctx.newTxBuilder()
      val box = txB.outBoxBuilder()
        .value(1e9.toLong)
        .contract(new ErgoTreeContract(Configs.initializer.address.getErgoAddress.script))
        .build().convertToInputWith(randomId(), 1)
      new CoveringBoxes(Configs.fee * 8, List(box).asJava)
    })
  })

  when(mockedCtx.newTxBuilder()).thenAnswer(_ => {
    client.getClient.execute(_.newTxBuilder())
  })
  when(mockedCtx.newProverBuilder()).thenAnswer(_ => {
    client.getClient.execute(_.newProverBuilder())
  })
  when(mockedCtx.sendTransaction(any())).thenReturn(randomId())

  when(mockedExplorer.getUnconfirmedTxByAddress(contracts.configAddress.toString)).thenReturn(
    JsObject(
      Seq(
        "items" -> JsArray(IndexedSeq()),
        "total" -> JsNumber(0)
      )
    )
  )
}
