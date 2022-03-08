package ProfitSharing
import helpers.{Configs, Utils}
import network.{Client, Explorer}
import org.ergoplatform.appkit.{BlockchainContext, CoveringBoxes, ErgoToken, ErgoValue, InputBox}
import org.ergoplatform.appkit.impl.ErgoTreeContract
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito._
import org.mockito.invocation.InvocationOnMock

import scala.collection.immutable.List
import collection.JavaConverters._

class MockedEnv (client: Client, contracts: Contracts) {
  private val mockedClient = mock(classOf[Client])
  private val mockedCtx: BlockchainContext = mock(classOf[BlockchainContext])
  private val mockedExplorer: Explorer = mock(classOf[Explorer])
  val dataset: testDataset = client.getClient.execute(new testDataset(_))
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
      val txB = ctx.newTxBuilder()
      for(i <- 1 to Configs.incomeMerge.min) {
        boxList = boxList :+
          txB.outBoxBuilder()
            .value(1e9.toLong)
            .contract(contracts.income)
            .build().convertToInputWith(randomId(), 1)
      }
      for(i <- 1 until Configs.incomeMerge.min) {
        boxList = boxList :+
          txB.outBoxBuilder()
            .value((1e9*0.001).toLong)
            .contract(contracts.income)
            .tokens(new ErgoToken(tokenId1, 5))
            .build().convertToInputWith(randomId(), 1)
      }
      for(i <- 1 to Configs.incomeMerge.max) {
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
  when(mockedClient.getAllUnspentBox(contracts.distributionAddress)).thenReturn({
    var boxList: List[InputBox] = List()
    client.getClient.execute({ctx =>{
      val txB = ctx.newTxBuilder()
      boxList = boxList :+
        txB.outBoxBuilder()
          .value(2e9.toLong)
          .contract(contracts.distribution)
          .build().convertToInputWith(randomId(), 1)
      for(i <- 101 to 110) {
        boxList = boxList :+
          txB.outBoxBuilder()
            .value(1e9.toLong)
            .contract(contracts.distribution)
            .tokens(new ErgoToken(Configs.token.distribution, 1))
            .registers(Utils.longListToErgoValue(Array(i, 100000, 200, Configs.fee)), ErgoValue.of(10L))
            .build().convertToInputWith(randomId(), 1)
      }
    }})
    boxList
  })
  when(mockedClient.getAllUnspentBox(contracts.ticketAddress)).thenReturn({
    var boxList: List[InputBox] = List()
    client.getClient.execute({ctx =>{
      val txB = ctx.newTxBuilder()
      boxList = boxList :+
        txB.outBoxBuilder()
          .value(1e9.toLong)
          .contract(contracts.ticket)
          .tokens(new ErgoToken(Configs.token.locking, 1), new ErgoToken(Configs.token.staking, 10))
          .registers(Utils.longListToErgoValue(Array(100, 103,Configs.fee, Configs.minBoxErg)),
            ErgoValue.of(new ErgoTreeContract(Configs.user.address.getErgoAddress.script).getErgoTree.bytes),
            ErgoValue.of(randomId().getBytes()))
          .build().convertToInputWith(randomId(), 1)
      for(i <- 101 to 110) {
        boxList = boxList :+
          txB.outBoxBuilder()
            .value(1e9.toLong)
            .contract(contracts.ticket)
            .tokens(new ErgoToken(Configs.token.locking, 1), new ErgoToken(Configs.token.staking, i))
            .registers(Utils.longListToErgoValue(Array(i, 101,Configs.fee, Configs.minBoxErg)),
              ErgoValue.of(new ErgoTreeContract(Configs.user.address.getErgoAddress.script).getErgoTree.bytes),
              ErgoValue.of(randomId().getBytes()))
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
  when(mockedCtx.signedTxFromJson(any())).thenAnswer((invocation: InvocationOnMock) => {
    val input = invocation.getArgument(0, classOf[String])
    client.getClient.execute(_.signedTxFromJson(input))
  })

  when(mockedExplorer.getUnconfirmedTxByAddress(dataset.lastMempoolBoxTest._2)).thenReturn(dataset.lastMempoolBoxTest._1)
  when(mockedExplorer.getUnconfirmedTxByAddress(contracts.configAddress.toString)).thenReturn(dataset.emptyResponse)
  when(mockedExplorer.getTxsInMempoolByAddress(dataset.boxInMempoolTest._2)).thenReturn(dataset.boxInMempoolTest._1)
}
