package ProfitSharing


import helpers.{Configs, Utils}
import org.scalatest.propspec._
import network.Client
import org.ergoplatform.appkit.impl.ErgoTreeContract
import org.ergoplatform.appkit.{BlockchainContext, CoveringBoxes, ErgoToken, InputBox, SignedTransaction}
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito._
import org.scalatest.matchers.should

import collection.JavaConverters._
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

  val mockedCtx: BlockchainContext = mock(classOf[BlockchainContext])
  when(mockedCtx.newTxBuilder()).thenAnswer(_ => {
    client.getClient.execute(_.newTxBuilder())
  })
  when(mockedCtx.newProverBuilder()).thenAnswer(_ => {
    client.getClient.execute(_.newProverBuilder())
  })
  when(mockedCtx.sendTransaction(any())).thenReturn(randomId())

//  val fakeTx: SignedTransaction = createProcedureObject.tokenIssueTx(mockedCtx, 10, mockedClient.getCoveringBoxesFor(Configs.initializer.address, Configs.fee*8), Configs.initializer.address, "test token", "test token")

  def createBoxObject: Boxes = new Boxes(mockedClient, utils, contracts)
  def createProcedureObject: Procedures = new Procedures(mockedClient, createBoxObject, contracts, utils)

  def randomId(): String ={
    val randomBytes = Array.fill(32)((scala.util.Random.nextInt(256) - 128).toByte)
    randomBytes.map("%02x" format _).mkString
  }

  /**
   * Testing the getIncomes
   * It should return the aggregated list of boxes that are ready to be merged
   * The output must have two lists one for Erg and one for the required token type
   */
  property("Testing income box merging") {
    val boxes = createBoxObject
    val result = boxes.getIncomes
    result.size should be (2)
    result(0).size should be (Configs.incomeMerge.min)
    result(1).size should be (Configs.incomeMerge.max)
    result(1).head.getTokens.get(0).getId.toString should be (tokenId2)
  }

  /**
   * Testing the tokenIssueTx
   * It should return a transaction that issued a new token with specified details
   * The output must have a new token type with the specified amount
   */
  property("Testing token issue tx") {
    val procedures = createProcedureObject
    val mockedInputBox = mockedCtx.newTxBuilder().outBoxBuilder()
      .value(1e9.toLong)
      .contract(new ErgoTreeContract(Configs.initializer.address.getErgoAddress.script))
      .build().convertToInputWith(randomId(), 1)
    val tx = procedures.tokenIssueTx(mockedCtx, 10, List(mockedInputBox), Configs.initializer.address, "test token", "test token")
    tx.getOutputsToSpend.size() should be (3)
    tx.getOutputsToSpend.get(1).getTokens.size() should be (1)
    tx.getOutputsToSpend.get(1).getValue should be (Configs.fee)
    tx.getOutputsToSpend.get(1).getTokens.get(0).getValue should be (10)
    tx.getOutputsToSpend.get(2).getValue should be (Configs.fee)
  }

  /**
   * Testing mergeIncomesTx
   * It should return a transaction that merged the required boxes into one
   * The merged box must contain all gathered tokens and erg
   */
  property("Testing merge income tx") {
    val procedures = createProcedureObject
    var inputList: List[InputBox] = List()
    val txB = mockedCtx.newTxBuilder()
    for(i <- 1 to Configs.incomeMerge.min) {
      inputList = inputList :+ txB.outBoxBuilder()
        .value((1e9 * 0.1).toLong)
        .contract(contracts.income)
        .tokens(new ErgoToken(tokenId1, 10))
        .build().convertToInputWith(randomId(), 1)
    }
    val tx = procedures.mergeIncomesTx(inputList, mockedCtx)
    tx.getOutputsToSpend.size() should be (2)
    tx.getOutputsToSpend.get(0).getTokens.size() should be (1)
    tx.getOutputsToSpend.get(0).getValue.toLong should be >= ((1e9 * 0.1)*Configs.incomeMerge.min - Configs.incomeMerge.maxFee).toLong
    tx.getOutputsToSpend.get(0).getTokens.get(0).getValue should be (10 * Configs.incomeMerge.min)
    tx.getOutputsToSpend.get(1).getValue.toLong should be <= Configs.incomeMerge.maxFee
  }

  /**
   * Testing serviceInitialization
   * It should create the service box and return a list of 3 elements denoting the tokens
   * The service box must have three token types and the correct contract
   */
  property("Testing service initialization") {
    val procedures = createProcedureObject
    val result = procedures.serviceInitialization(mockedCtx)
    result should have size 3
  }

  /**
   * Testing mergeIncome
   * It should call mergeIncomeTx for each set of income boxes
   */
  property("Testing merge income service") {
    val boxes: Boxes = createBoxObject
    val spyBoxes: Boxes = spy(boxes)
    doReturn(List(List(), List()), List(List(), List())).when(spyBoxes).getIncomes
    val procedures = new Procedures(mockedClient, spyBoxes, contracts, utils)
    val spyProcedures = spy(procedures)
    doReturn(null, null).when(spyProcedures).mergeIncomesTx(List(), mockedCtx)
    spyProcedures.mergeIncomes(mockedCtx)
    verify(spyBoxes, times(1)).getIncomes
    verify(spyProcedures, times(2)).mergeIncomesTx(any(), any())
  }
}
