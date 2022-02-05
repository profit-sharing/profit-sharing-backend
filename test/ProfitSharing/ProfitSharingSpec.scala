package ProfitSharing


import helpers.{Configs, Utils}
import org.scalatest.propspec._
import network.Client
import org.ergoplatform.appkit.impl.ErgoTreeContract
import org.ergoplatform.appkit.{ErgoToken, InputBox}
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito._
import org.scalatest.matchers.should

import scala.collection.immutable._

class ProfitSharingSpec extends AnyPropSpec with should.Matchers{
  val utils = new Utils()
  val client = new Client()
  client.setClient()
  val contracts = new Contracts(client, utils)

  def getMockedBoxes(mockedEnv: MockedEnv): Boxes = new Boxes(mockedEnv.getMockedClient, utils, contracts)
  def getMockedProcedure(mockedEnv: MockedEnv): Procedures = {
    val boxes = new Boxes(mockedEnv.getMockedClient, utils, contracts)
    new Procedures(mockedEnv.getMockedClient, boxes, contracts, utils)
  }

  /**
   * Target: testing getIncomes
   * Dependencies:
   *    Client & Ctx
   * Procedure:
   *    1- mocking environment
   *    2- Calling getIncome function
   * Expected Output:
   *    The function should return a list of incomes which are ready to merge
   *    The list should have 2 inner lists
   *    Each list should contain [mergeIncome.min, mergeIncome.max) boxes, The first one testing the lower bound and second testing the upper bound.
   */
  property("Testing income box search") {
    val mockedEnv = new MockedEnv(client, contracts)
    val boxes = getMockedBoxes(mockedEnv)
    val result = boxes.getIncomes
    result.size should be (2)
    result(0).size should be (Configs.incomeMerge.min)
    result(1).size should be (Configs.incomeMerge.max)
    result(1).head.getTokens.get(0).getId.toString should be (mockedEnv.tokenId2)
  }

  /**
   * Target: testing tokenIssue
   * Dependencies:
   *    Ctx
   * Procedure:
   *    1- mocking environment
   *    2- mocking inputBox
   *    2- Calling tokenIssue function
   * Expected Output:
   *    The function should return a transaction issued a new token with required amount
   */
  property("Testing token issue tx") {
    val mockedEnv = new MockedEnv(client, contracts)
    val procedures = getMockedProcedure(mockedEnv)
    val mockedInputBox = mockedEnv.getMockedCtx.newTxBuilder().outBoxBuilder()
      .value(1e9.toLong)
      .contract(new ErgoTreeContract(Configs.initializer.address.getErgoAddress.script))
      .build().convertToInputWith(mockedEnv.randomId(), 1)
    val tx = procedures.tokenIssueTx(mockedEnv.getMockedCtx, 10, List(mockedInputBox), Configs.initializer.address, "test token", "test token")
    tx.getOutputsToSpend.size() should be (3)
    tx.getOutputsToSpend.get(1).getTokens.size() should be (1)
    tx.getOutputsToSpend.get(1).getValue should be (Configs.fee)
    tx.getOutputsToSpend.get(1).getTokens.get(0).getValue should be (10)
    tx.getOutputsToSpend.get(2).getValue should be (Configs.fee)
  }

  /**
   * Target: testing mergeIncomeTx
   * Dependencies:
   *    Ctx
   * Procedure:
   *    1- mocking environment
   *    2- mocking inputBoxes
   *    2- Calling mergeIncome function
   * Expected Output:
   *    The function should return a transaction merging all input incomes
   */
  property("Testing merge income tx") {
    val mockedEnv = new MockedEnv(client, contracts)
    val procedures = getMockedProcedure(mockedEnv)
    var inputList: List[InputBox] = List()
    val txB = mockedEnv.getMockedCtx.newTxBuilder()
    for(i <- 1 to Configs.incomeMerge.min) {
      inputList = inputList :+ txB.outBoxBuilder()
        .value((1e9 * 0.1).toLong)
        .contract(contracts.income)
        .tokens(new ErgoToken(mockedEnv.tokenId1, 10))
        .build().convertToInputWith(mockedEnv.randomId(), 1)
    }
    val tx = procedures.mergeIncomesTx(inputList, mockedEnv.getMockedCtx)
    tx.getOutputsToSpend.size() should be (2)
    tx.getOutputsToSpend.get(0).getTokens.size() should be (1)
    tx.getOutputsToSpend.get(0).getValue.toLong should be >= ((1e9 * 0.1)*Configs.incomeMerge.min - Configs.incomeMerge.maxFee).toLong
    tx.getOutputsToSpend.get(0).getTokens.get(0).getValue should be (10 * Configs.incomeMerge.min)
    tx.getOutputsToSpend.get(1).getValue.toLong should be <= Configs.incomeMerge.maxFee
  }

  /**
   * Target: testing serviceInitialization
   * Dependencies:
   *    Ctx
   *    Procedures.tokenIssueTx
   * Procedure:
   *    1- mocking environment
   *    2- Calling serviceInitialization function
   * Expected Output:
   *    The function should create the service box in 4 transactions
   *    The function should return 3 token ids related to service
   */
  property("Testing service initialization") {
    val mockedEnv = new MockedEnv(client, contracts)
    val procedures = getMockedProcedure(mockedEnv)
    val result = procedures.serviceInitialization(mockedEnv.getMockedCtx)
    result should have size 3
    // TODO: Mock the tokenIssueTx and change this to 1 time
    verify(mockedEnv.getMockedCtx, times(4)).sendTransaction(any())
  }

  /**
   * Target: testing mergeIncome
   * Dependencies:
   *    Client & Ctx
   *    Boxes
   *    Procedures.mergeIncomeTx
   * Procedure:
   *    1- mocking environment
   *    1- mocking boxes object (getIncomes)
   *    2- mocking Procedure.mergeIncomeTx
   *    3- Calling mergeIncome function
   * Expected Output:
   *    The function should get incomes once
   *    The function should call mergeIncomeTx 2 times (2 input lists were passed)
   */
  property("Testing merge income service") {
    val mockedEnv = new MockedEnv(client, contracts)
    val boxes = getMockedBoxes(mockedEnv)
    val spyBoxes: Boxes = spy(boxes)
    doReturn(List(List(), List()), List(List(), List())).when(spyBoxes).getIncomes
    val procedures = new Procedures(mockedEnv.getMockedClient, spyBoxes, contracts, utils)
    val spyProcedures = spy(procedures)
    doReturn(null, null).when(spyProcedures).mergeIncomesTx(List(), mockedEnv.getMockedCtx)
    spyProcedures.mergeIncomes(mockedEnv.getMockedCtx)
    verify(spyBoxes, times(1)).getIncomes
    verify(spyProcedures, times(2)).mergeIncomesTx(any(), any())
  }
}
