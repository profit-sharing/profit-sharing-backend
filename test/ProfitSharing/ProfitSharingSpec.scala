package ProfitSharing


import helpers.{Configs, Utils}
import org.scalatest.propspec._
import network.{Client, Explorer}
import org.ergoplatform.appkit.impl.ErgoTreeContract
import org.ergoplatform.appkit.{ErgoToken, InputBox}
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito._
import org.scalatest.matchers.should

import scala.collection.immutable._

class ProfitSharingSpec extends AnyPropSpec with should.Matchers{
  val client = new Client()
  client.setClient()
  val utils = new Utils()
  val contracts = new Contracts(client, utils)

  def getMockedBoxes(mockedEnv: MockedEnv): Boxes = new Boxes(mockedEnv.getMockedClient, utils, contracts, mockedEnv.getMockedExplorer)
  def getMockedTransaction(mockedEnv: MockedEnv): Transactions = {
    val boxes = new Boxes(mockedEnv.getMockedClient, utils, contracts, mockedEnv.getMockedExplorer)
    new Transactions(boxes, contracts, utils)
  }
  def getMockedProcedure(mockedEnv: MockedEnv): Procedures = {
    val boxes = new Boxes(mockedEnv.getMockedClient, utils, contracts, mockedEnv.getMockedExplorer)
    val transactions = new Transactions(boxes, contracts, utils)
    new Procedures(mockedEnv.getMockedClient, boxes, contracts, utils, transactions)
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

  // TODO: Improve this test
  /**
   * Target: testing findLastMempoolBoxFor
   * Dependencies:
   *    Explorer & Ctx
   * Procedure:
   *    1- mocking environment
   *    2- mocking input
   *    3- Calling findLastMempoolBoxFor function
   * Expected Output:
   *    The function should return the last related box
   *    The mempool is empty so the last box must be the first input
   */
  property("Testing last mempool box search") {
    val mockedEnv = new MockedEnv(client, contracts)
    val boxes = getMockedBoxes(mockedEnv)
    val address = contracts.configAddress
    val inputBox = mockedEnv.getMockedCtx.newTxBuilder().outBoxBuilder()
      .value(10000)
      .contract(new ErgoTreeContract(address.getErgoAddress.script))
      .build().convertToInputWith(mockedEnv.randomId(), 1)
    val result = boxes.findLastMempoolBoxFor(address.toString, inputBox, mockedEnv.getMockedCtx)
    result shouldEqual inputBox
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
    val transactions = getMockedTransaction(mockedEnv)
    val mockedInputBox = mockedEnv.getMockedCtx.newTxBuilder().outBoxBuilder()
      .value(1e9.toLong)
      .contract(new ErgoTreeContract(Configs.initializer.address.getErgoAddress.script))
      .build().convertToInputWith(mockedEnv.randomId(), 1)
    val tx = transactions.tokenIssueTx(mockedEnv.getMockedCtx, 10, List(mockedInputBox), Configs.initializer.address, "test token", "test token")
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
    val transactions = getMockedTransaction(mockedEnv)
    var inputList: List[InputBox] = List()
    val txB = mockedEnv.getMockedCtx.newTxBuilder()
    for(i <- 1 to Configs.incomeMerge.min) {
      inputList = inputList :+ txB.outBoxBuilder()
        .value((1e9 * 0.1).toLong)
        .contract(contracts.income)
        .tokens(new ErgoToken(mockedEnv.tokenId1, 10))
        .build().convertToInputWith(mockedEnv.randomId(), 1)
    }
    val tx = transactions.mergeIncomesTx(inputList, mockedEnv.getMockedCtx)
    tx.getOutputsToSpend.size() should be (2)
    tx.getOutputsToSpend.get(0).getTokens.size() should be (1)
    tx.getOutputsToSpend.get(0).getValue.toLong should be >= ((1e9 * 0.1)*Configs.incomeMerge.min - Configs.incomeMerge.maxFee).toLong
    tx.getOutputsToSpend.get(0).getTokens.get(0).getValue should be (10 * Configs.incomeMerge.min)
    tx.getOutputsToSpend.get(1).getValue.toLong should be <= Configs.incomeMerge.maxFee
  }

  /**
   * Target: testing lockingTx
   * Dependencies:
   *    Ctx
   * Procedure:
   *    1- mocking environment
   *    2- mocking inputBoxes
   *    2- Calling lockingTx function
   * Expected Output:
   *    The function should return a transaction locked the staking tokens
   *    transaction must have 4 outputs, config box, locked tokens, reserved token and fee
   */
  property("Testing staking token locking tx") {
    val mockedEnv = new MockedEnv(client, contracts)
    val transactions = getMockedTransaction(mockedEnv)
    val mockedTokenBox = mockedEnv.getMockedCtx.newTxBuilder().outBoxBuilder()
      .value(1e9.toLong + 2*Configs.fee)
      .contract(new ErgoTreeContract(Configs.user.address.getErgoAddress.script))
      .tokens(new ErgoToken(Configs.token.staking, 10))
      .build().convertToInputWith(mockedEnv.randomId(), 1)
    val mockedConfigBox = mockedEnv.getMockedCtx.newTxBuilder().outBoxBuilder()
      .value(1e9.toLong)
      .contract(contracts.config)
      .tokens(new ErgoToken(Configs.token.configNFT, 1),
        new ErgoToken(Configs.token.distribution, 100),
        new ErgoToken(Configs.token.locking, 20))
      .registers(utils.longListToErgoValue(Array(1, 1e9.toLong, 10, 0, 0, Configs.fee, 1e9.toLong, Configs.minBoxErg)))
      .build().convertToInputWith(mockedEnv.randomId(), 1)
    val tx = transactions.lockingTx(mockedTokenBox, Configs.user.address, mockedConfigBox, mockedEnv.getMockedCtx)

    tx.getOutputsToSpend.size() should be (4)
    tx.getOutputsToSpend.get(0).getTokens.size() should be (3)
    tx.getOutputsToSpend.get(1).getTokens.size() should be (2)
    tx.getOutputsToSpend.get(1).getValue should be (1e9.toLong)
    tx.getOutputsToSpend.get(1).getTokens.get(0).getValue should be (1)
    tx.getOutputsToSpend.get(1).getTokens.get(1).getValue should be (10)
    tx.getOutputsToSpend.get(2).getTokens.get(0).getValue should be (1)
    tx.getOutputsToSpend.get(2).getValue should be (Configs.fee)
    tx.getOutputsToSpend.get(3).getValue should be (Configs.fee)
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
   *    The function should create the service box in 5 transactions
   *    The function should return 4 token ids related to service
   */
  property("Testing service initialization") {
    val mockedEnv = new MockedEnv(client, contracts)
    val procedures = getMockedProcedure(mockedEnv)
    val result = procedures.serviceInitialization(mockedEnv.getMockedCtx)
    result should have size 4
    // TODO: Mock the tokenIssueTx and change this to 1 time
    verify(mockedEnv.getMockedCtx, times(5)).sendTransaction(any())
  }

  /**
   * Target: testing mergeIncome
   * Dependencies:
   *    Client & Ctx
   *    Boxes
   *    Transactions
   * Procedure:
   *    1- mocking environment
   *    1- mocking boxes object (getIncomes)
   *    2- mocking transactions object (mergeIncomeTx)
   *    3- Calling mergeIncome function
   * Expected Output:
   *    The function should get incomes once
   *    The function should call mergeIncomeTx 2 times (2 input lists were passed)
   */
  property("Testing merge income service") {
    val mockedEnv = new MockedEnv(client, contracts)
    val mockedBoxes: Boxes = mock(classOf[Boxes])
    doReturn(List(List(), List()), List(List(), List())).when(mockedBoxes).getIncomes
    val mockedTransactions: Transactions = mock(classOf[Transactions])
    doReturn(null, null).when(mockedTransactions).mergeIncomesTx(List(), mockedEnv.getMockedCtx)
    val procedures = new Procedures(mockedEnv.getMockedClient, mockedBoxes, contracts, utils, mockedTransactions)
    procedures.mergeIncomes(mockedEnv.getMockedCtx)
    verify(mockedBoxes, times(1)).getIncomes
    verify(mockedTransactions, times(2)).mergeIncomesTx(any(), any())
  }
}
