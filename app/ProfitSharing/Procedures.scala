package ProfitSharing

import helpers.{Configs, Utils, failedTxException, proveException}
import network.Client
import org.ergoplatform.appkit.impl.ErgoTreeContract
import org.ergoplatform.appkit.{Address, BlockchainContext, ErgoToken, InputBox, SignedTransaction}
import play.api.Logger

import javax.inject.{Inject, Singleton}
import scala.collection.JavaConverters._

@Singleton
class Procedures@Inject()(client: Client ,boxes: Boxes, contracts: Contracts, utils: Utils) {
  private val logger: Logger = Logger(this.getClass)

  def tokenIssueTx(ctx: BlockchainContext, count: Long, inputs: Seq[InputBox], address: Address, name: String): SignedTransaction ={
    val txB = ctx.newTxBuilder()

    val totalValue: Long = inputs.map(item => item.getValue).reduce((a, b) => a + b)
    val output = txB.outBoxBuilder()
      .value(totalValue - 2*Configs.fee)
      .contract(new ErgoTreeContract(address.getErgoAddress.script))
      .build()

    val outToken = txB.outBoxBuilder()
      .value(Configs.fee)
      .tokens(new ErgoToken(inputs.head.getId, count))
      .contract(new ErgoTreeContract(address.getErgoAddress.script))
      .build()

    val tx = txB.boxesToSpend(inputs.asJava)
      .fee(Configs.fee)
      .outputs(output, outToken)
      .sendChangeTo(address.getErgoAddress)
      .build()

    val prover = ctx.newProverBuilder()
      .withDLogSecret(Configs.initializer.secret)
      .build()
    var signedTx: SignedTransaction = null
    try {
      signedTx = prover.sign(tx)
    } catch {
      case e: Throwable =>
        logger.error(utils.getStackTraceStr(e))
        logger.error(s"token $name issue tx proving failed")
        throw proveException()
    }
    logger.info(s" token $name issued successfully")
    val txId = ctx.sendTransaction(signedTx)
    if (txId == null) logger.error(s"Merge transaction sending failed")
    signedTx
  }

  def serviceInitialization(): List[String] = try{
    client.getClient.execute(ctx =>{
      val initialBox = client.getCoveringBoxesFor(Configs.initializer.address, Configs.fee * 8).getBoxes.asScala
      val configNFTTx: SignedTransaction = tokenIssueTx(ctx, 1, initialBox, Configs.initializer.address, "ErgoProfitSharing, ConfigNFT")
      val distTokenTx = tokenIssueTx(ctx, Configs.initializer.distributionCount, Seq(configNFTTx.getOutputsToSpend.get(0)), Configs.initializer.address, "ErgoProfitSharing, DistributionToken")
      val lockingTokenTx = tokenIssueTx(ctx, Configs.initializer.lockingCount, Seq(distTokenTx.getOutputsToSpend.get(0)), Configs.initializer.address, "ErgoProfitSharing, LockingToken")

      val configNFT = configNFTTx.getOutputsToSpend.get(1).getTokens.get(0).getId.toString
      val distributionToken = distTokenTx.getOutputsToSpend.get(1).getTokens.get(0).getId.toString
      val lockingToken = lockingTokenTx.getOutputsToSpend.get(1).getTokens.get(0).getId.toString
      logger.info(s"Config NFT: $configNFT")
      logger.info(s"Distribution Token: $distributionToken")
      logger.info(s"locking Token: $lockingToken")

      val txB = ctx.newTxBuilder()
      val configBox = txB.outBoxBuilder()
        .value(Configs.fee*2)
        .contract(contracts.config)
        .tokens(new ErgoToken(configNFT, 1),
          new ErgoToken(distributionToken, Configs.initializer.distributionCount),
          new ErgoToken(lockingToken, Configs.initializer.lockingCount))
        .registers(utils.longListToErgoValue(Array(1, 1e9.toLong, 10, 0, 0, Configs.fee, 1e9.toLong, Configs.minBoxErg)))
        .build()

      val tx = txB.boxesToSpend(Seq(configNFTTx.getOutputsToSpend.get(1), distTokenTx.getOutputsToSpend.get(1), lockingTokenTx.getOutputsToSpend.get(1)).asJava)
        .fee(Configs.fee)
        .outputs(configBox)
        .sendChangeTo(contracts.incomeAddress.getErgoAddress)
        .build()

      val prover = ctx.newProverBuilder()
        .withDLogSecret(Configs.initializer.secret)
        .build()
      var signedTx: SignedTransaction = null
      try {
        signedTx = prover.sign(tx)
      } catch {
        case e: Throwable =>
          logger.error(utils.getStackTraceStr(e))
          logger.error(s"config creation tx proving failed")
          throw proveException()
      }
      val txId = ctx.sendTransaction(signedTx)
      if (txId == null) {
        logger.error(s"Merge transaction sending failed")
        List()
      }
      else {
        logger.info(s" config box created successfully")
        List(configNFT, distributionToken, lockingToken)
      }
    })
  } catch {
    case _: proveException =>
      logger.error("initialization failed")
      List()
    case e: Throwable =>
      logger.error("initialization failed")
      logger.error(utils.getStackTraceStr(e))
      List()
  }

  def mergeIncomesTx(incomes: Seq[InputBox], ctx: BlockchainContext): SignedTransaction = {
      val txB = ctx.newTxBuilder()

      val totalValue: Long = incomes.map(item => item.getValue).reduce((a, b) => a + b)
      var outIncome = txB.outBoxBuilder()
        .value(totalValue - Configs.fee)
        .contract(contracts.income)

      if(incomes.head.getTokens.size() > 0) {
        val tokenId: String = incomes.head.getTokens.get(0).getId.toString
        val totalTokens: Long = incomes.map(item => item.getTokens.get(0).getValue).sum
        outIncome = outIncome.tokens(new ErgoToken(tokenId, totalTokens))
      }
      val outIncomeBox = outIncome.build()

      val tx = txB.boxesToSpend(incomes.asJava)
        .fee(Configs.fee)
        .outputs(outIncomeBox)
        .sendChangeTo(contracts.incomeAddress.getErgoAddress)
        .build()

      val prover = ctx.newProverBuilder().build()
      var signedTx: SignedTransaction = null
      try {
        signedTx = prover.sign(tx)
      } catch {
        case e: Throwable =>
          logger.error(utils.getStackTraceStr(e))
          logger.error(s"merge tx proving failed")
          throw proveException()
      }
      logger.info(s"${incomes.size} Incomes with total value of $totalValue merged successfully")
      signedTx
  }

  def mergeIncomes(): Unit = try{
    client.getClient.execute(ctx => {
      val incomes = boxes.getIncomes
      for(incomeSet <- incomes) {
        val signedTx = mergeIncomesTx(incomeSet, ctx)
        var txId = ctx.sendTransaction(signedTx)
        if (txId == null) logger.error(s"Merge transaction sending failed")
        else {
          txId = txId.replaceAll("\"", "")
          logger.info(s"Merge Transaction Sent with TxId: " + txId)
        }
      }
    })
  } catch {
    case _: java.lang.NullPointerException => logger.info("No incomes found in the network")
    case e: Throwable => logger.error(utils.getStackTraceStr(e))
  }

}
