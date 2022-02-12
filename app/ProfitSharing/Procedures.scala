package ProfitSharing

import helpers.{Configs, Utils, internalException, proveException}
import network.Client
import org.ergoplatform.appkit.{BlockchainContext, SignedTransaction}
import play.api.Logger

import javax.inject.{Inject, Singleton}
import scala.collection.JavaConverters._

@Singleton
class Procedures@Inject()(client: Client ,boxes: Boxes, contracts: Contracts, utils: Utils, transactions: Transactions) {
  private val logger: Logger = Logger(this.getClass)

  def serviceInitialization(ctx: BlockchainContext): List[String] = try{
    val initialBox = client.getCoveringBoxesFor(Configs.initializer.address, Configs.fee * 8).getBoxes.asScala
    val configNFTTx: SignedTransaction = transactions.tokenIssueTx(ctx, 1, initialBox, Configs.initializer.address, "ErgoProfitSharing, ConfigNFT", "ErgoProfitSharing, ConfigNFT")
    val distTokenTx = transactions.tokenIssueTx(ctx, Configs.initializer.distributionCount, Seq(configNFTTx.getOutputsToSpend.get(0)), Configs.initializer.address, "ErgoProfitSharing, DistributionToken", "ErgoProfitSharing, DistributionToken")
    val lockingTokenTx = transactions.tokenIssueTx(ctx, Configs.initializer.lockingCount, Seq(distTokenTx.getOutputsToSpend.get(0)), Configs.initializer.address, "ErgoProfitSharing, LockingToken", "ErgoProfitSharing, LockingToken")
    val stakingTokenTx = transactions.tokenIssueTx(ctx, Configs.fee, Seq(lockingTokenTx.getOutputsToSpend.get(0)), Configs.owner.address, "ErgoProfitSharing, StakingToken", "ErgoProfitSharing, StakingToken")

    val configNFT = configNFTTx.getOutputsToSpend.get(1).getTokens.get(0).getId.toString
    val distributionToken = distTokenTx.getOutputsToSpend.get(1).getTokens.get(0).getId.toString
    val lockingToken = lockingTokenTx.getOutputsToSpend.get(1).getTokens.get(0).getId.toString
    val stakingToken = stakingTokenTx.getOutputsToSpend.get(1).getTokens.get(0).getId.toString
    Configs.token.configNFT = configNFT
    Configs.token.distribution = distributionToken
    Configs.token.locking = lockingToken
    Configs.token.staking = stakingToken
    logger.info(s"Config NFT: $configNFT")
    logger.info(s"Distribution Token: $distributionToken")
    logger.info(s"locking Token: $lockingToken")
    logger.info(s"staking Token: $stakingToken")
    logger.debug(s"Config.NFT ${Configs.token.configNFT}")

    val txB = ctx.newTxBuilder()
    val configBox = boxes.createConfig(txB)

    val tx = txB.boxesToSpend(Seq(configNFTTx.getOutputsToSpend.get(1), distTokenTx.getOutputsToSpend.get(1), lockingTokenTx.getOutputsToSpend.get(1)).asJava)
      .fee(Configs.fee)
      .outputs(configBox)
      .sendChangeTo(Configs.initializer.address.getErgoAddress)
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
      logger.error(s"config creation tx sending failed")
      List()
    }
    else {
      logger.info(s" config box created successfully")
      List(configNFT, distributionToken, lockingToken)
    }
  } catch {
    case _: proveException =>
      logger.error("initialization failed")
      List()
    case e: Throwable =>
      logger.error("initialization failed")
      logger.error(utils.getStackTraceStr(e))
      List()
  }

  def mergeIncomes(ctx: BlockchainContext): Unit = try{
    logger.debug(s"income address is ${contracts.incomeAddress}")
    val incomes = boxes.getIncomes
    logger.debug(s"income list size is ${incomes.size}")
    if(incomes.nonEmpty)
      for(incomeSet <- incomes) {
        try {
          val signedTx = transactions.mergeIncomesTx(incomeSet, ctx)
          var txId = ctx.sendTransaction(signedTx)
          if (txId == null) logger.error(s"Merge transaction sending failed")
          else {
            txId = txId.replaceAll("\"", "")
            logger.info(s"Merge Transaction Sent with TxId: " + txId)
          }
        } catch {
          case _: proveException =>
          case e: Throwable =>
            logger.error(utils.getStackTraceStr(e))
            throw internalException()
        }
      }
  } catch {
    case _: internalException => logger.warn("Something went wrong on merging")
    case e: Throwable => logger.error(utils.getStackTraceStr(e))
  }

  def locking(ctx: BlockchainContext): Unit = try{
    val userBox = client.getAllUnspentBox(Configs.user.address).filter(_.getTokens.size() > 0)
      .filter(_.getTokens.get(0).getId.toString == Configs.token.staking).head
    transactions.lockingTx(userBox, Configs.user.address, boxes.findConfig(ctx), ctx)
  } catch {
    case _: internalException => logger.warn("Something went wrong on locking")
    case e: Throwable => logger.error(utils.getStackTraceStr(e))
  }
}
