package ProfitSharing

import helpers.{Configs, Utils, failedTxException, internalException, notCoveredException, proveException}
import models.{Config, Distribution}
import network.Client
import org.ergoplatform.appkit.{BlockchainContext, InputBox, SignedTransaction}
import play.api.Logger

import javax.inject.{Inject, Singleton}
import scala.collection.JavaConverters._

@Singleton
class Procedures@Inject()(client: Client ,boxes: Boxes, contracts: Contracts, transactions: Transactions) {
  private val logger: Logger = Logger(this.getClass)

  def serviceInitialization(ctx: BlockchainContext): List[String] = try{
    val initialBox = client.getCoveringBoxesFor(Configs.initializer.address, Configs.fee * 8).getBoxes.asScala.filter(_.getTokens.size() == 0)
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
    val signedTx: SignedTransaction = try {
      prover.sign(tx)
    } catch {
      case e: Throwable =>
        logger.error(Utils.getStackTraceStr(e))
        logger.error(s"config creation tx proving failed")
        throw proveException()
    }
    try{
      ctx.sendTransaction(signedTx)
      logger.info(s"config box created successfully with txId: ${signedTx.getId}")
      List(configNFT, distributionToken, lockingToken, stakingToken)
    } catch{
      case _: Throwable =>
        logger.error(s"Config box creation tx sending failed")
        List()
    }
  } catch {
    case _: proveException | _: failedTxException =>
      logger.error("initialization failed")
      List()
    case e: Throwable =>
      logger.error("initialization failed")
      logger.error(Utils.getStackTraceStr(e))
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
          try{
            ctx.sendTransaction(signedTx)
            logger.info(s"Merge tx Sent with TxId: ${signedTx.getId}")
          } catch{
            case _: Throwable =>
              logger.error(s"Merge tx sending failed")
          }
        } catch {
          case _: proveException =>
          case e: Throwable =>
            logger.error(Utils.getStackTraceStr(e))
            throw internalException()
        }
      }
  } catch {
    case _: internalException => logger.warn("Something went wrong on merging")
    case e: Throwable => logger.error(Utils.getStackTraceStr(e))
  }

  def distributionCreation(ctx: BlockchainContext): Unit = try{
    var configBox = boxes.findConfig(ctx)
    val config = Config(configBox)
    client.getAllUnspentBox(contracts.incomeAddress).foreach(income =>{
      if(income.getValue >= config.minErgShare * config.stakeCount + 2 * Configs.fee ||
        (income.getTokens.size() > 0 && income.getTokens.get(0).getValue >= config.minTokenShare * config.stakeCount)) {
        logger.info("one income hits the threshold creating the distribution")
        val signedTx = transactions.distributionCreationTx(ctx, income, configBox)
        try{
          ctx.sendTransaction(signedTx)
          logger.info(s"Distribution creation tx Sent with TxId: ${signedTx.getId}")
          configBox = signedTx.getOutputsToSpend.get(0)
        } catch{
          case _: Throwable =>
            logger.error(s"Distribution creation tx sending failed")
        }
      }
    })
  } catch {
    case e: notCoveredException => logger.error(e.getMessage)
    case _: proveException => logger.error("Distribution creation failed")
    case _: internalException => logger.warn("Something went wrong on distribution creation")
    case e: Throwable => logger.error(Utils.getStackTraceStr(e))
  }

  def payment(ctx: BlockchainContext): Unit = try{
    var configBox = boxes.findConfig(ctx)
    boxes.findDistributions().foreach(distributionBox =>{
      var spendingBox = distributionBox
      val distribution = Distribution(distributionBox)
      if(distribution.ticketCount > 0){
        boxes.findTickets(distribution.checkpoint).foreach(ticketBox =>{
          val signedTx = transactions.distributionPaymentTx(ctx, spendingBox, ticketBox)
          try{
            ctx.sendTransaction(signedTx)
            logger.info(s"Distribution payment tx Sent with TxId: ${signedTx.getId}")
            spendingBox = signedTx.getOutputsToSpend.get(0)
            logger.debug(s"${Distribution(spendingBox).ticketCount} number of payments left for the distribution with checkpoint ${distribution.checkpoint}")
          } catch{
            case _: Throwable =>
              logger.error(s"Distribution payment tx sending failed")
          }
        })
      } else {
        val signedTx = transactions.distributionRedeemTx(ctx, configBox, distributionBox)
        try{
          ctx.sendTransaction(signedTx)
          logger.info(s"Distribution redeem tx Sent with TxId: ${signedTx.getId}")
        } catch{
          case _: Throwable =>
            logger.error(s"Distribution redeem tx sending failed")
        }
      }
    })
  } catch {
    case _: proveException => logger.error("Distribution payment failed")
    case _: internalException => logger.warn("Something went wrong on distribution payment")
    case e: Throwable => logger.error(Utils.getStackTraceStr(e))
  }
}
