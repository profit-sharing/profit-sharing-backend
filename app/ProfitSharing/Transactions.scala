package ProfitSharing

import helpers.{Configs, Utils, failedTxException, notCoveredException, proveException}
import models.{Config, Distribution, Ticket}
import org.ergoplatform.appkit.{Address, BlockchainContext, ErgoToken, ErgoValue, InputBox, OutBox, SignedTransaction, UnsignedTransaction}
import org.ergoplatform.appkit.impl.ErgoTreeContract
import play.api.Logger
import special.collection.Coll

import scala.collection.JavaConverters._
import javax.inject.Inject

class Transactions@Inject()(boxes: Boxes, contracts: Contracts) {
  private val logger: Logger = Logger(this.getClass)

  def tokenIssueTx(ctx: BlockchainContext, count: Long, inputs: Seq[InputBox], address: Address, name: String, description: String): SignedTransaction ={
    val txB = ctx.newTxBuilder()

    val totalValue: Long = inputs.map(item => item.getValue.toLong).sum
    val output = txB.outBoxBuilder()
      .value(totalValue - 2*Configs.fee)
      .contract(new ErgoTreeContract(address.getErgoAddress.script))
      .build()

    val outToken = txB.outBoxBuilder()
      .value(Configs.fee)
      .tokens(new ErgoToken(inputs.head.getId, count))
      .registers(ErgoValue.of(name.getBytes("utf-8")), ErgoValue.of(description.getBytes("utf-8")), ErgoValue.of("0".getBytes("utf-8")))
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
    val signedTx: SignedTransaction = try {
      prover.sign(tx)
    } catch {
      case e: Throwable =>
        logger.error(Utils.getStackTraceStr(e))
        logger.error(s"token $name issue tx proving failed")
        throw proveException()
    }
    try{
      ctx.sendTransaction(signedTx)
      logger.info(s"Token $name issued successfully with txId: ${signedTx.getId}")
      signedTx
    } catch{
      case _: Throwable =>
        logger.error(s"Token Issue transaction sending failed")
        throw failedTxException()
    }
  }

  def mergeIncomesTx(incomes: Seq[InputBox], ctx: BlockchainContext): SignedTransaction = {
    val txB = ctx.newTxBuilder()

    val totalValue: Long = incomes.map(item => item.getValue).reduce((a, b) => a + b)
    val fee: Long = incomes.size * Configs.incomeMerge.boxSize * Configs.feePerByte
    logger.debug(s"Calculated fee for merge transaction is $fee and maximum available fee is ${Configs.incomeMerge.maxFee}")

    val tokenId: String = try{incomes.head.getTokens.get(0).getId.toString} catch{case _:Throwable => ""}
    val totalTokens: Long = try{incomes.map(item => item.getTokens.get(0).getValue).sum} catch{case _:Throwable => 0}
    val outIncomeBox = boxes.getIncome(txB, totalValue - fee, totalTokens, tokenId)

    val tx = txB.boxesToSpend(incomes.asJava)
      .fee(fee)
      .outputs(outIncomeBox)
      .sendChangeTo(contracts.incomeAddress.getErgoAddress)
      .build()

    val prover = ctx.newProverBuilder().build()
    try {
      val signedTx = prover.sign(tx)
      logger.info(s"${incomes.size} Incomes with total value of $totalValue merged successfully")
      signedTx
    } catch {
      case e: Throwable =>
        logger.error(Utils.getStackTraceStr(e))
        logger.error(s"merge tx proving failed")
        throw proveException()
    }
  }

  def lockingTx(tokenBox: InputBox, ownerAddress: Address, configBox: InputBox, ctx: BlockchainContext): SignedTransaction ={
    val txB = ctx.newTxBuilder()
    val config = Config(configBox)
    val newStakeCount = tokenBox.getTokens.get(0).getValue

    val outConfig = boxes.getConfig(txB, configBox.getValue, configBox.getTokens.get(1).getValue, configBox.getTokens.get(2).getValue - 1,
      Array(config.checkpoint, config.minErgShare, config.minTokenShare, config.ticketCount+1, config.stakeCount+ newStakeCount, config.fee,
        config.minTicketValue, config.minBoxVal))
    val outTicket = boxes.getTicket(txB, config.minTicketValue, newStakeCount, ownerAddress.getErgoAddress,
      Array(config.checkpoint, config.checkpoint, config.fee, Configs.minBoxErg), configBox.getId)
    val name = "ErgoProfitSharing, Reserved Token"
    val description = s"Reserved token, defining $newStakeCount stake amount in the ErgoProfitSharing"
    val outReservedToken = txB.outBoxBuilder()
      .value(config.fee)
      .contract(new ErgoTreeContract(ownerAddress.getErgoAddress.script))
      .registers(ErgoValue.of(name.getBytes("utf-8")), ErgoValue.of(description.getBytes("utf-8")), ErgoValue.of("0".getBytes("utf-8")))
      .tokens(new ErgoToken(configBox.getId, 1))
      .build()

    val tx = txB.boxesToSpend(Seq(configBox, tokenBox).asJava)
      .fee(Configs.fee)
      .outputs(outConfig, outTicket, outReservedToken)
      .sendChangeTo(ownerAddress.getErgoAddress)
      .build()

    val prover = ctx.newProverBuilder()
      .withDLogSecret(Configs.user.secret)
      .build()
    try {
      val signedTx = prover.sign(tx)
      signedTx
    } catch {
      case e: Throwable =>
        logger.error(Utils.getStackTraceStr(e))
        logger.error(s"Locking tx proving failed")
        throw proveException()
    }
  }

  def distributionCreationTx(ctx: BlockchainContext, income: InputBox, configBox: InputBox): SignedTransaction = {
    val txB = ctx.newTxBuilder()
    val config = Config(configBox)
    val distFee = 2 * config.fee

    var outDistribution: OutBox = null
    var outIncome: OutBox = null

    var tokenShare: Long = 0
    var tokenRemainder: Long = 0
    var ergShare = (income.getValue - distFee) / config.stakeCount
    var ergRemainder = income.getValue - ((ergShare * config.stakeCount) + distFee)
    if (ergRemainder > 0 && ergRemainder < Configs.minBoxErg) {
      ergShare = (income.getValue - distFee - Configs.minBoxErg) / config.stakeCount
      ergRemainder = income.getValue - ((ergShare * config.stakeCount) + distFee)
    }
    if (income.getTokens.size() > 0) {
      tokenShare = income.getTokens.get(0).getValue / config.stakeCount
      tokenRemainder = income.getTokens.get(0).getValue - tokenShare * config.stakeCount
      if (tokenRemainder > 0 && ergRemainder == 0) {
        ergShare = (income.getValue - distFee - Configs.minBoxErg) / config.stakeCount
        ergRemainder = income.getValue - ((ergShare * config.stakeCount) + distFee)
      }
    }

    if (ergShare >= config.minErgShare && income.getTokens.size() == 0) {
      val distValue = (ergShare * config.stakeCount) + config.fee
      outDistribution = boxes.getDistribution(txB, distValue, config.checkpoint, config.fee, config.ticketCount, ergShare)
      outIncome = boxes.getIncome(txB, ergRemainder)
    }
    else if (income.getTokens.size() > 0 && (income.getValue == distFee || income.getValue >= distFee + Configs.minBoxErg)
      && tokenShare >= config.minTokenShare) {
      val distValue = (ergShare * config.stakeCount) + config.fee
      val tokenCount = tokenShare * config.stakeCount
      val tokenId = income.getTokens.get(0).getId.toString
      outDistribution = boxes.getDistribution(txB, distValue, config.checkpoint, config.fee, config.ticketCount, ergShare, tokenShare, tokenCount, tokenId)
      outIncome = boxes.getIncome(txB, ergRemainder, income.getTokens.get(0).getValue - tokenCount, tokenId)
    }
    else {
      logger.warn(s"stakeCount * minErgShare: ${config.stakeCount * config.minErgShare}, and distFee: $distFee, and income: ${income.getValue}")
      throw notCoveredException()
    }

    val outConfig = boxes.getConfig(txB, configBox.getValue, configBox.getTokens.get(1).getValue - 1, configBox.getTokens.get(2).getValue,
      Array(config.checkpoint + 1, config.minErgShare, config.minTokenShare, config.ticketCount, config.stakeCount, config.fee,
        config.minTicketValue, config.minBoxVal))

    var tx: UnsignedTransaction = null
    val txBuilder = txB.boxesToSpend(Seq(configBox, income).asJava)
      .fee(Configs.fee)
      .sendChangeTo(Configs.owner.address.getErgoAddress)
    if (outIncome.getValue > 0) tx = txBuilder.outputs(outConfig, outDistribution, outIncome).build()
    else tx = txBuilder.outputs(outConfig, outDistribution).build()

    val prover = ctx.newProverBuilder().build()
    try {
      val signedTx = prover.sign(tx)
      logger.info(s"Distribution creation tx built successfully")
      signedTx
    } catch {
      case e: Throwable =>
        logger.error(Utils.getStackTraceStr(e))
        logger.error(s"Distribution tx proving failed")
        throw proveException()
    }
  }

  def distributionPaymentTx(ctx: BlockchainContext, bankBox: InputBox, ticketBox: InputBox): SignedTransaction = {
    val txB = ctx.newTxBuilder()
    val bank = Distribution(bankBox)
    val ticket = Ticket(ticketBox)

    var bankOut: OutBox = null
    var payment: OutBox = null
    if (bank.tokenShare == 0) {
      bankOut = boxes.getDistribution(txB, bankBox.getValue - bank.ergShare * ticket.stakeCount, bank.checkpoint, bank.fee, bank.ticketCount - 1, bank.ergShare)
      payment = txB.outBoxBuilder()
        .value(bank.ergShare * ticket.stakeCount + ticket.minBoxVal)
        .contract(new ErgoTreeContract(ticket.recipientAddress.script))
        .build()
    }
    else {
      val tokenPayment = bank.tokenShare * ticket.stakeCount
      val tokenCount = bankBox.getTokens.get(1).getValue - tokenPayment
      val tokenId = bankBox.getTokens.get(1).getId.toString
      bankOut = boxes.getDistribution(txB, bankBox.getValue - bank.ergShare * ticket.stakeCount,
        bank.checkpoint, bank.fee, bank.ticketCount - 1, bank.ergShare, bank.tokenShare, tokenCount, tokenId)
      payment = txB.outBoxBuilder()
        .value(bank.ergShare * ticket.stakeCount + ticket.minBoxVal)
        .contract(new ErgoTreeContract(ticket.recipientAddress.script))
        .tokens(new ErgoToken(tokenId, tokenPayment))
        .build()
    }
    val ticketOut = boxes.getTicket(txB, ticketBox.getValue - ticket.fee - ticket.minBoxVal, ticket.stakeCount, ticket.recipientAddress,
      Array(ticket.InitialCheckpoint, ticket.checkpoint + 1, ticket.fee, ticket.minBoxVal), ticket.reservedTokenId)

    val tx = txB.boxesToSpend(Seq(bankBox, ticketBox).asJava)
      .fee(Configs.fee)
      .outputs(bankOut, ticketOut, payment)
      .sendChangeTo(Configs.owner.address.getErgoAddress)
      .build()

    val prover = ctx.newProverBuilder().build()
    try {
      val signedTx = prover.sign(tx)
      logger.info(s"Payment tx built successfully")
      signedTx
    } catch {
      case e: Throwable =>
        logger.error(Utils.getStackTraceStr(e))
        logger.error(s"Payment tx proving failed")
        throw proveException()
    }
  }

  def distributionRedeemTx(ctx: BlockchainContext, configBox: InputBox, bankBox: InputBox): SignedTransaction = {
    val txB = ctx.newTxBuilder()
    val r4 = configBox.getRegisters.get(0).getValue.asInstanceOf[Coll[Long]].toArray
    val fee = r4(5)
    val configOut: OutBox = boxes.getConfig(txB, configBox.getValue, configBox.getTokens.get(1).getValue + 1, configBox.getTokens.get(2).getValue, r4)

    val tx = txB.boxesToSpend(Seq(configBox, bankBox).asJava)
      .fee(fee)
      .outputs(configOut)
      .sendChangeTo(Configs.owner.address.getErgoAddress)
      .build()

    val prover = ctx.newProverBuilder().build()
    try {
      val signedTx = prover.sign(tx)
      logger.info(s"Payment tx built successfully")
      signedTx
    } catch {
      case e: Throwable =>
        logger.error(Utils.getStackTraceStr(e))
        logger.error(s"Payment tx proving failed")
        throw proveException()
    }
  }
}
