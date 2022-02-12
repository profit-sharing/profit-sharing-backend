package ProfitSharing

import helpers.{Configs, Utils, proveException}
import org.ergoplatform.appkit.{Address, BlockchainContext, ErgoToken, ErgoValue, InputBox, SignedTransaction}
import org.ergoplatform.appkit.impl.ErgoTreeContract
import play.api.Logger
import special.collection.Coll

import scala.collection.JavaConverters._
import javax.inject.Inject

class Transactions@Inject()(boxes: Boxes, contracts: Contracts, utils: Utils) {
  private val logger: Logger = Logger(this.getClass)

  def tokenIssueTx(ctx: BlockchainContext, count: Long, inputs: Seq[InputBox], address: Address, name: String, description: String): SignedTransaction ={
    val txB = ctx.newTxBuilder()

    val totalValue: Long = inputs.map(item => item.getValue).reduce((a, b) => a + b)
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
    var signedTx: SignedTransaction = null
    try {
      signedTx = prover.sign(tx)
    } catch {
      case e: Throwable =>
        logger.error(utils.getStackTraceStr(e))
        logger.error(s"token $name issue tx proving failed")
        throw proveException()
    }
    val txId = ctx.sendTransaction(signedTx)
    if (txId == null) logger.error(s"Token Issue transaction sending failed")
    else logger.info(s"Token $name issued successfully with txId: $txId")
    signedTx
  }

  def mergeIncomesTx(incomes: Seq[InputBox], ctx: BlockchainContext): SignedTransaction = {
    val txB = ctx.newTxBuilder()

    val totalValue: Long = incomes.map(item => item.getValue).reduce((a, b) => a + b)
    val fee: Long = incomes.size * Configs.incomeMerge.boxSize * Configs.feePerByte
    logger.debug(s"Calculated fee for merge transaction is $fee and maximum available fee is ${Configs.incomeMerge.maxFee}")
    var outIncome = txB.outBoxBuilder()
      .value(totalValue - fee)
      .contract(contracts.income)

    if(incomes.head.getTokens.size() > 0) {
      val tokenId: String = incomes.head.getTokens.get(0).getId.toString
      val totalTokens: Long = incomes.map(item => item.getTokens.get(0).getValue).sum
      outIncome = outIncome.tokens(new ErgoToken(tokenId, totalTokens))
    }
    val outIncomeBox = outIncome.build()

    val tx = txB.boxesToSpend(incomes.asJava)
      .fee(fee)
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

  def lockingTx(tokenBox: InputBox, ownerAddress: Address, configBox: InputBox, ctx: BlockchainContext): SignedTransaction ={
    val txB = ctx.newTxBuilder()
    val r4 = configBox.getRegisters.get(0).getValue.asInstanceOf[Coll[Long]].toArray
    val checkpoint = r4(0)
    val minErgShare = r4(1)
    val minTokenShare = r4(2)
    val ticketCount = r4(3)
    val stakeCount = r4(4)
    val fee = r4(5)
    val minTicketValue = r4(6)
    val newStakeCount = tokenBox.getTokens.get(0).getValue

    val outConfig = boxes.getConfig(txB, configBox.getValue, configBox.getTokens.get(1).getValue, configBox.getTokens.get(2).getValue - 1,
      Array(checkpoint, minErgShare, minTokenShare, ticketCount+1, stakeCount+ newStakeCount, fee,
        minTicketValue, r4(7)))
    val outTicket = boxes.getTicket(txB, minTicketValue, newStakeCount, ownerAddress.getErgoAddress,
      Array(checkpoint, checkpoint, fee, Configs.minBoxErg), configBox.getId)
    val name = "ErgoProfitSharing, Reserved Token"
    val description = s"Reserved token, defining $newStakeCount stake amount in the ErgoProfitSharing"
    val outReservedToken = txB.outBoxBuilder()
      .value(fee)
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
    var signedTx: SignedTransaction = null
    try {
      signedTx = prover.sign(tx)
    } catch {
      case e: Throwable =>
        logger.error(utils.getStackTraceStr(e))
        logger.error(s"Locking tx proving failed")
        throw proveException()
    }
    logger.info("Staking tokens locked successfully")
    signedTx
  }
}
