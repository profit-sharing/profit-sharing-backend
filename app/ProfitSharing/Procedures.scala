package ProfitSharing

import helpers.{Configs, Utils, failedTxException, proveException}
import network.Client
import org.ergoplatform.appkit.{BlockchainContext, ErgoToken, InputBox, SignedTransaction}
import play.api.Logger

import javax.inject.{Inject, Singleton}
import scala.collection.JavaConverters._

@Singleton
class Procedures@Inject()(client: Client ,boxes: Boxes, contracts: Contracts, utils: Utils) {
  private val logger: Logger = Logger(this.getClass)

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

  def mergeIncomes(): Unit = {
    client.getClient.execute(ctx => {
      val incomes = boxes.getIncomes
      for(incomeSet <- incomes) {
        val signedTx = mergeIncomesTx(incomeSet, ctx)
        var txId = ctx.sendTransaction(signedTx)
        if (txId == null) logger.error(s"Merge transaction sending failed")
        else txId = txId.replaceAll("\"", "")
        logger.info(s"Merge Transaction Sent with TxId: " + txId)
      }
    })
  }
}
