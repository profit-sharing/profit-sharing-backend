package ProfitSharing

import org.ergoplatform.appkit.{BlockchainContext, SignedTransaction}
import play.api.libs.json._
import scala.io.Source.fromFile

class testDataset(ctx: BlockchainContext) {

  private def readJsonFile(filePath: String): String = {
    val sourceFile = fromFile(filePath)
    val jsonString = sourceFile.getLines.mkString
    sourceFile.close()
    jsonString
  }

  val signedMergeTx: SignedTransaction = ctx.signedTxFromJson(readJsonFile("./test/Dataset/signedMergeTx.json"))
  val signedDistributionCreationTx: SignedTransaction = ctx.signedTxFromJson(readJsonFile("./test/Dataset/signedDistributionCreationTx.json"))
  val signedPaymentTx: SignedTransaction = ctx.signedTxFromJson(readJsonFile("./test/Dataset/signedPaymentTx.json"))
  val signedDistributionRedeemTx: SignedTransaction = ctx.signedTxFromJson(readJsonFile("./test/Dataset/signedDistributionRedeemTx.json"))
  val unconfirmedTxByAddress: JsValue = Json.parse(readJsonFile("./test/Dataset/unconfirmedTxByAddress.json"))
  val signedJsonTx: JsValue = Json.parse(readJsonFile("./test/Dataset/fakeTx.json"))
  val mempoolTransactions: JsValue = Json.parse(readJsonFile("./test/Dataset/mempoolTransactions.json"))
  val emptyResponse: JsValue = Json.parse(readJsonFile("./test/Dataset/emptyResponse.json"))
  val jsonToTxTest = (signedJsonTx, "5ce9b5444702e248b9ad40cc638542026468f6eee93336239b6b27db122442bb")
  val lastMempoolBoxTest = (unconfirmedTxByAddress, "5vSUZRZbdVbnk4sJWjg2uhL94VZWRg4iatK9VgMChufzUgdihgvhR8yWSUEJKszzV7Vmi6K8hCyKTNhUaiP8p5ko6YEU9yfHpjVuXdQ4i5p4cRCzch6ZiqWrNukYjv7Vs5jvBwqg5hcEJ8u1eerr537YLWUoxxi1M4vQxuaCihzPKMt8NDXP4WcbN6mfNxxLZeGBvsHVvVmina5THaECosCWozKJFBnscjhpr3AJsdaL8evXAvPfEjGhVMoTKXAb2ZGGRmR8g1eZshaHmgTg2imSiaoXU5eiF3HvBnDuawaCtt674ikZ3oZdekqswcVPGMwqqUKVsGY4QuFeQoGwRkMqEYTdV2UDMMsfrjrBYQYKUBFMwsQGMNBL1VoY78aotXzdeqJCBVKbQdD3ZZWvukhSe4xrz8tcF3PoxpysDLt89boMqZJtGEHTV9UBTBEac6sDyQP693qT3nKaErN8TCXrJBUmHPqKozAg9bwxTqMYkpmb9iVKLSoJxG7MjAj72SRbcqQfNCVTztSwN3cRxSrVtz4p87jNFbVtFzhPg7UqDwNFTaasySCqM", "174a90c5334a0bd093c59816477edc479ab513d00427d808bd15099090a8a1f6", "c8a5a73042cdabe4b9f494d38b2697b32fa1cefbadee7a6c1146b981da9a6cee")
  val boxInMempoolTest = (mempoolTransactions, "5vSUZRZbdVbnk4sJWjg2uhL94VZWRg4iatK9VgMChufzUgdihgvhR8yWSUEJKszzV7Vmi6K8hCyKTNhUaiP8p5ko6YEU9yfHpjVuXdQ4i5p4cRCzch6ZiqWrNukYjv7Vs5jvBwqg5hcEJ8u1eerr537YLWUoxxi1M4vQxuaCihzPKMt8NDXP4WcbN6mfNxxLZeGBvsHVvVmina5THaECosCWozKJFBnscjhpr3AJsdaL8evXAvPfEjGhVMoTKXAb2ZGGRmR8g1eZshaHmgTg2imSiaoXU5eiF3HvBnDuawaCtt674ikZ3oZdekqswcVPGMwqqUKVsGY4QuFeQoGwRkMqEYTdV2UDMMsfrjrBYQYKUBFMwsQGMNBL1VoY78aotXzdeqJCBVKbQdD3ZZWvukhSe4xrz8tcF3PoxpysDLt89boMqZJtGEHTV9UBTBEac6sDyQP693qT3nKaErN8TCXrJBUmHPqKozAg9bwxTqMYkpmb9iVKLSoJxG7MjAj72SRbcqQfNCVTztSwN3cRxSrVtz4p87jNFbVtFzhPg7UqDwNFTaasySCqM", "3bc8c33fadbb7d332cc68c89eb402b989f0d45cdbb6a6f9a294ba7638cb90e1d")
}
