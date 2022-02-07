package network

import helpers.{Configs, connectionException, requestException}
import play.api.libs.json._
import javax.inject.Singleton
import play.api.Logger


@Singleton
class Explorer() {
  private val baseUrlV0 = s"${Configs.explorerUrl}/api/v0"
  private val baseUrlV1 = s"${Configs.explorerUrl}/api/v1"
  private val tx = s"$baseUrlV1/transactions"
  private val unconfirmedTx = s"$baseUrlV0/transactions/unconfirmed"
  private val unspentBoxesByTokenId = s"$baseUrlV1/boxes/unspent/byTokenId"
  private val allBoxesByTokenId = s"$baseUrlV1/boxes/byTokenId"
  private val boxesP1 = s"$tx/boxes"
  private val mempoolTransactions = s"$baseUrlV1/mempool/transactions/byAddress"
  private val boxSearch = s"$baseUrlV1/boxes/search"
  private val logger: Logger = Logger(this.getClass)

  /**
   * @param address address to search in mempool
   * @return mempool transactions belonging to the address
   */
  def getTxsInMempoolByAddress(address: String): JsValue = try {
    Json.parse(Request.httpGet(s"$mempoolTransactions/$address").toString())
  } catch {
    case e: requestException =>
      logger.warn(e.getMessage)
      throw connectionException()
    case e: Throwable =>
      logger.error(e.getMessage)
      throw connectionException()
  }

  /**
   * @param txId transaction id
   * @return transaction if it is unconfirmed
   */
  def getUnconfirmedTx(txId: String): JsValue = try {
    Json.parse(Request.httpGet(s"$unconfirmedTx/$txId").toString())
  } catch {
    case _: Throwable =>
      JsNull
  }

  /**
   * @param txId transaction id
   * @return transaction if it is confirmed (mined)
   */
  def getConfirmedTx(txId: String): JsValue = try {
    Json.parse(Request.httpGet(s"$tx/$txId").toString())
  } catch {
    case _: Throwable =>
      JsNull
  }

  /**
   * @param txId transaction id
   * @return -1 if tx does not exist, 0 if it is unconfirmed, otherwise, confirmation num
   */
  def getConfNum(txId: String): Int = try {
    val unc = getUnconfirmedTx(txId)
    if (unc != JsNull) 0
    else {
      val conf = getConfirmedTx(txId)
      if (conf != JsNull) ((conf \ "summary").as[JsValue] \ "confirmationsCount").as[Int]
      else -1
    }
  } catch {
    case e: connectionException => throw e
    case e: Throwable =>
      logger.error(e.getMessage)
      throw connectionException()
  }

  /**
   * @param tokenId token id to search for
   * @return list of unspent boxes containing the token
   */
  def getUnspentTokenBoxes(tokenId: String, offset: Int, limit: Int): JsValue = try {
    Json.parse(Request.httpGet(s"$unspentBoxesByTokenId/$tokenId?offset=$offset&limit=$limit").toString())
  } catch {
    case e: requestException =>
      logger.warn(e.getMessage)
      throw connectionException()
    case e: Throwable =>
      logger.error(e.getMessage)
      throw connectionException()
  }

  /**
   * @param tokenId token id to search for
   * @return list of all boxes(spent and unspent) containing the token
   */
  def getAllTokenBoxes(tokenId: String, offset: Int, limit: Int): JsValue = try {
    Json.parse(Request.httpGet(s"$allBoxesByTokenId/$tokenId?offset=$offset&limit=$limit").toString())
  } catch {
    case e: requestException =>
      logger.warn(e.getMessage)
      throw connectionException()
    case e: Throwable =>
      logger.error(e.getMessage)
      throw connectionException()
  }

  /**
   * @param boxId required box id
   * @return a box with that id in the network
   */
  def getUnspentBoxByID(boxId: String): JsValue = try {
    Json.parse(Request.httpGet(s"$boxesP1/$boxId").toString())
  } catch {
    case e: requestException =>
      logger.warn(e.getMessage)
      throw connectionException()
    case e: Throwable =>
      logger.error(e.getMessage)
      throw connectionException()
  }

  /**
   * @param address address to search for
   * @return list of unconfirmed transactions belonging to the address
   */
  def getUnconfirmedTxByAddress(address: String): JsValue = try {
    Json.parse(Request.httpGet(s"$unconfirmedTx/byAddress/$address/?offset=0&limit=100").toString())
  } catch {
    case e: requestException =>
      logger.warn(e.getMessage)
      throw connectionException()
    case e: Throwable =>
      logger.error(e.getMessage)
      throw connectionException()
  }
}
