package services

import ProfitSharing.Procedures
import akka.actor.{Actor, ActorLogging}
import network.Client
import play.api.Logger

object JobsUtil {
  val merge = "merge"
  val distribution = "dist"
  val payment = "payment"
}

class Jobs(procedures: Procedures, client: Client)
  extends Actor with ActorLogging {
  private val logger: Logger = Logger(this.getClass)

  def receive = {
    case JobsUtil.merge =>
      logger.info(s"Merge Income Thread started")
      client.getClient.execute(procedures.mergeIncomes(_))
      logger.info(s"Merge Income Thread finished working")
    case JobsUtil.distribution =>
      logger.info(s"Distribution Creation Thread started")
      client.getClient.execute(procedures.distributionCreation(_))
      logger.info(s"Distribution Creation Thread finished working")
    case JobsUtil.payment =>
      logger.info(s"Payment Thread started")
      client.getClient.execute(procedures.payment(_))
      logger.info(s"Payment Thread finished working")
  }
}
