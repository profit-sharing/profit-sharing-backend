package services

import ProfitSharing.Procedures
import akka.actor.{Actor, ActorLogging}
import play.api.Logger

object JobsUtil {
  val merge = "merge"
}

class Jobs(procedures: Procedures)
  extends Actor with ActorLogging {
  private val logger: Logger = Logger(this.getClass)

  def receive = {
    case JobsUtil.merge =>
      logger.info(s"Merge Income Thread started")
      procedures.mergeIncomes()
      logger.info(s"Merge Income Thread finished working")
  }
}
