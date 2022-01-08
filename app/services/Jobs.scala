package services

import akka.actor.{Actor, ActorLogging}
import play.api.Logger

object JobsUtil {
  val create = "create"
}

class Jobs()
  extends Actor with ActorLogging {
  private val logger: Logger = Logger(this.getClass)

  def receive = {
    case JobsUtil.create =>
      logger.info(s"Creation Thread started")
  }
}
