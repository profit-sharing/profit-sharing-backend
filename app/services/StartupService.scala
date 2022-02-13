package services

import ProfitSharing.Procedures
import akka.actor.{ActorRef, ActorSystem, Props}
import helpers.Configs
import network.Client
import play.api.Logger

import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

@Singleton
class StartupService @Inject()(node: Client, system: ActorSystem, procedures: Procedures)
                              (implicit ec: ExecutionContext) {

  private val logger: Logger = Logger(this.getClass)

  logger.info("App started!")
  node.setClient()

  val jobs: ActorRef = system.actorOf(Props(new Jobs(procedures, node)), "scheduler")

  system.scheduler.scheduleAtFixedRate(
    initialDelay = 2.seconds,
    interval = Configs.timeInterval.incomeMerge.seconds,
    receiver = jobs,
    message = JobsUtil.merge
  )

  system.scheduler.scheduleAtFixedRate(
    initialDelay = 2.seconds,
    interval = Configs.timeInterval.distribution.seconds,
    receiver = jobs,
    message = JobsUtil.distribution
  )
}
