package controllers

import ProfitSharing.{Contracts, Procedures}
import helpers.Configs
import network.Client
import play.api.Logger
import play.api.libs.circe.Circe
import play.api.mvc._

//import scala.concurrent.{ExecutionContext, Future}
import javax.inject._
import io.circe.Json

//import scala.concurrent.ExecutionContext.Implicits.global

@Singleton
class HomeController @Inject()(assets: Assets, client: Client, procedures: Procedures, contracts: Contracts,
                               val controllerComponents: ControllerComponents) extends BaseController
  with Circe {
  private val logger: Logger = Logger(this.getClass)

  def index: Action[AnyContent] = {
    assets.at("index.html")
  }

  def assetOrDefault(resource: String): Action[AnyContent] = {
    if (resource.contains(".")) assets.at(resource) else index
  }

  def exception(e: Throwable): Result = {
    logger.warn(e.getMessage)
    BadRequest(s"""{"success": false, "message": "${e.getMessage}"}""").as("application/json")
  }

  /**
   * @return service initialization information
   * You need to update the service config after this initialization
   */
  def serviceInitialization(): Action[AnyContent] = Action {
    client.getClient.execute(ctx => {
//      Future{procedures.serviceInitialization(ctx)}
    })
    val result: Json = Json.fromFields(List(
      ("message", Json.fromString("Initialization started, don't forget to update the config after completion"))
    ))
    Ok(result.toString()).as("application/json")
  }

  /**
   * @return service information
   */
  def info(): Action[AnyContent] = Action {
    val result = Json.fromFields(List(
      ("ergoTrees", Json.fromFields(List(
        ("config", Json.fromString(contracts.configAddress.getErgoAddress.script.bytesHex)),
        ("ticket", Json.fromString(contracts.ticketAddress.getErgoAddress.script.bytesHex)),
        ("fee", Json.fromString(Configs.feeErgoTree))
      ))),
      ("tokens", Json.fromFields(List(
        ("configNFT", Json.fromString(Configs.token.configNFT)),
        ("distribution", Json.fromString(Configs.token.distribution)),
        ("locking", Json.fromString(Configs.token.locking)),
        ("staking", Json.fromString(Configs.token.staking))
      )))
    ))
    Ok(result.toString()).as("application/json")
  }
}
