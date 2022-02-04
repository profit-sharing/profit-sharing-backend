package controllers

import ProfitSharing.Procedures
import network.Client
import play.api.Logger
import play.api.libs.circe.Circe
import play.api.mvc._

import javax.inject._
import io.circe.Json


@Singleton
class HomeController @Inject()(assets: Assets, client: Client, procedures: Procedures, val controllerComponents: ControllerComponents) extends BaseController
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
   * @return service initialization
   * You need to update the service config after this initialization
   */
  def serviceInitialization(): Action[AnyContent] = Action {
    var response: List[String] = null
    client.getClient.execute(ctx => {
      response = procedures.serviceInitialization(ctx)
    })
    var result: Json = null
    if(response.isEmpty)
      result = Json.fromFields(List(
        ("status", Json.fromString("Error"))
      ))
    else
      result = Json.fromFields(List(
        ("status", Json.fromString("Ok")),
        ("configNFT", Json.fromString(response.head)),
        ("distributionToken", Json.fromString(response(1))),
        ("lockingToken", Json.fromString(response(2)))
      ))
    Ok(result.toString()).as("application/json")
  }

  /**
   * @return service information
   */
  def info(): Action[AnyContent] = Action {

    val result = Json.fromFields(List(
      ("status", Json.fromString("ok")),
    ))
    Ok(result.toString()).as("application/json")
  }
}
