package controllers

import network.Client
import play.api.Logger
import play.api.libs.circe.Circe
import play.api.mvc._
import javax.inject._
import io.circe.Json


@Singleton
class HomeController @Inject()(assets: Assets, client: Client, val controllerComponents: ControllerComponents) extends BaseController
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
   * @return service information
   */
  def info(): Action[AnyContent] = Action {

    val result = Json.fromFields(List(
      ("status", Json.fromString("ok")),
    ))
    Ok(result.toString()).as("application/json")
  }
}
