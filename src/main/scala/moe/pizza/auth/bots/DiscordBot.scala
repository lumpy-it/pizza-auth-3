package moe.pizza.auth.bots

import moe.pizza.auth.config.ConfigFile.DiscordConfig
import moe.pizza.crestapi.CrestApi.CallbackResponse

import org.http4s.client.Client

import org.http4s._

import org.http4s.circe._
import io.circe.generic.auto._

/**
  * Created by Kalu on 20/02/2017.
  */
object DiscordBot {
  val TOKEN_URL = Uri.uri("https://discordapp.com/api/oauth2/token")
  val AUTHORIZE_URL = Uri.uri("https://discordapp.com/api/oauth2/authorize")
  val SCOPES = List("identify", "guilds.join")
}

class DiscordBot(config: DiscordConfig)(implicit client: Client) {

  def getAuthorisationUrl(): Uri = {
    DiscordBot.AUTHORIZE_URL
      .withQueryParam("client_id", config.clientId)
      .withQueryParam("redirect_uri", config.redirectUrl)
      .withQueryParam("response_type", "code")
      .withQueryParam("scope", DiscordBot.SCOPES.mkString(" "))
  }

  def getAccessToken(code: String): Option[String] = {
    val req = Request(
      method = Method.POST,
      uri = DiscordBot.TOKEN_URL
        .withQueryParam("grant_type", "authorization_code")
        .withQueryParam("code", code)
        .withQueryParam("redirect_uri", config.redirectUrl)
        .withQueryParam("client_id", config.clientId)
        .withQueryParam("client_secret", config.clientSecret))

    val task = client.fetchAs[CallbackResponse](req)(jsonOf[CallbackResponse])
    val res = task.unsafePerformSync

    Some(res.access_token)
  }

}