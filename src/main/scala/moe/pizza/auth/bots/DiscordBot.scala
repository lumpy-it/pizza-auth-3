package moe.pizza.auth.bots

import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.ObjectNode
import moe.pizza.crestapi.CrestApi.CallbackResponse
import moe.pizza.auth.config.ConfigFile.DiscordConfig
import moe.pizza.auth.models.Pilot
import org.http4s.client.Client
import org.http4s._
import org.http4s.circe._
import io.circe._
import io.circe.generic.JsonCodec
import io.circe.syntax._
import io.circe.generic.auto._
import moe.pizza.auth.interfaces.UserDatabase
import org.http4s.headers.Authorization
import org.http4s.MediaType._
import org.http4s.headers.`Content-Type`
import sx.blah.discord.api.ClientBuilder

/**
  * Created by Kalu on 20/02/2017.
  */
object DiscordBot {
  val API_URL = "https://discordapp.com/api/"
  val TOKEN_URL = Uri.uri("https://discordapp.com/api/oauth2/token")
  val AUTHORIZE_URL = Uri.uri("https://discordapp.com/api/oauth2/authorize")
  val SCOPES = List("identify", "guilds.join")
}

case class DiscordUser(id: String,
                      username: String,
                      discriminator: String,
                      avatar: Option[String],
                      bot: Option[Boolean],
                       mfa_enabled: Option[Boolean]
                    )

case class DiscordGuildMember(deaf: Boolean,
                              joined_at: String,
                              user: DiscordUser,
                              nick: Option[String],
                              roles: List[String],
                              mute: Boolean)

class DiscordBot(config: DiscordConfig, ud: UserDatabase)(implicit client: Client) {

  //val discordClient = new ClientBuilder().withToken(config.botToken).build()


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

  def getDiscordUserId(accessToken: String): Option[String] = {
    val token = OAuth2BearerToken(accessToken)
    val req = Request(
      uri = Uri.uri("https://discordapp.com/api/users/@me"),
      headers = Headers(new Authorization(token))
    )
    val task = client.fetchAs[DiscordUser](req)(jsonOf[DiscordUser])
    val res = task.unsafePerformSync
    Some(res.id)
  }

  case class AddMemberParams(access_token: String, nick: String)

  def addMember(userid: String, accessToken: String, p:Pilot): Option[String] = {
    val botToken = config.botToken

    val json = AddMemberParams(accessToken,p.characterName).asJson

    val req = Request(method = Method.PUT, uri = Uri.fromString(
      DiscordBot.API_URL + "guilds/" + config.guildId + "/members/" + userid).toOption.get)
      .putHeaders(Header("Authorization", s"Bot $botToken"))
      .withBody(json)

    val task = client.fetchAs[DiscordGuildMember](req)(jsonOf[DiscordGuildMember])
    val res = task.unsafePerformSync

    Some(res.user.id)
  }

  def saveDiscordId(discordId: String, p: Pilot): Option[String] = {
    val json2 = p.metadata match {
      case json: ObjectNode =>
        json.put("discordId", discordId)
      case json: JsonNode => json
    }
    ud.updateUser(p.copy(metadata = json2))
    Some(discordId)
  }

  def handleDiscordCode(req: Request, p: Pilot): String = {
    req.params.get("code") match {
      case Some(code) =>
        val token = getAccessToken(code)
        token
          .flatMap(getDiscordUserId)
          .flatMap((userid) => addMember(userid, token.get, p))
          .flatMap((userid) => saveDiscordId(userid, p))
        "success"
      case None =>
        "Error: " + req.params.getOrElse("error", "")
    }
  }

}