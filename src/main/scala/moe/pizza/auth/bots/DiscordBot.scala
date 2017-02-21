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
import sx.blah.discord.api.ClientBuilder
import sx.blah.discord.api.events.{Event, IListener}
import sx.blah.discord.handle.impl.events.ReadyEvent
import sx.blah.discord.handle.obj.IGuild

import scalaz.concurrent.Task
import scala.collection.JavaConverters._

/**
  * Created by Kalu on 20/02/2017.
  */
object DiscordAPI {
  val API_URL = "https://discordapp.com/api/"
  val TOKEN_URL = Uri.uri("https://discordapp.com/api/oauth2/token")
  val AUTHORIZE_URL = Uri.uri("https://discordapp.com/api/oauth2/authorize")
  val SCOPES = List("identify", "guilds.join")

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

  case class AddMemberParams(access_token: String, nick: String)
}

class DiscordAPI(config: DiscordConfig)(implicit client:Client) {

  def getAuthorizationUrl(): Uri = {
    DiscordAPI.AUTHORIZE_URL
      .withQueryParam("client_id", config.clientId)
      .withQueryParam("redirect_uri", config.redirectUrl)
      .withQueryParam("response_type", "code")
      .withQueryParam("scope", DiscordAPI.SCOPES.mkString(" "))
  }

  def token(code: String): Task[CallbackResponse] = {
    val req = Request(
      method = Method.POST,
      uri = DiscordAPI.TOKEN_URL
        .withQueryParam("grant_type", "authorization_code")
        .withQueryParam("code", code)
        .withQueryParam("redirect_uri", config.redirectUrl)
        .withQueryParam("client_id", config.clientId)
        .withQueryParam("client_secret", config.clientSecret))
    client.expect[CallbackResponse](req)(jsonOf[CallbackResponse])
  }

  def identify(accessToken: String): Task[DiscordAPI.DiscordUser] = {
    val token = OAuth2BearerToken(accessToken)
    val req = Request(
      uri = Uri.fromString(DiscordAPI.API_URL + "users/@me").toOption.get,
      headers = Headers(new Authorization(token))
    )
    client.expect[DiscordAPI.DiscordUser](req)(jsonOf[DiscordAPI.DiscordUser])
  }

  def addMember(userid: String, accessToken: String, nick: String): Task[DiscordAPI.DiscordGuildMember] = {
    val botToken = config.botToken

    val json = DiscordAPI.AddMemberParams(accessToken,nick).asJson

    val req = Request(method = Method.PUT, uri = Uri.fromString(
      DiscordAPI.API_URL + "guilds/" + config.guildId + "/members/" + userid).toOption.get)
      .putHeaders(Header("Authorization", s"Bot $botToken"))
      .withBody(json)

    client.expect[DiscordAPI.DiscordGuildMember](req)(jsonOf[DiscordAPI.DiscordGuildMember])
  }
}

class DiscordBot(
                  config: DiscordConfig,
                  ud: UserDatabase,
                  dapi: Option[DiscordAPI] = None)
                (implicit client: Client) extends IListener[Event]{

  val discordClient = new ClientBuilder().withToken(config.botToken).build()
  val discordAPI = dapi.getOrElse(new DiscordAPI(config))
  var guild : Option[IGuild] = None

  def connect(): Unit = {
    discordClient.getDispatcher.registerListener(this)
    discordClient.login()
  }

  override def handle(t: Event): Unit = {
    t match {
      case e: ReadyEvent =>
        guild = Some(discordClient.getGuildByID(config.guildId))
        println("DiscordBot: Saved Discord Guild Handle")
      case _ =>
    }
  }

  def getRoles(): List[(String, String)] = {
    guild match {
      case Some(g) =>
        g.getRoles().asScala.toList.map((role) => (role.getName(), role.getID()))
      case None =>
        List()
    }
  }

  def getAuthorizationUrl(): Uri = {
    discordAPI.getAuthorizationUrl()
  }

  def getAccessToken(code: String): Option[String] = {
    discordAPI.token(code).unsafePerformSyncAttempt.toOption.map(_.access_token)
  }

  def getUserId(token: String): Option[String] = {
    discordAPI.identify(token).unsafePerformSyncAttempt.toOption.map(_.id)
  }

  def addMember(userid: String, accessToken: String, p:Pilot): Option[String] = {
    discordAPI.addMember(userid, accessToken, p.characterName).unsafePerformSyncAttempt
      .toOption.map(_.user.id)
  }

  def saveDiscordId(discordId: String, p: Pilot): Option[String] = {
    p.metadata match {
      case json: ObjectNode =>
        val json2 = json.put ("discordId", discordId)
        ud.updateUser (p.copy (metadata = json2) )
        Some (discordId)
      case _ =>
       None
    }
  }

  def handleDiscordCode(code: String, p: Pilot): Option[String] = {
    val token = getAccessToken(code)
    token
      .flatMap(getUserId)
      .flatMap((userid) => addMember(userid, token.get, p))
      .flatMap((userid) => saveDiscordId(userid, p))
  }
}