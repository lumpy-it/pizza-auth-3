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
import sx.blah.discord.api.{ClientBuilder, IDiscordClient}
import sx.blah.discord.api.events.{Event, IListener}
import sx.blah.discord.handle.impl.events.ReadyEvent
import sx.blah.discord.handle.obj.{IGuild, IRole, IUser}

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

class DiscordBot(config: DiscordConfig,
                  ud: UserDatabase,
                  dapi: Option[DiscordAPI] = None,
                 dclient: Option[IDiscordClient] = None)
                (implicit client: Client) extends IListener[Event]{

  val discordAPI = dapi.getOrElse(new DiscordAPI(config))
  val discordClient = dclient.getOrElse(new ClientBuilder().withToken(config.botToken).build())

  var guild : Option[IGuild] = None
  var roleLookup : Map[String, IRole] = Map()

  def connect(): Unit = {
    discordClient.getDispatcher.registerListener(this)
    discordClient.login()
  }

  override def handle(t: Event): Unit = {
    t match {
      case e: ReadyEvent =>
        guild = Some(discordClient.getGuildByID(config.guildId))
        println("DiscordBot: Saved Discord Guild Handle")

        roleLookup = createRoleLookup()
      case _ =>
    }
  }

  def createRoleLookup(): Map[String, IRole] = {
    val roles = guild.get.getRoles.asScala.toList
    config.roles.map((x) => (x._1 -> roles.find(_.getID == x._2).get))
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
    p.accountStatus match {
      case Pilot.Status.internal =>
        discordAPI.addMember(userid, accessToken, p.characterName).unsafePerformSyncAttempt
          .toOption.map(_.user.id)
      case _ =>
        None
    }
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

  def removeDiscordId(p: Pilot): Unit = {
    p.metadata match {
      case json: ObjectNode if json.has("discordId") =>
        json.remove("discordId")
        ud.updateUser (p.copy (metadata = json) )
    }
  }

  def getDiscordId(p: Pilot): Option[String] = {
    p.metadata.has("discordId") match {
      case true => Some(p.metadata.get("discordId").asText())
      case false => None
    }
  }

  def handleDiscordCode(code: String, p: Pilot): Option[String] = {
    val token = getAccessToken(code)
    token
      .flatMap(getUserId)
      .flatMap((userid) => addMember(userid, token.get, p))
      .flatMap((userid) => saveDiscordId(userid, p))
  }

  def getUserById(id: String): Option[IUser] = {
    guild.get.getUserByID(id) match {
      case null => None
      case user => Some(user)
    }
  }

  def update(p: Pilot): Unit = {
    guild match {
      case Some(g) =>
        getDiscordId(p)
          .flatMap(getUserById) match {
          case Some(user) if p.accountStatus == Pilot.Status.internal =>
            // sync roles
            val rolesThere = user.getRolesForGuild(g).asScala.toSet

            val groupsAndCorp = p.corporation :: p.authGroups

            val rolesNeeded = groupsAndCorp.flatMap(roleLookup.get).toSet

            val toBeDeleted = rolesThere diff rolesNeeded
            val toBeAdded = rolesNeeded diff rolesThere

            toBeDeleted.filter(_.getName() != "@everyone").foreach(user.removeRole)
            toBeAdded.foreach(user.addRole)
          case Some(user) =>
            // kick from discord and remove discord ID
            guild.get.kickUser(user)
            removeDiscordId(p)

          case None =>
        }



      case None =>
        println("rip")
    }
  }
}
