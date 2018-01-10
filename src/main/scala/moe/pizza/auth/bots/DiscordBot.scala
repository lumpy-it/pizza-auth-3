package moe.pizza.auth.bots

import java.awt.Color
import java.util.concurrent.TimeUnit

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
import org.joda.time.Seconds
import org.log4s.getLogger
import sx.blah.discord.api.{ClientBuilder, IDiscordClient}
import sx.blah.discord.api.events.{Event, IListener}
import sx.blah.discord.handle.impl.events.ReadyEvent
import sx.blah.discord.handle.obj.{IChannel, IGuild, IRole, IUser}
import sx.blah.discord.util.{EmbedBuilder, RequestBuffer}
import sx.blah.discord.util.RequestBuffer.{IRequest, IVoidRequest}

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

  var channel: Option[IChannel] = None

  private[this] val log = getLogger

  def connect(): Unit = {
    discordClient.getDispatcher.registerListener(this)
    discordClient.login()
  }

  override def handle(t: Event): Unit = {
    t match {
      case e: ReadyEvent =>
        guild = Some(discordClient.getGuildByID(config.guildId.toLong))
        log.info("DiscordBot: Saved Discord Guild Handle")
        channel = Some(guild.get.getChannelByID(config.channelId.toLong))
        log.info("DiscordBot: Saved Channel handle")
        log.info(channel.toString)
        roleLookup = createRoleLookup()
      case _ =>
    }
  }

  def createRoleLookup(): Map[String, IRole] = {
    val roles = guild.get.getRoles.asScala.toList
    config.roles.map((x) => (x._1 -> roles.find(_.getStringID() == x._2).get))
  }

  def getRoles(): List[(String, String)] = {
    guild match {
      case Some(g) =>
        g.getRoles().asScala.toList.map((role) => (role.getName(), role.getStringID()))
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

  def getDiscordId(p: Pilot): Option[Long] = {
    p.metadata.has("discordId") match {
      case true => Some(p.metadata.get("discordId").asText().toLong)
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

  def getUserById(id: Long): Option[IUser] = {
    guild.get.getUserByID(id) match {
      case null => None
      case user => Some(user)
    }
  }

  def update(p: Pilot): Option[String] = {
    guild match {
      case Some(g) =>
        getDiscordId(p)
          .flatMap(getUserById) match {
          case Some(user) if p.accountStatus == Pilot.Status.internal =>
            log.debug(p.uid)
            // sync roles
            val rolesThere = RequestBuffer.request(
              new IRequest[java.util.List[IRole]] {
                override def request() : java.util.List[IRole] = user.getRolesForGuild(g)
              }).get.asScala.toSet

            val groupsAndCorp = p.corporation :: p.authGroups

            val rolesNeeded = groupsAndCorp.flatMap(roleLookup.get).toSet

            val toBeDeleted = (rolesThere diff rolesNeeded).filter(_.getName() != "@everyone")
            val toBeAdded = rolesNeeded diff rolesThere
            log.debug(toBeDeleted.toString)
            log.debug(toBeAdded.toString)
            val changes = toBeDeleted.size + toBeAdded.size

            toBeDeleted.foreach((role) =>
              RequestBuffer.request(new IVoidRequest {
                override def doRequest() = user.removeRole(role)
              }))

            toBeAdded.foreach((role) =>
              RequestBuffer.request(new IVoidRequest {
                override def doRequest() = user.addRole(role)
              }))
            changes match {
              case 0 => None
              case _ => Some(p.uid)
            }
          case Some(user) =>
            // kick from discord and remove discord ID
            RequestBuffer.request(new IVoidRequest {
              override def doRequest() = guild.get.kickUser(user)
            })
            removeDiscordId(p)
            Some(p.uid)

          case None => None
        }



      case None =>
        log.error("rip")
        None
    }
  }

  def sendFleetAnnouncement(sender: String, fc: String, staging: String,
                            formup: String, departure: String,
                            shiptypes: String, message: String,
                            pingtype: String, broadcast: String): Boolean = {
    channel match {
      case Some(c) =>
        val color = pingtype match {
          case "Homedef" => new Color(0,200,0)
          case "Roaming" => new Color(0,0,255)
          case "Roaming Planung" => new Color(50,50,255)
          case "Stratop" => new Color(255, 127, 0)
          case "Stratop Planung" => new Color(255, 180, 50)
          case "CTA" => new Color(255,0,0)
          case "CTA Planung" => new Color(255,50,50)
          case _ => new Color(255,255,255)
        }

        val eb = new EmbedBuilder()
        eb.withColor(color).withTitle(pingtype).withDesc(message)
        eb.appendField("FC",fc,true).appendField("Shiptypes",shiptypes,true)
        eb.appendField("Staging",staging,true)
        eb.appendField("Formup / Abflug", formup + " / " + departure,true)

        val e = eb.build()

	val ping = broadcast match {
          case "" => ""
          case s => "@" + s
          case _ => ""
        }

        val m = c.sendMessage(s"fleet ping by $sender $ping",e,false)
        if (pingtype.endsWith(" Planung")) {
          RequestBuffer.request(new IVoidRequest {
            override def doRequest() = m.addReaction("✅")
          })
          RequestBuffer.request(new IVoidRequest {
            override def doRequest() = m.addReaction("❓")
          })
          RequestBuffer.request(new IVoidRequest {
            override def doRequest() = m.addReaction("❌")
          })
        }

        true
      case None =>
        false
    }

  }
}
