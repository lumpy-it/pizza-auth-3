package moe.pizza.auth.webapp

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import moe.pizza.auth.config.ConfigFile.ConfigFile
import moe.pizza.auth.graphdb.EveMapDb
import moe.pizza.auth.interfaces.{BroadcastService, PilotGrader, UserDatabase}
import BroadcastService._
import moe.pizza.auth.adapters.EsiApi
import moe.pizza.auth.models.Pilot
import moe.pizza.auth.plugins.LocationManager
import moe.pizza.auth.tasks.Update
import moe.pizza.auth.webapp.Types.{HydratedSession, Session, Session2, SignupData}
import moe.pizza.crestapi.CrestApi
import org.http4s.{HttpService, _}
import org.http4s.dsl.{Root, _}
import org.http4s.server._
import org.http4s.server.staticcontent.ResourceService
import org.http4s.server.syntax.ServiceOps
import org.joda.time.DateTime
import play.twirl.api.Html
import moe.pizza.eveapi._
import org.http4s.client.Client
import org.http4s.client.blaze.PooledHttp1Client

import scala.concurrent.ExecutionContext.Implicits.global
import org.http4s.twirl._

import scala.concurrent.Future
import scala.util.Try
import scalaz._
import Scalaz._
import scala.util.{Success => TSuccess}
import scala.util.{Failure => TFailure}
import scala.concurrent.duration._
import scala.concurrent.Await
import moe.pizza.crestapi.character.location.Types.Location

import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import moe.pizza.auth.bots.DiscordBot
import moe.pizza.auth.webapp.oauth.OAuthResource
import moe.pizza.auth.webapp.rest.{RestKeyMiddleware, RestResource}
import org.http4s.circe._

import scalaz.\/-

import org.log4s._

object Webapp {
  val PILOT = "pilot"
  val defaultCrestScopes =
List("publicData", "esi-location.read_location.v1", "esi-location.read_ship_type.v1", "esi-skills.read_skills.v1", "esi-skills.read_skillqueue.v1", "esi-clones.read_clones.v1", "esi-fleets.read_fleet.v1", "esi-fleets.write_fleet.v1")
}

class Webapp(fullconfig: ConfigFile,
             graders: PilotGrader,
             portnumber: Int = 9021,
             ud: UserDatabase,
             crestapi: Option[CrestApi] = None,
             eve: Option[EsiApi] = None,
             apiClient: Option[Client] = None,
             mapper: Option[EveMapDb] = None,
             updater: Option[Update] = None,
             connectedDiscordBot: Option[DiscordBot] = None,
             broadcasters: List[BroadcastService] =
               List.empty[BroadcastService]) {

  private[this] val log = getLogger
  log.info("created Webapp")

  val config = fullconfig.crest
  val groupconfig = fullconfig.auth.groups


  implicit val client = apiClient.getOrElse(PooledHttp1Client())

  val crest = crestapi.getOrElse(
    new CrestApi(baseurl = config.loginUrl,
                 cresturl = config.crestUrl,
                 config.clientID,
                 config.secretKey,
                 config.redirectUrl))
  val esi = eve.getOrElse(new EsiApi())
  val map = mapper.getOrElse {
    // make the graph database in the webapp for the MVP version
    val map = new EveMapDb("internal-map")
    // initialise the database
    map.provisionIfRequired()
    map
  }

  val update = updater.getOrElse(new Update(crest, esi, graders))

  val discordBot = connectedDiscordBot.orElse(
    fullconfig.discord match {
      case Some(dConfig) => Some(new DiscordBot(dConfig, ud))
      case None => None
      case _ => None
    })

  // used for serializing JSON responses, for now
  val OM = new ObjectMapper()
  OM.registerModules(DefaultScalaModule)

  case class PlayerWithLocation(name: String,
                                system: String,
                                station: Option[String])

  def staticrouter =
    staticcontent.resourceService(
      ResourceService.Config("/static/static/", "/static/"))

  import Utils._

  implicit class RichHydratedSession(hs: HydratedSession) {
    def toNormalSession = new Session(hs.alerts, hs.redirect)

    def updatePilot: HydratedSession = {
      hs.pilot match {
        case Some(p) =>
          hs.copy(pilot = ud.getUser(p.uid))
        case None =>
          hs
      }
    }
  }

  def getJabberServer(u: Pilot): String = u.accountStatus match {
    case Pilot.Status.internal => fullconfig.auth.domain
    case Pilot.Status.ally => s"allies.${fullconfig.auth.domain}"
    case Pilot.Status.ineligible => s"public.${fullconfig.auth.domain}"
    case _ => "none"
  }

  def dynamicWebRouter = HttpService {
    case req @ GET -> Root => {
      req.getSession match {
        case Some(s) =>
          s.pilot match {
            case Some(pilot) =>
          val discordId = pilot.metadata.get("discordId") match {
            case null => None
            case s => Some(s.asText)
          }
              Ok(
                templates.html.base(
                  "LUMPY Auth",
                  templates.html.main(
                    pilot,
                    discordId,
                    discordBot.get.getAuthorizationUrl().toString()),
                  req.getSession.map(_.toNormalSession),
                  req.getSession.flatMap(_.pilot)
                )
              ).attachSessionifDefined(
                req.getSession.map(_.copy(alerts = List())))
            case None =>
              Ok(
                templates.html.base(
                  "LUMPY Auth",
                  templates.html.landing(),
                  req.getSession.map(_.toNormalSession),
                  None
                )).attachSessionifDefined(
                req.getSession.map(_.copy(alerts = List())))
          }
        case None =>
          InternalServerError(
            templates.html.base(
              "LUMPY Auth",
              Html("An error occurred with the session handler"),
              None,
              None))
      }
    }
    case req @ GET -> Root / "login" => {
      Uri
        .fromString(crest.redirect("login", Webapp.defaultCrestScopes)) match {
        case \/-(url) => TemporaryRedirect(url)
        case _ => InternalServerError("unable to construct url")
      }
    }
    case req @ GET -> Root / "signup" => {
      Uri.fromString(crest.redirect("signup", Webapp.defaultCrestScopes)) match {
        case \/-(url) => TemporaryRedirect(url)
        case _ => InternalServerError("unable to construct url")
      }
    }
    case req @ GET -> Root / "logout" => {
      TemporaryRedirect(Uri(path = "/")).clearSession()
    }

    case req @ GET -> Root / "signup" / "confirm" => {
      log.debug("following route for GET signup/confirm")
      req.getSession
        .flatMap(_.signupData)
        .map { signup =>
          val charinfo =
            esi.characterInfo(signup.verify.CharacterID.toInt)
          val pilot = charinfo.map { ci =>
            val refresh = crest.refresh(signup.refresh).unsafePerformSync
            new Pilot(
              Utils.sanitizeUserName(ci.character),
              Pilot.Status.unclassified,
              ci.alliance.getOrElse(""),
              ci.corporation,
              ci.character,
              "none@none",
              Pilot.OM.createObjectNode(),
              List.empty[String],
              List("%d:%s".format(signup.verify.CharacterID.toInt,
                                  refresh.refresh_token.get)),
              List.empty[String]
            )
          }
          Try {
            pilot.unsafePerformSync
          } match {
            case TSuccess(p) =>
              log.debug(s"pilot has been read out of XML API: ${p}")
              // grade the pilot
              val result = graders.grade(p)
              log.debug(s"pilot was graded as ${result}")
              val gradedpilot = p.withNewAccountStatus(result)
              log.debug(s"pilot has been graded: ${gradedpilot}")
              // mark it as ineligible if it fell through
              val gradedpilot2 =
                if (gradedpilot.accountStatus == Pilot.Status.unclassified) {
                  log.debug("marking pilot as Ineligible")
                  gradedpilot.copy(accountStatus = Pilot.Status.ineligible)
                } else {
                  log.debug("not marking pilot as Ineligible")
                  gradedpilot
                }
              log.debug("trying to redirect back to signup confirm")
              Ok(
                templates.html.base("LUMPY Auth",
                                    templates.html.signup(gradedpilot2),
                                    req.getSession.map(_.toNormalSession),
                                    None))
            case TFailure(f) =>
              log.debug(
                s"failure when grading pilot, redirecting back ${f.toString}")
              val newsession = req.flash(
                Alerts.warning,
                "Unable to unpack CREST response, please try again later")
              TemporaryRedirect(Uri(path = "/"))
                .attachSessionifDefined(newsession)
          }
        }
        .getOrElse(TemporaryRedirect(Uri(path = "/")))
    }

    case req @ POST -> Root / "signup" / "confirm" => {
      log.debug("signup confirm called")
      req.getSession
        .flatMap(_.signupData)
        .map { signup =>
          val charinfo = esi.characterInfo(signup.verify.CharacterID.toInt)
          val pilot = charinfo.map { ci =>
            val refresh = crest.refresh(signup.refresh).unsafePerformSync
            new Pilot(
              Utils.sanitizeUserName(ci.character),
              Pilot.Status.unclassified,
              ci.alliance.getOrElse(""),
              ci.corporation,
              ci.character,
              "none@none",
              Pilot.OM.createObjectNode(),
              List.empty[String],
              List("%d:%s".format(signup.verify.CharacterID.toInt,
                                  refresh.refresh_token.get)),
              List.empty[String]
            )
          }
          Try {
            pilot.unsafePerformSync
          } match {
            case TSuccess(p) =>
              log.debug(s"pilot has been read out of XML API: ${p}")
              // grade the pilot
              val result = graders.grade(p)
              log.debug(s"pilot was graded as ${result}")
              val gradedpilot = p.withNewAccountStatus(result)
              log.debug(s"pilot has been graded: ${gradedpilot}")
              // mark it as ineligible if it fell through
              val gradedpilot2 =
                if (gradedpilot.accountStatus == Pilot.Status.unclassified) {
                  log.debug("marking pilot as Ineligible")
                  gradedpilot.copy(accountStatus = Pilot.Status.ineligible)
                } else {
                  log.debug("not marking pilot as Ineligible")
                  gradedpilot
                }
              req.decode[UrlForm] { data =>
                val newemail = data.getFirstOrElse("email", "none")
                val pilotwithemail = gradedpilot2.copy(email = newemail)
                val password = data.getFirst("password").get
                log.debug(s"signing up ${pilotwithemail.uid}")
                log.debug(OM.writeValueAsString(pilotwithemail))
                val res = ud.addUser(pilotwithemail, password)
                log.debug(s"$res")
                SeeOther(Uri(path = "/")).attachSessionifDefined(
                  req
                    .flash(Alerts.success,
                           s"Successfully created and signed in as ${p.uid}")
                    .map(_.copy(pilot = Some(p)))
                )
              }
            case TFailure(f) =>
              log.debug(
                s"failure when grading pilot, redirecting back ${f.toString}")
              val newsession = req.flash(
                Alerts.warning,
                "Unable to unpack CREST response, please try again later")
              SeeOther(Uri(path = "/")).attachSessionifDefined(newsession)
          }
        }
        .getOrElse(SeeOther(Uri(path = "/")).attachSessionifDefined(req.flash(
          Alerts.danger,
          "Unable to create your user, please contact the administrator.")))
    }

    case req @ GET -> Root / "groups" => {
      req.getSession.map(_.updatePilot).flatMap(_.pilot) match {
        case Some(p) =>
          val groups = fullconfig.auth.groups
          Ok(
            templates.html.base(
              "LUMPY Auth",
              templates.html.groups(p, groups.closed),
              req.getSession.map(_.toNormalSession),
              req.getSession.flatMap(_.pilot))).attachSessionifDefined(
            req.getSession.map(_.copy(alerts = List())))
        case None =>
          TemporaryRedirect(Uri(path = "/"))
      }
    }

    case req @ GET -> Root / "groups" / "admin" => {
      req.getSession.map(_.updatePilot).flatMap(_.pilot) match {
        case Some(p) =>
          p.getGroups contains "admin" match {
            case true =>
              // get all pending applications
              val applications = ud
                .getAllUsers()
                .filter(_.authGroups.exists(_.endsWith("-pending")))
                .flatMap { pilot =>
                  pilot.authGroups.filter(_.endsWith("-pending")).map {
                    group =>
                      (pilot.uid, group.stripSuffix("-pending"))
                  }
                }
                .toList
              // render it
              Ok(
                templates.html.base("LUMPY Auth",
                                    templates.html.groupadmin(applications),
                                    req.getSession.map(_.toNormalSession),
                                    req.getSession.flatMap(_.pilot)))
                .attachSessionifDefined(
                  req.getSession.map(_.copy(alerts = List())))
            case false =>
              TemporaryRedirect(Uri(path = "/")).attachSessionifDefined(
                req.flash(
                  Alerts.warning,
                  "You must be in the admin group to access that resource"))
          }
        case None =>
          TemporaryRedirect(Uri(path = "/"))
      }
    }

    case req @ GET -> Root / "groups" / "admin" / "approve" / user / group => {
      val goback = TemporaryRedirect(Uri(path = "/groups/admin"))
      req.getSession.map(_.updatePilot).flatMap(_.pilot) match {
        case Some(p) =>
          p.getGroups contains "admin" match {
            case true =>
              // get the user
              ud.getUser(user) match {
                case Some(target)
                    if target.authGroups.contains(s"${group}-pending") =>
                  val result = ud.updateUser(
                    target.copy(
                      authGroups = target.authGroups
                        .filter(_ != s"${group}-pending")
                        .+:(group)))
                  result match {
                    case true =>
                      goback.attachSessionifDefined(
                        req.flash(Alerts.success,
                                  s"Accepted $user into $group"))
                    case false =>
                      goback.attachSessionifDefined(
                        req.flash(Alerts.danger,
                                  s"Unable to accept $user into $group"))
                  }
                case Some(target) =>
                  goback.attachSessionifDefined(
                    req.flash(
                      Alerts.warning,
                      s"$user did not apply to $group, or has already been accepted/denied"))
                case None =>
                  goback.attachSessionifDefined(
                    req.flash(Alerts.warning, "Can't find that user"))
              }
            case false =>
              TemporaryRedirect(Uri(path = "/")).attachSessionifDefined(
                req.flash(
                  Alerts.warning,
                  "You must be in the admin group to access that resource"))
          }
        case None =>
          TemporaryRedirect(Uri(path = "/"))
      }
    }

    case req @ GET -> Root / "groups" / "admin" / "deny" / user / group => {
      val goback = TemporaryRedirect(Uri(path = "/groups/admin"))
      req.getSession.map(_.updatePilot).flatMap(_.pilot) match {
        case Some(p) =>
          p.getGroups contains "admin" match {
            case true =>
              // get the user
              ud.getUser(user) match {
                case Some(target)
                    if target.authGroups.contains(s"${group}-pending") =>
                  val result = ud.updateUser(
                    target.copy(authGroups =
                      target.authGroups.filter(_ != s"${group}-pending")))
                  result match {
                    case true =>
                      goback.attachSessionifDefined(
                        req.flash(Alerts.success,
                                  s"Denied $user membership of $group"))
                    case false =>
                      goback.attachSessionifDefined(
                        req.flash(
                          Alerts.danger,
                          s"Unable to deny $user membership of $group"))
                  }
                case Some(target) =>
                  goback.attachSessionifDefined(
                    req.flash(
                      Alerts.warning,
                      s"$user did not apply to $group, or has already been accepted/denied"))
                case None =>
                  goback.attachSessionifDefined(
                    req.flash(Alerts.warning, "Can't find that user"))
              }
            case false =>
              TemporaryRedirect(Uri(path = "/")).attachSessionifDefined(
                req.flash(
                  Alerts.warning,
                  "You must be in the admin group to access that resource"))
          }
        case None =>
          TemporaryRedirect(Uri(path = "/"))
      }
    }

    case req @ GET -> Root / "ping" => {
      req.getSession.map(_.updatePilot).flatMap(_.pilot) match {
        case Some(p) =>
          p.getGroups contains "ping" match {
            case true =>
              Ok(
                templates.html.base("LUMPY Auth",
                                    templates.html.ping(p),
                                    req.getSession.map(_.toNormalSession),
                                    req.getSession.flatMap(_.pilot)))
                .attachSessionifDefined(
                  req.getSession.map(_.copy(alerts = List())))
            case false =>
              TemporaryRedirect(Uri(path = "/")).attachSessionifDefined(
                req.flash(
                  Alerts.warning,
                  "You must be in the ping group to access that resource"))
          }
        case None =>
          TemporaryRedirect(Uri(path = "/"))
      }
    }

    case req @ POST -> Root / "ping" / "fleet" => {
      req.getSession.map(_.updatePilot).flatMap(_.pilot) match {
        case Some(p) =>
          p.getGroups contains "ping" match {
            case true =>
              req.decode[UrlForm] { form =>
                val fc = form.getFirstOrElse("fc","tba")
                val staging = form.getFirstOrElse("staging", "Staging")
                val formup = form.getFirstOrElse("formup","jetzt")
                val departure = form.getFirstOrElse("departure","bald")
                val shiptypes = form.getFirstOrElse("shiptypes","tba")
                val message = form.getFirstOrElse("message","")
                val pingtype = form.getFirstOrElse("type","Roaming")
                val broadcast = form.getFirstOrElse("broadcast","@everyone")

                val result = discordBot.get.sendFleetAnnouncement(
                  p.uid, fc,staging,formup,departure,
                  shiptypes,message,pingtype,broadcast
                )
                result match {
                  case true =>
                    SeeOther(Uri(path = "/ping")).attachSessionifDefined(
                      req.flash(Alerts.info, s"Ping sent."))
                  case false =>
                    SeeOther(Uri(path = "/ping")).attachSessionifDefined(
                      req.flash(Alerts.danger, s"Error sending ping"))
                }
              }
            case false =>
              SeeOther(Uri(path = "/")).attachSessionifDefined(
                req.flash(
                  Alerts.warning,
                  "You must be in the ping group to access that resource"))
          }
        case None =>
          SeeOther(Uri(path = "/"))
      }
    }

    case req @ GET -> Root / "locations" => {
      req.getSession.map(_.updatePilot).flatMap(_.pilot) match {
        case Some(p) =>
          p.getGroups contains "ping" match {
            case true =>
              val locations = LocationManager
                .locateUsers(crest)(ud.getUsers("accountStatus=Internal"))
                .flatten
                .map { kv =>
                  Try {
                    (kv._1, kv._2, kv._3.unsafePerformSyncFor(2 seconds))
                  }
                }
              log.debug(locations.toString)
              val res = locations
                .map(_.toOption)
                .flatten
                .filter { x =>
                  x._3.solarSystem.isDefined
                }
                .map { x =>
                  PlayerWithLocation(x._2,
                                     x._3.solarSystem.get.name,
                                     x._3.station.map {
                                       _.name
                                     })
                }
              Ok(OM.writeValueAsString(res))
            case false =>
              TemporaryRedirect(Uri(path = "/")).attachSessionifDefined(
                req.flash(
                  Alerts.warning,
                  "You must be in the ping group to access that resource"))
          }
        case None =>
          TemporaryRedirect(Uri(path = "/"))
      }
    }

    case req @ POST -> Root / "ping" / "global" => {
      req.getSession.map(_.updatePilot).flatMap(_.pilot) match {
        case Some(p) =>
          p.getGroups contains "ping" match {
            case true =>
              req.decode[UrlForm] { form =>
                val flags = List("internal", "ally", "public")
                val groups = flags.flatMap { g =>
                  form.getFirst(g).map(_ => g.capitalize)
                }
                log.debug(s"sending a ping to groups ${groups}")
                val users = groups.map(name =>
                  (name, ud.getUsers(s"accountStatus=${name}")))
                val message = form.getFirstOrElse(
                  "message",
                  "This message is left intentionally blank.")
                val total = users.map { kv =>
                  val (label, us) = kv
                  val templatedMessage =
                    templates.txt.broadcast(message,
                                            label,
                                            p.uid,
                                            DateTime.now())
                  val totals = broadcasters.map { b =>
                    b.sendAnnouncement(templatedMessage.toString(), p.uid, us)
                  }
                  Await.result(Future.sequence(totals), 2 seconds)
                }.map {
                  _.sum
                }.sum
                SeeOther(Uri(path = "/ping")).attachSessionifDefined(
                  req.flash(Alerts.info, s"Message sent to ${total} users."))
              }
            case false =>
              SeeOther(Uri(path = "/")).attachSessionifDefined(
                req.flash(
                  Alerts.warning,
                  "You must be in the ping group to access that resource"))
          }
        case None =>
          SeeOther(Uri(path = "/"))
      }
    }

    case req @ POST -> Root / "ping" / "group" => {
      req.getSession.map(_.updatePilot).flatMap(_.pilot) match {
        case Some(p) =>
          p.getGroups contains "ping" match {
            case true =>
              req.decode[UrlForm] { form =>
                val group = form.getFirstOrElse("group", "none")
                log.debug(s"sending a ping to group ${group}")
                val users = ud.getGroupUsers(group)
                val message = form.getFirstOrElse(
                  "message",
                  "This message is left intentionally blank.")
                val templatedMessage = templates.txt
                  .broadcast(message, group, p.uid, DateTime.now())
                val totals =
                  ud.sendGroupAnnouncement(broadcasters,
                                           templatedMessage.toString(),
                                           p.uid,
                                           users)
                val total =
                  Await.result(Future.sequence(totals), 2 seconds).sum
                SeeOther(Uri(path = "/ping")).attachSessionifDefined(
                  req.flash(
                    Alerts.info,
                    s"Message sent to ${total} users in group ${group}."))
              }
            case false =>
              SeeOther(Uri(path = "/")).attachSessionifDefined(
                req.flash(
                  Alerts.warning,
                  "You must be in the ping group to access that resource"))
          }
        case None =>
          SeeOther(Uri(path = "/"))
      }
    }

    case req @ GET -> Root / "ping" / "complete" => {
      val data = req.params.getOrElse("term", "")
      val allGroups = groupconfig.closed ++ groupconfig.open
      val allCorpsAndAlliances =
        ud.getAllUsers().flatMap(p => List(p.corporation, p.alliance))
      Ok(
        (allGroups ++ allCorpsAndAlliances).distinct
          .filter(_.toLowerCase.startsWith(data.toLowerCase))
          .sortBy(_.length)
          .asJson)
    }

    case req @ GET -> Root / "groups" / "apply" / group => {
      val goback = TemporaryRedirect(Uri(path = "/groups"))
      req.sessionResponse { (s: HydratedSession, p: Pilot) =>
        // TODO extend for public users
        group match {
          case g if groupconfig.open.contains(g) =>
            ud.updateUser(p.copy(authGroups = p.authGroups :+ group)) match {
              case true =>
                goback.attachSessionifDefined(
                  req
                    .flash(Alerts.success, s"Joined $group")
                    .map(_.updatePilot))
              case false =>
                goback.attachSessionifDefined(
                  req.flash(Alerts.warning, s"Unable to join $group"))
            }
          case g if groupconfig.closed.contains(g) =>
            ud.updateUser(
              p.copy(authGroups = p.authGroups :+ s"$group-pending")) match {
              case true =>
                goback.attachSessionifDefined(
                  req
                    .flash(Alerts.success, s"Applied to $group")
                    .map(_.updatePilot))
              case false =>
                goback.attachSessionifDefined(
                  req.flash(Alerts.warning, s"Unable to join $group"))
            }
          case _ =>
            goback.attachSessionifDefined(
              req.flash(Alerts.warning,
                        s"Unable to find a group named $group"))
        }
      }
    }

    case req @ GET -> Root / "groups" / "remove" / group => {
      val goback = TemporaryRedirect(Uri(path = "/groups"))
      req.sessionResponse { (s: HydratedSession, p: Pilot) =>
        // TODO extend for public users
        group match {
          case g if p.authGroups.contains(g) =>
            ud.updateUser(p.copy(authGroups = p.authGroups.filter(_ != group))) match {
              case true =>
                goback.attachSessionifDefined(
                  req.flash(Alerts.success, s"Left $group").map(_.updatePilot))
              case false =>
                goback.attachSessionifDefined(
                  req.flash(Alerts.warning, s"Unable to join $group"))
            }
          case _ =>
            goback.attachSessionifDefined(
              req.flash(Alerts.warning,
                        s"You are not in a group named $group"))
        }
      }
    }

    case req @ GET -> Root / "account" => {
      req.getSession.map(_.updatePilot).flatMap(_.pilot) match {
        case Some(p) =>
          val verifies = p.getCrestTokens.map(crestToken => {
            crest.refresh(crestToken.token).attemptRun match {
              case \/-(callbackResult) =>
                val verify = crest.verify(callbackResult.access_token).unsafePerformSync
                (crestToken, Some(verify))
              case -\/(_) =>
                (crestToken, None)
            }
          })

          val (validKeys, invalidKeys) = verifies.partition(x => x._2.isDefined)

          val validKeysWithoutMain = validKeys.filter(x => x._2.get.CharacterName != p.characterName)

          val discordId = p.metadata.get("discordId") match {
            case null => None
            case s => Some(s.asText)
          }
          Ok(
            templates.html.base(
              "LUMPY Auth",
              templates.html.account(
                p,
                validKeysWithoutMain,
                invalidKeys,
                discordId,
                discordBot.get.getAuthorizationUrl().toString()),
              req.getSession.map(_.toNormalSession),
              req.getSession.flatMap(_.pilot))).attachSessionifDefined(
            req.getSession.map(_.copy(alerts = List())))
        case None =>
          TemporaryRedirect(Uri(path = "/"))
      }
    }

    case req @ POST -> Root / "account" / "update" / "email" => {
      val goback = SeeOther(Uri(path = "/account"))
      req.getSession.map(_.updatePilot).flatMap(_.pilot) match {
        case Some(p) =>
          req.decode[UrlForm] { data =>
            val newemail = data.getFirstOrElse("email", "none")
            ud.updateUser(p.copy(email = newemail)) match {
              case true =>
                goback.attachSessionifDefined(
                  req
                    .flash(Alerts.success, s"Successfully updated email.")
                    .map(_.updatePilot))
              case false =>
                goback.attachSessionifDefined(
                  req.flash(Alerts.warning, s"Error updateing email."))
            }
          }
        case None =>
          TemporaryRedirect(Uri(path = "/"))
      }
    }

    case req @ POST -> Root / "account" / "update" / "password" => {
      val goback = SeeOther(Uri(path = "/account"))
      req.getSession.map(_.updatePilot).flatMap(_.pilot) match {
        case Some(p) =>
          req.decode[UrlForm] { data =>
            val newpassword = data.getFirstOrElse("password", "none")
            ud.setPassword(p, newpassword) match {
              case true =>
                goback.attachSessionifDefined(
                  req
                    .flash(Alerts.success, s"Successfully changed password.")
                    .map(_.updatePilot))
              case false =>
                goback.attachSessionifDefined(
                  req.flash(Alerts.warning, s"Error changing password."))
            }
          }
        case None =>
          TemporaryRedirect(Uri(path = "/"))
      }
    }

    case req @ GET -> Root / "account" / "characters" / "add" => {
      req.getSession match {
          case Some(s) =>
            s.pilot match {
              case Some(pilot) =>
                Uri.fromString(crest.redirect("add", Webapp.defaultCrestScopes)) match {
                  case \/-(url) => TemporaryRedirect(url)
                  case _ => InternalServerError("unable to construct url")
                }
              case None =>
                InternalServerError(
                  templates.html.base(
                    "LUMPY Auth",
                    Html("An error occurred with the session handler"),
                    None,
                    None))
            }
          case None =>
            InternalServerError(
              templates.html.base(
                "LUMPY Auth",
                Html("An error occurred with the session handler"),
                None,
                None))
      }
    }

    case req @ POST -> Root / "account" / "characters" / "remove" => {
      val goback = SeeOther(Uri(path = "/account"))
      req.getSession.map(_.updatePilot).flatMap(_.pilot) match {
        case Some(p) =>
          req.decode[UrlForm] { data =>
            val crestToken = data.getFirstOrElse("crestToken", "none")
            ud.updateUser(p.copy(crestTokens = p.crestTokens.filter(_ != crestToken))) match {
              case true =>
                goback.attachSessionifDefined(
                  req.flash(Alerts.success, s"Successfully removed character.").map(_.updatePilot))
              case false =>
                goback.attachSessionifDefined(
                  req.flash(Alerts.success, s"Error removing character."))
            }
          }
        case None =>
          TemporaryRedirect(Uri(path = "/"))
      }
    }

    case req @ GET -> Root / "discord" / "callback" => {
      val goback = SeeOther(Uri(path = "/"))
      req.getSession.map(_.updatePilot).flatMap(_.pilot) match {
        case Some(p) =>
          req.params.get("code") match {
            case Some(c) =>
              val result = discordBot.get.handleDiscordCode(c,p).get
              goback.attachSessionifDefined(
                req.flash(Alerts.success, s"Successfully added discord user $result")
                  .map(_.updatePilot))
            case None =>
              val error = req.params("error")
              goback.attachSessionifDefined(
                req.flash(Alerts.danger, s"Error adding discord user: $error"))
          }
        case None =>
          BadRequest()
      }
    }

    case req @ GET -> root / "discord" => {
      req.getSession.map(_.updatePilot).flatMap(_.pilot) match {
        case Some(p) if p.getGroups contains "admin" =>
          val roles = discordBot.get.getRoles()
          Ok(
            templates.html.base(
              "LUMPY Auth",
              templates.html.discordroles(roles),
              req.getSession.map(_.toNormalSession),
              req.getSession.flatMap(_.pilot))).attachSessionifDefined(
            req.getSession.map(_.copy(alerts = List())))
        case _ =>
          NotFound()
      }
    }

    case req @ GET -> Root / "update" / username =>
      log.info(s"update route called for ${username}")
      req.getSession.flatMap(_.pilot) match {
        case Some(p) =>
          p.getGroups contains "admin" match {
            case true =>
              ud.getUser(username) match {
                case Some(u) =>
                  val updatedpilot = update.updateUser(u)
                  ud.updateUser(updatedpilot)
                  Ok(OM.writeValueAsString(updatedpilot))
                case None =>
                  NotFound()
              }
            case false =>
              TemporaryRedirect(Uri(path = "/")).attachSessionifDefined(
                req.flash(
                  Alerts.warning,
                  "You must be in the admin group to access that resource"))
          }
        case None =>
          TemporaryRedirect(Uri(path = "/"))
      }

    case req @ GET -> Root / "discord" / "update" / username =>
      log.info(s"discord update route called for ${username}")
      req.getSession.flatMap(_.pilot) match {
        case Some(p) =>
          p.getGroups contains "admin" match {
            case true =>
              ud.getUser(username) match {
                case Some(u) =>
                  discordBot.get.update(u)
                  Ok()
                case None =>
                  NotFound()
              }
            case false =>
              TemporaryRedirect(Uri(path = "/")).attachSessionifDefined(
                req.flash(
                  Alerts.warning,
                  "You must be in the admin group to access that resource"))
          }
        case None =>
          TemporaryRedirect(Uri(path = "/"))
      }

    case req @ GET -> Root / "callback" => {
      val code = req.params("code")
      val state = req.params("state")
      val callbacktask = crest.callback(code)
      val callbackresults = callbacktask.unsafePerformSync
      val verify = crest.verify(callbackresults.access_token).unsafePerformSync
      state match {
        case "signup" =>
          log.debug(s"signup route in callback for ${verify.CharacterName}")
          val newSession = req.getSession.map(x =>
            x.copy(signupData =
              Some(new SignupData(verify, callbackresults.refresh_token.get))))
          TemporaryRedirect(Uri(path = "/signup/confirm"))
            .attachSessionifDefined(
              newSession
            )

        case "add" =>
          val goback = SeeOther(Uri(path = "/account"))
          req.getSession.map(_.updatePilot).flatMap(_.pilot) match {
            case Some(p) =>
              val crestToken = new Pilot.CrestToken(verify.CharacterID,callbackresults.refresh_token.get).toString
              ud.updateUser(p.copy(crestTokens = p.crestTokens :+ crestToken)) match {
                case true =>
                  goback.attachSessionifDefined(
                    req.flash(Alerts.success, "Successfully added character %s.".format(verify.CharacterName)).map(_.updatePilot))
                case false =>
                  goback.attachSessionifDefined(
                    req.flash(Alerts.warning, s"Error adding character."))
              }
            case None =>
              TemporaryRedirect(Uri(path = "/"))
          }
        case "login" =>
          val uid = Utils.sanitizeUserName(verify.CharacterName)
          val pilot = ud.getUser(uid)
          val session = pilot match {
            case Some(p) =>
              req
                .flash(Alerts.success,
                       "Thanks for logging in %s".format(verify.CharacterName))
                .map(_.copy(pilot = Some(p), redirect=None))
            case None =>
              req.flash(
                Alerts.warning,
                "Unable to find a user associated with that EVE character, please sign up or use another character")
          }
          req.getSession.flatMap(_.redirect) match {
            case Some(uristring) =>
              Uri.fromString(uristring).toOption match {
                case Some(uri) =>
                  TemporaryRedirect(uri).attachSessionifDefined(session)
                case None =>
                  TemporaryRedirect(Uri(path = "/")).attachSessionifDefined(session)
              }

            case None =>
              TemporaryRedirect(Uri(path = "/")).attachSessionifDefined(session)
          }
        case _ =>
          TemporaryRedirect(Uri(path = "/"))
      }
    }

    case req @ GET -> root / "auth" => {
      req.getSession.map(_.updatePilot).flatMap(_.pilot) match {
        case Some(p) if p.accountStatus == Pilot.Status.internal =>
          Ok().attachSessionifDefined(
            req.getSession.map(_.copy(alerts = List())))
        case _ =>
          Forbidden("Only for internal members")
      }
    }
  }

  def adminRouter = HttpService {
    case req @ GET -> Root / "updateall" =>
      log.info(s"update route called for all users")
      val updated = ud
        .getAllUsers()
        .filter(_.uid != "pingbot")
        .map {
          update.updateUser
        }
        .filter { p =>
          ud.updateUser(p)
        }
      Ok(OM.writeValueAsString(updated))

    case req @ GET -> Root / "discord" / "updateall" =>
      val users = ud.getAllUsers()
      val updated = users.flatMap((p) => {
          discordBot.get.update(p)
        })

      Ok(OM.writeValueAsString(updated))
  }

  val secretKey = "SECRET IS GOING HERE"
  //UUID.randomUUID().toString
  val sessions = new SessionManager(secretKey, ud)
  val restapi = new RestResource(ud, broadcasters)
  val restMiddleware = new RestKeyMiddleware(fullconfig.auth.restkeys)

  val oauthServer = new OAuthResource(portnumber, ud, fullconfig.auth.applications)

  val needsSession = dynamicWebRouter orElse oauthServer.resource

  def router =
    staticrouter orElse sessions(needsSession) orElse restMiddleware(restapi.resource)

}
