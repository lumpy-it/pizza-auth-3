package moe.pizza.auth.tasks

import moe.pizza.auth.interfaces.PilotGrader
import moe.pizza.auth.models.Pilot
import moe.pizza.crestapi.CrestApi
import moe.pizza.eveapi._
import org.http4s.client.Client

import org.log4s.getLogger
import scala.util.Try

class Update(crest: CrestApi, eveapi: EVEAPI, chain: PilotGrader)(
    implicit val client: Client) {

  private[this] val log = getLogger

  def updateUser(p: Pilot): Pilot = {
    val keys = p.getCrestTokens
    log.debug(s" updating $p.uid")
    val mainkey = keys.head
    Try {
      val charinfo = eveapi.eve.CharacterInfo(mainkey.characterID).unsafePerformSync
      val refreshed = crest.refresh(mainkey.token).unsafePerformSync
      val corpAndAlliance = charinfo match {
        case Left(r) => (r.result.corporation, "")
        case Right(r) => (r.result.corporation, r.result.alliance)
      }
      val pilotWithUpdatedMembership =
        p.copy(corporation = corpAndAlliance._1, alliance = corpAndAlliance._2)
      val gradedPilot = pilotWithUpdatedMembership.copy(
        accountStatus = chain.grade(pilotWithUpdatedMembership))
      gradedPilot
    }.getOrElse(p.copy(accountStatus = Pilot.Status.banned))
  }

}
