package moe.pizza.auth.tasks

import moe.pizza.auth.interfaces.PilotGrader
import moe.pizza.auth.models.Pilot
import moe.pizza.crestapi.CrestApi
import moe.pizza.auth.adapters.EsiApi
import org.http4s.client.Client

import org.log4s.getLogger
import scala.util.Try

class Update(crest: CrestApi, esi: EsiApi, chain: PilotGrader)(
    implicit val client: Client) {

  private[this] val log = getLogger

  def updateUser(p: Pilot): Pilot = {
    val keys = p.getCrestTokens
    log.debug(s" updating $p.uid")
    val mainkey = keys.head
    Try {
      val charinfo = esi.characterInfo(mainkey.characterID.toInt).unsafePerformSync
      log.info(charinfo.toString)
      val refreshed = crest.refresh(mainkey.token).unsafePerformSync
      log.info(refreshed.toString)
      val pilotWithUpdatedMembership =
        p.copy(corporation = charinfo.corporation, alliance = charinfo.alliance.getOrElse(""))
      val gradedPilot = pilotWithUpdatedMembership.copy(
        accountStatus = chain.grade(pilotWithUpdatedMembership))
      gradedPilot
    }.getOrElse(p.copy(accountStatus = Pilot.Status.banned))
  }

}
