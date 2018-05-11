package moe.pizza.auth.adapters

import io.circe._
import io.circe.generic.JsonCodec
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._

import org.http4s._
import org.http4s.client.Client
import org.http4s.circe._

import scalaz.concurrent.Task

case class CharacterInfo(
    character: String,
    corporation: String,
    alliance: Option[String]
)

case class CharacterResponse(
    alliance_id: Option[Int],
    ancestry_id: Option[Int],
    birthday: String,
    bloodline_id: Int,
    corporation_id: Int,
    description: String,
    faction_id: Option[Int],
    gender: String,
    name: String,
    race_id: Int,
    security_status: Double
)

case class CorporationResponse(
    alliance_id: Option[Int],
    ceo_id: Int,
    creator_id: Int,
    date_founded: String,
    description: String,
    faction_id: Option[Int],
    home_station_id: Option[Int],
    member_count: Int,
    name: String,
    shares: Option[Int],
    tax_rate: Float,
    ticker: String,
    url: Option[String]
)

case class AllianceResponse(
    creator_corporation_id: Int,
    creator_id: Int,
    date_founded: String,
    executor_corporation_id: Option[Int],
    faction_id: Option[Int],
    name: String,
    ticker: String
)

class EsiApi()(implicit val client: Client) {
    val charBase = Uri.uri("https://esi.evetech.net/v4/characters/")
    val corpBase = Uri.uri("https://esi.evetech.net/v4/corporations/")
    val alliBase = Uri.uri("https://esi.evetech.net/v3/alliances/")

    def getCorpName(corporation_id: Int): Task[String] = {
        val corpUri = corpBase / corporation_id.toString / ""
        val corpReq = new Request(uri = corpUri)
        client.expect[CorporationResponse](corpReq)(jsonOf[CorporationResponse]).map(_.name)
    }
    
    def getAllianceName(alliance_id: Int): Task[String] = {
        val alliUri = alliBase / alliance_id.toString / ""
        val alliReq = new Request(uri = alliUri)
        client.expect[AllianceResponse](alliReq)(jsonOf[AllianceResponse]).map(_.name)
    }

    def characterInfo(characterId: Int): Task[CharacterInfo] = {
        val charUri = charBase / characterId.toString / ""
        val charReq = new Request(uri = charUri)
        val charResp = client.expect[CharacterResponse](charReq)(jsonOf[CharacterResponse])
        charResp.flatMap(resp => {
            resp.alliance_id match {
                case Some(x) =>
                    getCorpName(resp.corporation_id).flatMap(corpName =>
                        getAllianceName(x).map(alliName => CharacterInfo(resp.name, corpName, Some(alliName))))
                case None =>
                    getCorpName(resp.corporation_id).map(corpName =>
                        CharacterInfo(resp.name, corpName, None))
            }
        })
    }
}