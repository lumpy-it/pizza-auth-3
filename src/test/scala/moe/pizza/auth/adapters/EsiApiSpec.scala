package moe.pizza.auth.adapters

import org.http4s.client.blaze.PooledHttp1Client

import org.scalatest.{FlatSpec, MustMatchers}

/**
  * Created by andi on 18/02/16.
  */
class EsiApiSpec extends FlatSpec with MustMatchers {

  "getting character info" should "query esi" in {
    implicit val client = PooledHttp1Client()
    val api = new EsiApi()

    val t = api.characterInfo(545542175)
    val info: CharacterInfo = t.run

    info.corporation must equal("Know your Role")
    info.alliance must equal(Some("League of Unaligned Master Pilots"))
  }

  "getting character info" should "cope with no alliance" in {
    implicit val client = PooledHttp1Client()
    val api = new EsiApi()

    val t = api.characterInfo(998240117)
    val info: CharacterInfo = t.run

    info.corporation must equal("New Generation Industries")
    info.alliance must equal(None)
  }
}
