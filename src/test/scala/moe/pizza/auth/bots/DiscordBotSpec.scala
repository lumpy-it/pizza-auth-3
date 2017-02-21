package moe.pizza.auth.bots

import moe.pizza.auth.config.ConfigFile.{ConfigFile, DiscordConfig}
import moe.pizza.auth.interfaces.UserDatabase
import moe.pizza.auth.models.Pilot
import moe.pizza.crestapi.CrestApi.CallbackResponse
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar
import org.scalatest.{FlatSpec, MustMatchers}
import org.http4s.client.blaze.PooledHttp1Client

import scalaz.concurrent.Task

class DiscordBotSpec extends FlatSpec with MustMatchers with MockitoSugar {

  "DiscordBot" should "extract access token from response" in {
    val config = mock[ConfigFile]
    val discordConfig = mock[DiscordConfig]
    val ud = mock[UserDatabase]
    val discordAPI = mock[DiscordAPI]

    when(config.discord).thenReturn(discordConfig)

    when(discordAPI.token("code")).thenReturn(Task {
      CallbackResponse("tokenishere","code",0,Some("blabla"))
    })

    implicit val client = PooledHttp1Client()
    val discordBot = new DiscordBot(config.discord, ud, Some(discordAPI))

    discordBot.getAccessToken("code") must equal(Some("tokenishere"))
  }

  "DiscordBot" should "extract discordID from identify response" in {
    val config = mock[ConfigFile]
    val discordConfig = mock[DiscordConfig]
    val ud = mock[UserDatabase]
    val discordAPI = mock[DiscordAPI]

    when(config.discord).thenReturn(discordConfig)

    when(discordAPI.identify("token")).thenReturn(Task {
      DiscordAPI.DiscordUser("12345","","",None,None,None)
    })

    implicit val client = PooledHttp1Client()
    val discordBot = new DiscordBot(config.discord, ud, Some(discordAPI))

    discordBot.getUserId("token") must equal(Some("12345"))
  }

  "DiscordBot" should "add user to guild" in {
    val config = mock[ConfigFile]
    val discordConfig = mock[DiscordConfig]
    val ud = mock[UserDatabase]
    val discordAPI = mock[DiscordAPI]

    when(config.discord).thenReturn(discordConfig)

    when(discordAPI.addMember("12345","token","Bob McFace")).thenReturn(Task {
      DiscordAPI.DiscordGuildMember(
        false,
        "",
        DiscordAPI.DiscordUser("12345","","",None,None,None),
        Some("nickname"),
        List(),
        false
      )
    })

    implicit val client = PooledHttp1Client()
    val discordBot = new DiscordBot(config.discord, ud, Some(discordAPI))

    val p = new Pilot("bob_mcface",Pilot.Status.internal,null,null,"Bob McFace",null,null,List(),List(),List())

    discordBot.addMember("12345","token",p) must equal(Some("12345"))
  }

}
