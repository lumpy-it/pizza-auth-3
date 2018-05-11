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
import sx.blah.discord.api.IDiscordClient
import sx.blah.discord.handle.obj.{IGuild, IRole, IUser}

import scalaz.concurrent.Task
import scala.collection.JavaConverters._

class DiscordBotSpec extends FlatSpec with MustMatchers with MockitoSugar {

  "DiscordBot" should "extract access token from response" in {
    val config = mock[ConfigFile]
    val discordConfig = mock[DiscordConfig]
    val ud = mock[UserDatabase]
    val discordAPI = mock[DiscordAPI]
    val discordClient = mock[IDiscordClient]

    when(config.discord).thenReturn(Some(discordConfig))

    when(discordAPI.token("code")).thenReturn(Task {
      CallbackResponse("tokenishere","code",0,Some("blabla"))
    })

    implicit val client = PooledHttp1Client()
    val discordBot = new DiscordBot(config.discord.get, ud, Some(discordAPI), Some(discordClient))

    discordBot.getAccessToken("code") must equal(Some("tokenishere"))
  }

  "DiscordBot" should "extract discordID from identify response" in {
    val config = mock[ConfigFile]
    val discordConfig = mock[DiscordConfig]
    val ud = mock[UserDatabase]
    val discordAPI = mock[DiscordAPI]
    val discordClient = mock[IDiscordClient]

    when(config.discord).thenReturn(Some(discordConfig))

    when(discordAPI.identify("token")).thenReturn(Task {
      DiscordAPI.DiscordUser("12345","","",None,None,None)
    })

    implicit val client = PooledHttp1Client()
    val discordBot = new DiscordBot(config.discord.get, ud, Some(discordAPI), Some(discordClient))

    discordBot.getUserId("token") must equal(Some("12345"))
  }

  "DiscordBot" should "add user to guild" in {
    val config = mock[ConfigFile]
    val discordConfig = mock[DiscordConfig]
    val ud = mock[UserDatabase]
    val discordAPI = mock[DiscordAPI]
    val discordClient = mock[IDiscordClient]

    when(config.discord).thenReturn(Some(discordConfig))

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
    val discordBot = new DiscordBot(config.discord.get, ud, Some(discordAPI), Some(discordClient))

    val p = new Pilot("bob_mcface",Pilot.Status.internal,null,null,"Bob McFace",null,null,List(),List(),List())

    discordBot.addMember("12345","token",p) must equal(Some("12345"))

    val p2 = p.copy(accountStatus = Pilot.Status.ineligible)
    discordBot.addMember("12345","token",p2) must equal(None)
  }

  "DiscordBot" should "save discordId" in {
    val config = mock[ConfigFile]
    val discordConfig = mock[DiscordConfig]
    val ud = mock[UserDatabase]
    val discordAPI = mock[DiscordAPI]
    val discordClient = mock[IDiscordClient]

    when(config.discord).thenReturn(Some(discordConfig))

    implicit val client = PooledHttp1Client()
    val discordBot = new DiscordBot(config.discord.get, ud, Some(discordAPI), Some(discordClient))

    val p = new Pilot("bob_mcface",
      null,null,null,null,null,Pilot.OM.createObjectNode(),List(),List(),List())

    discordBot.saveDiscordId("12345",p) must equal(Some("12345"))

    verify(ud).updateUser(p.copy(metadata = Pilot.OM.readTree("{\"discordId\":\"12345\"}")))
  }

  "DiscordBot" should "return discordId" in {
    val config = mock[ConfigFile]
    val discordConfig = mock[DiscordConfig]
    val ud = mock[UserDatabase]
    val discordAPI = mock[DiscordAPI]
    val discordClient = mock[IDiscordClient]

    when(config.discord).thenReturn(Some(discordConfig))

    implicit val client = PooledHttp1Client()
    val discordBot = new DiscordBot(config.discord.get, ud, Some(discordAPI), Some(discordClient))

    val p1 = new Pilot("bob_mcface",
      null,null,null,null,null,Pilot.OM.createObjectNode(),List(),List(),List())
    val p2 = new Pilot("bob_mcface",
      null,null,null,null,null,Pilot.OM.readTree("{\"discordId\":\"12345\"}"),List(),List(),List())

    discordBot.getDiscordId(p1) must equal(None)
    discordBot.getDiscordId(p2) must equal(Some("12345"))
  }

  "DiscordBot" should "remove discordId" in {
    val config = mock[ConfigFile]
    val discordConfig = mock[DiscordConfig]
    val ud = mock[UserDatabase]
    val discordAPI = mock[DiscordAPI]
    val discordClient = mock[IDiscordClient]

    when(config.discord).thenReturn(Some(discordConfig))

    implicit val client = PooledHttp1Client()
    val discordBot = new DiscordBot(config.discord.get, ud, Some(discordAPI), Some(discordClient))

    val p = new Pilot("bob_mcface",
      null,null,null,null,null,Pilot.OM.readTree("{\"discordId\":\"12345\"}"),List(),List(),List())

    discordBot.removeDiscordId(p)
    verify(ud).updateUser(p.copy(metadata = Pilot.OM.createObjectNode()))
  }

  "DiscordBot" should "return roles" in {
    val config = mock[ConfigFile]
    val discordConfig = mock[DiscordConfig]
    val ud = mock[UserDatabase]
    val discordAPI = mock[DiscordAPI]
    val discordClient = mock[IDiscordClient]

    when(config.discord).thenReturn(Some(discordConfig))
    implicit val client = PooledHttp1Client()
    val discordBot = new DiscordBot(config.discord.get, ud, Some(discordAPI), Some(discordClient))

    val guild = mock[IGuild]
    val role = mock[IRole]
    when(role.getStringID()).thenReturn("12345")
    when(role.getName()).thenReturn("testRole")

    discordBot.guild = Some(guild)
    when(guild.getRoles()).thenReturn(List(role).asJava)

    discordBot.getRoles must equal(List(("testRole","12345")))
  }

  "DiscordBot" should "create role lookup table" in {
    val config = mock[ConfigFile]
    val discordConfig = mock[DiscordConfig]
    val ud = mock[UserDatabase]
    val discordAPI = mock[DiscordAPI]
    val discordClient = mock[IDiscordClient]

    when(config.discord).thenReturn(Some(discordConfig))
    when(discordConfig.roles).thenReturn(Map("testAuthGroup" -> "12345"))

    implicit val client = PooledHttp1Client()
    val discordBot = new DiscordBot(config.discord.get, ud, Some(discordAPI), Some(discordClient))

    val guild = mock[IGuild]
    val role = mock[IRole]
    when(role.getStringID()).thenReturn("12345")
    when(role.getName()).thenReturn("testRole")

    discordBot.guild = Some(guild)
    when(guild.getRoles()).thenReturn(List(role).asJava)

    discordBot.createRoleLookup must equal(Map("testAuthGroup" -> role))
  }

  "DiscordBot" should "update users" in {
    val config = mock[ConfigFile]
    val discordConfig = mock[DiscordConfig]
    val ud = mock[UserDatabase]
    val discordAPI = mock[DiscordAPI]
    val discordClient = mock[IDiscordClient]

    when(config.discord).thenReturn(Some(discordConfig))
    when(discordConfig.roles).thenReturn(Map("testAuthGroup" -> "12345"))

    implicit val client = PooledHttp1Client()
    val discordBot = new DiscordBot(config.discord.get, ud, Some(discordAPI), Some(discordClient))

    val p = new Pilot("bob_mcface",
      Pilot.Status.internal,null,null,null,null,Pilot.OM.readTree("{\"discordId\":\"12345\"}"),List("testAuthGroup"),List(),List())

    val role = mock[IRole]
    val user = mock[IUser]
    val guild = mock[IGuild]

    when(role.getStringID()).thenReturn("12345")
    when(role.getName()).thenReturn("testRole")

    when(user.getRolesForGuild(guild)).thenReturn(List[IRole]().asJava)

    when(guild.getUserByID(12345)).thenReturn(user)
    when(guild.getRoles()).thenReturn(List(role).asJava)

    discordBot.guild = Some(guild)
    discordBot.roleLookup = discordBot.createRoleLookup()

    discordBot.update(p)
    verify(user).addRole(role)
    verify(user, never()).removeRole(role)

    val user2 = mock[IUser]
    when(guild.getUserByID(12345)).thenReturn(user2)
    when(user2.getRolesForGuild(guild)).thenReturn(List(role).asJava)
    discordBot.update(p)
    verify(user2, never()).addRole(role)
    verify(user2, never()).removeRole(role)

    val p2 = p.copy(authGroups = List())
    val user3 = mock[IUser]
    when(guild.getUserByID(12345)).thenReturn(user3)
    when(user3.getRolesForGuild(guild)).thenReturn(List(role).asJava)
    discordBot.update(p2)
    verify(user3, never()).addRole(role)
    verify(user3).removeRole(role)

    // corp role
    val p4 = p2.copy(corporation = "Know Your Role")
    val user5 = mock[IUser]
    when(discordConfig.roles).thenReturn(Map("Know Your Role" -> "12345"))
    discordBot.roleLookup = discordBot.createRoleLookup()
    when(guild.getUserByID(12345)).thenReturn(user5)
    when(user5.getRolesForGuild(guild)).thenReturn(List[IRole]().asJava)
    discordBot.update(p4)
    verify(user5).addRole(role)
    verify(user5, never()).removeRole(role)

    // kick user

    val p3 = p.copy(accountStatus = Pilot.Status.ineligible)
    val user4 = mock[IUser]
    when(guild.getUserByID(12345)).thenReturn(user4)
    when(user4.getRolesForGuild(guild)).thenReturn(List(role).asJava)
    discordBot.update(p3)
    verify(guild).kickUser(user4)
    verify(ud).updateUser(p3.copy(metadata = Pilot.OM.createObjectNode()))
  }

}
