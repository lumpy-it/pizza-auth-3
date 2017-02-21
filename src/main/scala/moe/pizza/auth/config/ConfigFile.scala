package moe.pizza.auth.config

import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.databind.JsonNode
import moe.pizza.auth.adapters.GraderChain
import moe.pizza.auth.adapters.PilotGraderLike.PilotGraderFactory
import moe.pizza.auth.interfaces.PilotGrader
import org.http4s.client.Client
import moe.pizza.auth.webapp.oauth.OAuthApplication


/**
  * Created by andi on 19/02/16.
  */
object ConfigFile {
  case class EmbeddedLdapConfig(
      instancePath: String = "./ldap",
      port: Int = 389,
      basedn: String = "ou=pizza",
      host: String = "localhost",
      password: Option[String] = None
  )
  case class AuthGroupConfig(closed: List[String],
                             open: List[String],
                             public: List[String])

  case class AuthConfig(
      domain: String,
      corporation: String,
      alliance: String,
      groupName: String,
      groupShortName: String,
      groups: AuthGroupConfig,
      graders: List[JsonNode],
      restkeys: List[String],
      applications: List[OAuthApplication] = List()
  ) {
    def constructGraders(c: ConfigFile)(
        implicit client: Client): PilotGrader =
      new GraderChain(
        graders.map(g => PilotGraderFactory.fromYaml(g, c)).flatten.toList)

  }
  case class CrestConfig(
      @JsonProperty("login_url") loginUrl: String,
      @JsonProperty("crest_url") crestUrl: String,
      clientID: String,
      secretKey: String,
      redirectUrl: String
  )
  case class DiscordConfig(
          clientId: String,
          clientSecret: String,
          redirectUrl: String,
          botToken: String,
          guildId: String,
          roles: Map[String, String] = Map()
  )
  case class ConfigFile(
      crest: CrestConfig,
      auth: AuthConfig,
      discord: DiscordConfig,
      embeddedldap: EmbeddedLdapConfig
  )

}
