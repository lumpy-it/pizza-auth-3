name := "pizza-auth-3"

version := "1.0"

scalaVersion := "2.11.7"

resolvers += "jitpack" at "https://jitpack.io"

resolvers += Resolver.jcenterRepo

fork := true
parallelExecution in Test := false

val HTTP4S_VERSION = "0.15.3a"

// main dependencies
libraryDependencies ++= Seq(
  // frameworks
  "org.http4s"                       %% "http4s-core"               % HTTP4S_VERSION,
  "org.http4s"                       %% "http4s-dsl"                % HTTP4S_VERSION,
  "org.http4s"                       %% "http4s-blaze-server"       % HTTP4S_VERSION,
  "org.http4s"                       %% "http4s-blaze-client"       % HTTP4S_VERSION,
  "org.http4s"                       %% "http4s-circe"              % HTTP4S_VERSION,
  "org.http4s"                       %% "http4s-twirl"              % HTTP4S_VERSION,
  "org.http4s"                       %% "http4s-json4s"             % HTTP4S_VERSION,
  // logging
  "org.log4s"                        %% "log4s"                     % "1.2.0",
  "ch.qos.logback"                   % "logback-classic"            % "1.1.7",
  "ch.qos.logback"                   % "logback-core"               % "1.1.7",
  // supporting libraries
  "com.github.austinv11"             % "Discord4J"                  % "2.9.3",
  "moe.pizza"                        %% "eveapi"                    % "0.56" exclude("org.slf4j", "slf4j-simple"),
  "com.github.scopt"                 %% "scopt"                     % "3.3.0",
  "com.googlecode.gettext-commons"   % "gettext-maven-plugin"       % "1.2.4",
  "com.fasterxml.jackson.dataformat" % "jackson-dataformat-yaml"    % "2.6.1",
  "com.fasterxml.jackson.module"     %% "jackson-module-scala"      % "2.9.2",
  "net.andimiller"                   %% "integrated-query-language" % "1.1",
  "com.github.tototoshi"             %% "scala-csv"                 % "1.3.0",
  "javax.persistence"                % "persistence-api"            % "1.0.2",
  "com.orientechnologies"            % "orientdb-client"            % "2.1.12",
  "com.orientechnologies"            % "orientdb-graphdb"           % "2.1.12",
  "com.tinkerpop.blueprints"         % "blueprints"                 % "2.6.0",
  "com.pauldijou"                    %% "jwt-circe"                 % "0.10.0",
  "commons-io"                       % "commons-io"                 % "2.5",
  // xmpp client
  "org.igniterealtime.smack"         % "smack-core"                 % "4.1.7",
  "org.igniterealtime.smack"         % "smack-tcp"                  % "4.1.7",
  "org.igniterealtime.smack"         % "smack-extensions"           % "4.1.7",
  "org.igniterealtime.smack"         % "smack-java7"                % "4.1.7",
  // embedded services
  "org.apache.directory.server"      % "apacheds-all"               % "2.0.0-M22" exclude("org.slf4j", "slf4j-log4j12") exclude("org.slf4j", "slf4j-simple"),
  "org.apache.kafka"                 %% "kafka"                     % "0.8.2.2" exclude("org.slf4j", "slf4j-log4j12") exclude("org.slf4j", "slf4j-simple"),
  "com.orientechnologies"            % "orientdb-server"            % "2.1.12"
)

// test frameworks and tools
libraryDependencies ++= Seq(
  "org.scalatest"  %% "scalatest"  % "2.2.4"   % "test",
  "org.mockito"    % "mockito-all" % "1.10.19" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.0"  % "test"
)

enablePlugins(SbtTwirl)

coverageExcludedPackages := "templates\\.html\\.*;moe\\.pizza\\.auth\\.Main;moe\\.pizza\\.auth\\.queue.*"

testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-maxSize", "5", "-minSuccessfulTests", "33", "-workers", "1", "-verbosity", "1")

// Assembly

mainClass in assembly := Some("moe.pizza.auth.Main")
test in assembly := {}

assemblyMergeStrategy in assembly := {
 case PathList("META-INF", xs @ _*) => MergeStrategy.discard
 case x => MergeStrategy.first
}