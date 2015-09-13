name := "Lift 2.6 starter template"

version := "0.0.4"

organization := "net.liftweb"

scalaVersion := "2.11.4"

resolvers ++= Seq("snapshots"     at "https://oss.sonatype.org/content/repositories/snapshots",
                "releases"        at "https://oss.sonatype.org/content/repositories/releases"
                )

seq(webSettings :_*)

unmanagedResourceDirectories in Test <+= (baseDirectory) { _ / "src/main/webapp" }

scalacOptions ++= Seq("-deprecation", "-unchecked")

libraryDependencies ++= {
  val liftVersion = "2.6.2"
  Seq(
    "net.liftweb"       %% "lift-webkit"        % liftVersion        % "compile",
    "net.liftmodules"   %% "lift-jquery-module_2.6" % "2.8",
    "org.eclipse.jetty" % "jetty-webapp"        % "8.1.7.v20120910"  % "container,test",
    "org.eclipse.jetty" % "jetty-plus"          % "8.1.7.v20120910"  % "container,test", // For Jetty Config
    "org.eclipse.jetty.orbit" % "javax.servlet" % "3.0.0.v201112011016" % "container,test" artifacts Artifact("javax.servlet", "jar", "jar"),
    "ch.qos.logback"    % "logback-classic"     % "1.0.6",
    "org.specs2"        %% "specs2"             % "2.3.12"           % "test",
    "net.liftweb" %% "lift-mongodb-record" % liftVersion,
    "com.foursquare" %% "rogue-field"         % "2.5.0" intransitive(),
    "com.foursquare" %% "rogue-core"          % "2.5.1" intransitive(),
    "com.foursquare" %% "rogue-lift"          % "2.5.1" intransitive(),
    "com.foursquare" %% "rogue-index"         % "2.5.1" intransitive(),
    "org.mongodb" %% "casbah" % "2.8.1",
    "com.github.tototoshi" %% "scala-csv" % "1.2.1" intransitive(),
    "org.apache.kafka" % "kafka_2.11" % "0.8.2.1" % "provided,compile",
    "org.apache.spark" %% "spark-core" % "1.4.1" % "provided",
    "org.apache.spark" %% "spark-hive" % "1.4.1",
    "org.apache.hadoop" % "hadoop-client" % "2.6.0",
    "com.nulab-inc" %% "scala-oauth2-core" % "0.15.0",
    "net.debasishg" %% "redisclient" % "3.0",
    "com.google.api-client" % "google-api-client" % "1.20.0",
    "com.jason-goodwin" %% "authentikat-jwt" % "0.4.1",
    "info.folone" %% "poi-scala" % "0.15"
  )
}

port in container.Configuration := 8081