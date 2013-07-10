name := "siigna-cad-suite"

version := "stable"

organization := "com.siigna"

scalaVersion := "2.10.0"

crossScalaVersions := Seq("2.9.2", "2.10.0")

resolvers += "Siigna" at "http://rls.siigna.com"

publishTo := Some(Resolver.sftp("Siigna rls", "rls.siigna.com", 22, "/srv/rls") as ("siigna", new File("../budapest/jenkins.rsa")))

libraryDependencies ++= Seq(
  "com.siigna" %% "siigna-main" % "stable",
  "com.siigna" %% "siigna-base" % "stable",
  "org.scalatest" %% "scalatest" % "1.9.1" % "test"
)
