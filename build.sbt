name := "siigna-cad-suite"

version := "nightly"

organization := "com.siigna"

scalaVersion := "2.9.2"

crossScalaVersions := Seq("2.9.1", "2.9.2")

scalaSource in Compile <<= (baseDirectory in Compile)(_ / "src")

publishTo := Some(Resolver.file("file",  new File( "../rls" )) )

resolvers += "Siigna" at "http://rls.siigna.com"

libraryDependencies ++= Seq(
  "com.siigna" %% "siigna-main" % "nightly",
  "com.siigna" %% "siigna-base" % "nightly"
)
