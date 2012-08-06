name := "siigna-module"

version := "preAlpha"

organization := "com.siigna"

scalaVersion := "2.9.2"

crossScalaVersions := Seq("2.9.1", "2.9.2")

scalaSource in Compile <<= (baseDirectory in Compile)(_ / "src")

publishTo := Some(Resolver.file("file",  new File( "../rls/base" )) )

resolvers += "Siigna" at "http://siigna.com/rls/base"

libraryDependencies ++= Seq(
  "com.siigna" %% "siigna-main" % "0.1"
)
