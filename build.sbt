name := "siigna-module"

version := "0.1"

organization := "com.siigna"

scalaVersion := "2.9.2"

crossScalaVersions := Seq("2.9.1", "2.9.2")

scalaSource in Compile <<= (baseDirectory in Compile)(_ / "src")

publishTo := Some(Resolver.file("file",  new File( "../rls" )) )

resolvers += "Siigna" at "http://siigna.com/rls"

libraryDependencies ++= Seq(
  "com.siigna" %% "siigna-main" % "0.1"
)
