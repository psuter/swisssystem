name := "SwissSystem"

version := "0.1"

scalaVersion := "2.11.1"

scalacOptions ++= List(
    "-deprecation",
    "-unchecked",
    "-feature",
    "-language:_")

libraryDependencies += "org.specs2" %% "specs2" % "2.3.12" % "test"
