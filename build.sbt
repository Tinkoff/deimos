import Publish._

name := "deimos"

sbtPlugin := true

scalaVersion := "2.12.8"

resolvers += Resolver.sonatypeRepo("releases")
resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "com.eed3si9n" %% "treehugger" % "0.4.3"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0-RC1"

libraryDependencies += "ru.tinkoff" %% "phobos-core" % "0.1.2-hygiene-SNAPSHOT"

updateOptions in ThisBuild := updateOptions.value.withGigahorse(false)
