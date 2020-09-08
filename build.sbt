import Publish._

name := "deimos"

sbtPlugin := true

scalaVersion := "2.12.8"

resolvers += Resolver.sonatypeRepo("releases")
resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "com.eed3si9n" %% "treehugger" % "0.4.4"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0"

libraryDependencies += "ru.tinkoff" %% "phobos-core" % "0.9.0"

updateOptions in ThisBuild := updateOptions.value.withGigahorse(false)
