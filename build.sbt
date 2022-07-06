import sbt.ThisBuild
import sbt.url

scalaVersion := "2.13.8"

libraryDependencies ++= Seq(
  "com.eed3si9n"  %% "treehugger"  % "0.4.4",
  "org.typelevel" %% "cats-core"   % "2.7.0",
  "ru.tinkoff"    %% "phobos-core" % "0.15.1",
)

updateOptions := updateOptions.value.withGigahorse(false)

moduleName := "deimos-plugin"
versionScheme := Some("early-semver")

organization := "ru.tinkoff"
version := "0.1"
publishMavenStyle := true
publishTo := sonatypePublishToBundle.value
scmInfo := Some(
  ScmInfo(
    url("https://github.com/Tinkoff/deimos"),
    "git@github.com:Tinkoff/deimos",
  ),
)
developers := List(
  Developer(
    id = "valentiay",
    name = "Alexander Valentinov",
    email = "a.valentinov@tinkoff.ru",
    url = url("https://github.com/valentiay"),
  ),
)
description := "Binaries for generating classes from XML Schemas for Phobos library"
licenses := List("Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt"))
homepage := Some(url("https://github.com/Tinkoff/deimos"))
// Remove all additional repository other than Maven Central from POM
pomIncludeRepository := { _ => false}
credentials += Credentials(Path.userHome / ".sbt" / "sonatype_credential")
Compile / doc / sources := List.empty
