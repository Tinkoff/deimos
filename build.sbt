import Publish._
import sbt.ThisBuild
import sbt.url

ThisBuild / sbtPlugin := true

ThisBuild / scalaVersion := "2.13.8"

ThisBuild / resolvers += Resolver.sonatypeRepo("releases")
ThisBuild / resolvers += Resolver.sonatypeRepo("snapshots")

ThisBuild / libraryDependencies ++= Seq(
  "com.eed3si9n"  %% "treehugger"  % "0.4.4",
  "org.typelevel" %% "cats-core"   % "2.7.0",
  "ru.tinkoff"    %% "phobos-core" % "0.15.1",
)

ThisBuild / updateOptions := updateOptions.value.withGigahorse(false)

lazy val deimos =
  (project in file("."))
    .settings(
      publishVersion := "0.1",
      organization := "ru.tinkoff",
      ThisBuild / version := {
        val branch = git.gitCurrentBranch.value
        if (branch == "master") publishVersion.value
        else s"${publishVersion.value}-$branch-SNAPSHOT"
      },
      publishMavenStyle := true,
      publishTo :=
        (if (!isSnapshot.value) {
           sonatypePublishToBundle.value
         } else {
           Some(Opts.resolver.sonatypeSnapshots)
         }),
      scmInfo := Some(
        ScmInfo(
          url("https://github.com/Tinkoff/deimos"),
          "git@github.com:Tinkoff/deimos",
        ),
      ),
      developers := List(
        Developer(
          id = "valentiay",
          name = "Alexander Valentinov",
          email = "a.valentinov@tinkoff.ru",
          url = url("https://github.com/valentiay"),
        ),
      ),
      description := "Binaries for generating classes from XML Schemas for Phobos library",
      licenses := List("Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt")),
      homepage := Some(url("https://github.com/Tinkoff/phobos")),
      // Remove all additional repository other than Maven Central from POM
      pomIncludeRepository := { _ =>
        false
      },
      credentials += Credentials(Path.userHome / ".sbt" / "sonatype_credential"),
      moduleName := s"deimos-plugin",
      Compile / doc / sources := List.empty,
    )
