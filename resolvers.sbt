ThisBuild / resolvers ++= Seq(
  "Sbt repo" at "https://repo.scala-sbt.org/scalasbt/simple/repo1-cache",
  "Confluent Maven Repo" at "https://packages.confluent.io/maven/",
  "spray repo" at "https://repo.spray.io",
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots"),
  Resolver.bintrayIvyRepo("scalameta", "maven"),
  Resolver.bintrayRepo("webjars", "maven"),
)
