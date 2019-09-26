resolvers in ThisBuild ++= Seq(
  "Sbt repo" at "http://repo.scala-sbt.org/scalasbt/simple/repo1-cache",
  "Confluent Maven Repo" at "http://packages.confluent.io/maven/",
  "spray repo" at "http://repo.spray.io",
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots"),
  Resolver.bintrayIvyRepo("scalameta", "maven"),
  Resolver.bintrayRepo("webjars", "maven"),
)