package ru.tinkoff.deimos

import sbt._
import sbt.Keys._

final case class XmlResource(resource: File, destinationPackage: String)
