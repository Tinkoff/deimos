package ru.tinkoff.deimos.structure

import java.nio.file.Path

import cats.Eval

final case class GeneratedPackage(
    namespaces: Map[String, String],
    classes: ImportsIndex[Path, GlobalName, Class],
    imports: Map[Path, List[Path]]
)

final case class Content(params: List[Param], children: List[LazyClass])

final case class LazyClass(typeName: String, content: Eval[Content], xmlCodecInfo: Option[XmlCodecInfo], pkg: String) {
  def toClass = Class(typeName, content.value, xmlCodecInfo, pkg)
}

final case class Class(typeName: String, content: Content, xmlCodecInfo: Option[XmlCodecInfo], pkg: String)

sealed trait Param {
  val name: String
  val typ: Typ
}

final case class Tag(
    name: String,
    typ: Typ,
    namespace: Option[String],
    pkg: Option[String],
    inlineDef: Option[LazyClass]
) extends Param
final case class Text(name: String, typ: Pure)                           extends Param
final case class Attr(name: String, typ: Typ, namespace: Option[String]) extends Param

sealed trait Typ {
  val typ: String
  def toOptional: Typ
  def toListing: Typ
}

case class Pure(typ: String) extends Typ {
  def toOptional = Optional(typ)
  def toListing  = Listing(typ)
}
case class Optional(typ: String) extends Typ {
  def toOptional: Optional = this
  def toListing            = Listing(typ)
}
case class Listing(typ: String) extends Typ {
  def toOptional: Listing = this
  def toListing: Listing  = this
}
