package ru.tinkoff.deimos.structure

import java.nio.file.Path

import cats.Eval

final case class GeneratedPackageWrapper(
    namespaces: Map[String, String],
    files: Map[Path, GeneratedFile],
    imports: Map[Path, List[Path]]
)

final case class GeneratedPackage(
    files: Map[Path, GeneratedFile]
) {
  def addClass(path: Path, globalName: GlobalName, clazz: GeneratedClass): GeneratedPackage = {
    files.get(path) match {
      case Some(file) => GeneratedPackage(files.updated(path, file.addClass(globalName, clazz)))
      case None       => GeneratedPackage(files.updated(path, GeneratedFile(Map(globalName -> clazz), Set())))
    }
  }

  def addXmlCodec(path: Path, xmlCodec: XmlCodecInfo): GeneratedPackage = {
    files.get(path) match {
      case Some(file) => GeneratedPackage(files.updated(path, file.addXmlCodec(xmlCodec)))
      case None       => GeneratedPackage(files.updated(path, GeneratedFile(Map(), Set(xmlCodec))))
    }
  }
}

object GeneratedPackage {
  def empty = GeneratedPackage(Map())
}

final case class GeneratedFile(classes: Map[GlobalName, GeneratedClass], xmlCodecs: Set[XmlCodecInfo]) {
  def addClass(globalName: GlobalName, clazz: GeneratedClass): GeneratedFile =
    copy(classes = classes.updated(globalName, clazz), xmlCodecs)
  def addXmlCodec(xmlCodec: XmlCodecInfo): GeneratedFile =
    copy(xmlCodecs = xmlCodecs + xmlCodec)
}

final case class GeneratedClass(
    name: String,
    params: List[Param],
)

sealed trait Param {
  val name: String
  val typ: Typ
}

final case class Tag(
    name: String,
    typ: Typ,
    namespace: Option[String],
    pkg: Option[String],
    inlineDef: Option[GeneratedClass]
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
