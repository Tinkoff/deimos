package ru.tinkoff.deimos.structure.operations

import cats.syntax.flatMap._
import cats.syntax.functor._

import ru.tinkoff.deimos.schema.classes.Element
import ru.tinkoff.deimos.structure.{InvalidSchema, Pure, Tag, Typ}

object ProcessLocalElement {
  def apply(element: Element): XsdMonad[Option[Tag]] = {
    def modifyType(typ: Typ): Typ = {
      (element.minOccurs, element.maxOccurs) match {
        case (_, Some("unbounded"))               => typ.toListing
        case (_, Some(a)) if a.toInt > 1          => typ.toListing
        case (Some(0), _)                         => typ.toOptional
        case _ if element.nillable.contains(true) => typ.toOptional
        case _                                    => typ
      }
    }

    def name: XsdMonad[String] =
      element.name match {
        case Some(name) => XsdMonad.pure(name)
        case None       => XsdMonad.raiseError("Global element name is missing")
      }

    def generateTypeByName(name: String): String = name.updated(0, name.head.toUpper)

    def namespace: XsdMonad[Option[String]] =
      for {
        ctx    <- XsdMonad.ask
        schema = ctx.indices.schemas(ctx.currentPath)
      } yield
        (schema.targetNamespace, schema.elementFormDefault, element.form) match {
          case (Some(uri), _, Some("qualified"))    => ctx.indices.namespacePrefixes.get(uri)
          case (Some(uri), Some("qualified"), None) => ctx.indices.namespacePrefixes.get(uri)
          case _                                    => None
        }

    element match {
      case _ if element.ref.isDefined =>
        for {
          ctx        <- XsdMonad.ask
          globalName = ctx.toGlobalName(element.ref.get)
          namespace  <- namespace
          res <- ctx.indices.elements
                  .getItem(ctx.availableFiles, globalName)
                  .map {
                    case (newFile, e) =>
                      ProcessGlobalElement(e)
                        .local(_.copy(currentPath = newFile)) // TODO: review soft mode
                        .map { maybeTag =>
                          val tag = maybeTag.get
                          Some(tag.copy(typ = modifyType(tag.typ), namespace = namespace))
                        }
                  }
                  .getOrElse(throw InvalidSchema(s"Element $globalName not found", ctx.currentPath))
        } yield res

      case _ if element.simpleType.isDefined =>
        for {
          typ       <- ProcessSimpleType(element.simpleType.get)
          name      <- name
          namespace <- namespace
        } yield Some(Tag(name, modifyType(Pure(typ)), namespace, None, None))

      case _ if element.complexType.isDefined =>
        for {
          name         <- name
          namespace    <- namespace
          realTypeName = generateTypeByName(name)
          clazz        <- ProcessComplexType(element.complexType.get, realTypeName, None)
          typ          = modifyType(Pure(realTypeName))
        } yield Some(Tag(name, typ, namespace, None, Some(clazz)))

      case _ if element.`type`.isDefined =>
        for {
          ctx            <- XsdMonad.ask
          globalTypeName = element.`type`.map(ctx.toGlobalName).get
          name           <- name
          namespace      <- namespace
          typ <- XsdMonad.getSimpleTypeByName(globalTypeName).flatMap {
                  case Some(simpleType) =>
                    XsdMonad.pure(Pure(simpleType))
                  case None =>
                    XsdMonad.getOrProcessClassName(globalTypeName).map(Pure)
                }
        } yield Some(Tag(name, modifyType(typ), namespace, None, None))

      case _ => // TODO: Soft mode
        XsdMonad.pure(None)
    }
  }
}
