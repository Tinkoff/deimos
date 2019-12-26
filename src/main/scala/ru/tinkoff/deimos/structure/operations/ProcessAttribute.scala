package ru.tinkoff.deimos.structure.operations

import cats.syntax.flatMap._
import cats.syntax.functor._

import ru.tinkoff.deimos.schema.classes.Attribute
import ru.tinkoff.deimos.structure._

object ProcessAttribute {
  def apply(attribute: Attribute): XsdMonad[Attr] =
    for {
      ctx <- XsdMonad.ask
      realAttribute = attribute.ref match {
        case Some(ref) =>
          ctx.indices.attributes
            .get(ctx.availableFiles, ctx.toGlobalName(ref))
            .getOrElse(throw InvalidSchema(s"$ref refrences to nothing", ctx.currentPath))
        case None => attribute
      }
      typ <- (realAttribute.`type`, realAttribute.simpleType) match {
              case (Some(t), None) =>
                XsdMonad.pure(Pure(simpleTypesMap.getOrElse(ctx.toGlobalName(t), "String"))) // TODO: Search in imports
              case (None, Some(simpleType)) => ProcessSimpleType(simpleType).map(Pure)
              case _ =>
                XsdMonad.pure(Pure("String")) // TODO
              //              throw InvalidSchema("Either type or embedded simpleType required")
            }
      transformedTyp = realAttribute.use match {
        case Some("optional") | Some("prohibited") | None => typ.toOptional
        case Some("required")                             => typ
        case _ =>
          throw InvalidSchema("Forbidden 'use' attribute value", ctx.currentPath)
      }
      schema = ctx.indices.schemas(ctx.currentPath)
      namespace = (schema.targetNamespace, schema.attributeFormDefault, attribute.form) match {
        case (Some(uri), _, Some("qualified"))    => ctx.indices.namespacePrefixes.get(uri)
        case (Some(uri), Some("qualified"), None) => ctx.indices.namespacePrefixes.get(uri)
        case _                                    => None
      }
    } yield
      Attr(
        name = realAttribute.name.getOrElse(throw InvalidSchema("Attribute 'name' is missing", ctx.currentPath)),
        typ = transformedTyp,
        namespace = namespace
      )
}
