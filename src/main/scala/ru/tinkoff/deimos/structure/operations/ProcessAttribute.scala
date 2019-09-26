package ru.tinkoff.deimos.structure.operations

import ru.tinkoff.deimos.schema.classes.Attribute
import ru.tinkoff.deimos.structure._

object ProcessAttribute {
  def apply(ctx: XsdContext)(attribute: Attribute): Attr = {
    val realAttribute = attribute.ref match {
      case Some(ref) =>
        ctx.indices.attributes
          .get(ctx.availableFiles, ctx.toGlobalName(ref))
          .getOrElse(throw InvalidSchema(s"$ref refrences to nothing", ctx.operationContext))
      case None => attribute
    }
    val typ = (realAttribute.`type`, realAttribute.simpleType) match {
      case (Some(t), None)          => Pure(simpleTypesMap.getOrElse(ctx.toGlobalName(t), "String")) // TODO: Search in imports
      case (None, Some(simpleType)) => Pure(ProcessSimpleType(ctx)(simpleType))
      case _ =>
        Pure("String") // TODO
      //              throw InvalidSchema("Either type or embedded simpleType required")
    }
    val transformedTyp = realAttribute.use match {
      case Some("optional") | Some("prohibited") | None => typ.toOptional
      case Some("required")                             => typ
      case _ =>
        throw InvalidSchema("Forbidden 'use' attribute value", ctx.operationContext)
    }

    val schema = ctx.indices.schemas(ctx.operationContext.currentPath)

    val namespace = (schema.targetNamespace, schema.attributeFormDefault, attribute.form) match {
      case (Some(uri), _, Some("qualified"))    => ctx.indices.namespacePrefixes.get(uri)
      case (Some(uri), Some("qualified"), None) => ctx.indices.namespacePrefixes.get(uri)
      case _                                    => None
    }
    Attr(
      name = realAttribute.name.getOrElse(throw InvalidSchema("Attribute 'name' is missing", ctx.operationContext)),
      typ = transformedTyp,
      namespace = namespace
    )
  }
}
