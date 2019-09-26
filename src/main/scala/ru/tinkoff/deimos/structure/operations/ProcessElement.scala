package ru.tinkoff.deimos.structure.operations

import java.nio.file.Path

import ru.tinkoff.deimos.schema.classes.Element
import ru.tinkoff.deimos.structure._

object ProcessElement {
  def apply(ctx: XsdContext)(element: Element): Option[Tag] = {
    val schema = ctx.indices.schemas(ctx.operationContext.currentPath)

    def modifyType(typ: Typ): Typ = {
      (element.minOccurs, element.maxOccurs) match {
        case (_, Some("unbounded"))               => typ.toListing
        case (_, Some(a)) if a.toInt > 1          => typ.toListing
        case (Some(0), _)                         => typ.toOptional
        case _ if element.nillable.contains(true) => typ.toOptional
        case _                                    => typ
      }
    }

    def fetchName: String =
      element.name.getOrElse(
        throw InvalidSchema("Element name is missing while ref was not provided", ctx.operationContext))

    def generateTypeByName(name: String): String =
      name.updated(0, name.head.toUpper)

    val namespace = (schema.targetNamespace, schema.elementFormDefault, element.form) match {
      case (Some(uri), _, Some("qualified"))    => ctx.indices.namespacePrefixes.get(uri)
      case (Some(uri), Some("qualified"), None) => ctx.indices.namespacePrefixes.get(uri)
      case _                                    => None
    }

    element match {
      case _ if element.ref.isDefined => // TODO: Maybe caching
        val globalName = ctx.toGlobalName(element.ref.get)
          ctx.indices.elements
            .getItem(ctx.availableFiles, globalName)
            .map {
              case (newFile, e) =>
                val uri          = Some(globalName.uri).filter(_.nonEmpty).flatMap(ctx.indices.namespacePrefixes.get)
                val xmlCodecInfo = XmlCodecInfo(globalName.localName, uri)
                val tag = ProcessElement(ctx.copy(operations.OperationContext(newFile, Some(xmlCodecInfo))))(e).get // TODO: review soft mode
                Some(tag.copy(typ = modifyType(tag.typ), namespace = namespace)) // TODO: Really rewrite namespace?
            }
            .getOrElse(throw InvalidSchema(s"Element $globalName not found", ctx.operationContext))

      case _ if element.simpleType.isDefined =>
        val typ = modifyType(Pure(ProcessSimpleType(ctx)(element.simpleType.get)))
        Some(Tag(fetchName, typ, namespace, None, None))

      case _ if element.complexType.isDefined =>
        val name     = fetchName
        val typeName = generateTypeByName(name) // TODO: Children classes here
        val clazz    = ProcessComplexType(ctx)(element.complexType.get, Some(typeName))
        val typ      = modifyType(Pure(clazz.typeName))
        if (element.complexType.get.name.isDefined || ctx.operationContext.xmlCodecInfo.map(_.name).isDefined) {
          Some(Tag(name, typ, namespace, None, None))
        } else {
          Some(Tag(name, typ, namespace, None, Some(clazz)))
        }

      case _ if element.`type`.isDefined =>
        val globalTypeName = element.`type`.map(ctx.toGlobalName).get
        val typ            = ctx.getSimpleTypeByName(globalTypeName).map(Pure).getOrElse(Pure(ctx.getOrProcessClass(globalTypeName).typeName))
        Some(Tag(fetchName, modifyType(typ), namespace, None, None))

      case _ => // TODO: Soft mode
        None
    }
  }
}
