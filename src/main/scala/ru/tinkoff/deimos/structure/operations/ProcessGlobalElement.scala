package ru.tinkoff.deimos.structure.operations

import ru.tinkoff.deimos.schema.classes.Element
import ru.tinkoff.deimos.structure.{GeneratedPackage, GlobalName, InvalidSchema, Pure, Tag, XmlCodecInfo}

object ProcessGlobalElement {
  def apply(ctx: XsdContext)(element: Element): (Option[Tag], GeneratedPackage) = {
    val schema = ctx.indices.schemas(ctx.operationContext.currentPath)

    def fetchName: String =
      element.name.getOrElse(
        throw InvalidSchema("Element name is missing while ref was not provided", ctx.operationContext))

    def generateTypeByName(name: String): String = name.updated(0, name.head.toUpper)

    val namespace = (schema.targetNamespace, schema.elementFormDefault, element.form) match {
      case (Some(uri), _, Some("qualified"))    => ctx.indices.namespacePrefixes.get(uri)
      case (Some(uri), Some("qualified"), None) => ctx.indices.namespacePrefixes.get(uri)
      case _                                    => None
    }

    element match {
      case _ if element.simpleType.isDefined =>
        val typ      = Pure(ProcessSimpleType(ctx)(element.simpleType.get))
        val name     = fetchName
        val xmlCodec = XmlCodecInfo(typ.typ, name, namespace)
        (Some(Tag(name, typ, namespace, None, None)),
         ctx.generatedPackage.addXmlCodec(ctx.operationContext.currentPath, xmlCodec))

      case _ if element.complexType.isDefined =>
        val name         = fetchName
        val realTypeName = generateTypeByName(name) // TODO: Children classes here
        val globalName   = GlobalName(ctx.targetNamespace.getOrElse(""), name)
        val newCtx       = ctx.copy(stack = ctx.stack.push(globalName))
        val (clazz, generatedPackage) =
          ProcessComplexType(newCtx)(element.complexType.get, realTypeName, Some(globalName))
        val typ      = Pure(clazz.name)
        val xmlCodec = XmlCodecInfo(clazz.name, name, namespace)
        (Some(Tag(name, typ, namespace, None, None)),
         generatedPackage.addXmlCodec(ctx.operationContext.currentPath, xmlCodec))

      case _ if element.`type`.isDefined =>
        val globalTypeName = element.`type`.map(ctx.toGlobalName).get
        val (typ, generatedPackage) =
          ctx.getSimpleTypeByName(globalTypeName) match {
            case Some(simpleType) =>
              (Pure(simpleType), ctx.generatedPackage)
            case None =>
              val (className, generatedPackage) = ctx.getOrProcessClassName(globalTypeName)
              (Pure(className), generatedPackage)
          }
        val name     = fetchName
        val xmlCodec = XmlCodecInfo(typ.typ, name, namespace)
        (Some(Tag(name, typ, namespace, None, None)),
         generatedPackage.addXmlCodec(ctx.operationContext.currentPath, xmlCodec))
    }
  }
}
