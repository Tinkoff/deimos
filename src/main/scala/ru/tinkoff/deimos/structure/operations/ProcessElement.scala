//package ru.tinkoff.deimos.structure.operations
//
//import java.nio.file.Path
//
//import ru.tinkoff.deimos.schema.classes.Element
//import ru.tinkoff.deimos.structure._
//
//object ProcessElement {
//  def apply(ctx: XsdContext)(element: Element): (Option[Tag], GeneratedPackage) = {
//    val schema = ctx.indices.schemas(ctx.operationContext.currentPath)
//
//    def modifyType(typ: Typ): Typ = {
//      (element.minOccurs, element.maxOccurs) match {
//        case (_, Some("unbounded"))               => typ.toListing
//        case (_, Some(a)) if a.toInt > 1          => typ.toListing
//        case (Some(0), _)                         => typ.toOptional
//        case _ if element.nillable.contains(true) => typ.toOptional
//        case _                                    => typ
//      }
//    }
//
//    def fetchName: String =
//      element.name.getOrElse(
//        throw InvalidSchema("Element name is missing while ref was not provided", ctx.operationContext))
//
//    def xmlCodecInfo: Option[XmlCodecInfo] = element.name.map(name => XmlCodecInfo(name, ctx.targetNamespace))
//
//    def generateTypeByName(name: String): String =
//      name.updated(0, name.head.toUpper)
//
//    val namespace = (schema.targetNamespace, schema.elementFormDefault, element.form) match {
//      case (Some(uri), _, Some("qualified"))    => ctx.indices.namespacePrefixes.get(uri)
//      case (Some(uri), Some("qualified"), None) => ctx.indices.namespacePrefixes.get(uri)
//      case _                                    => None
//    }
//
//    element match {
//      case _ if element.ref.isDefined => // TODO: Maybe caching
//        val globalName = ctx.toGlobalName(element.ref.get)
//        ctx.indices.elements
//          .getItem(ctx.availableFiles, globalName)
//          .map {
//            case (newFile, e) =>
//              val (maybeTag, generatedPackage) = ProcessElement(
//                ctx.copy(operations.OperationContext(newFile)))(e) // TODO: review soft mode
//              val tag = maybeTag.get
//              (Some(tag.copy(typ = modifyType(tag.typ), namespace = namespace)), generatedPackage) // TODO: Really rewrite namespace?
//          }
//          .getOrElse(throw InvalidSchema(s"Element $globalName not found", ctx.operationContext))
//
//      case _ if element.simpleType.isDefined =>
//        val typ = modifyType(Pure(ProcessSimpleType(ctx)(element.simpleType.get)))
//        (Some(Tag(fetchName, typ, namespace, None, None)), ctx.generatedPackage)
//
//      case _ if element.complexType.isDefined =>
//        val name         = fetchName
//        val realTypeName = generateTypeByName(name) // TODO: Children classes here
//        val newCtx = xmlCodecInfo match {
//          case Some(XmlCodecInfo(codecName, _)) =>
//            val globalName = GlobalName(ctx.targetNamespace.getOrElse(""), codecName)
//            ctx.copy(stack = ctx.stack.push(globalName))
//          case None => ctx
//        }
//        val (clazz, generatedPackage) = ProcessComplexType(newCtx)(element.complexType.get, realTypeName, xmlCodecInfo)
//        val typ                       = modifyType(Pure(realTypeName))
//        if (xmlCodecInfo.map(_.name).isDefined) {
//          (Some(Tag(name, typ, namespace, None, None)), generatedPackage)
//        } else {
//          (Some(Tag(name, typ, namespace, None, Some(clazz))), generatedPackage)
//        }
//
//      case _ if element.`type`.isDefined =>
//        val globalTypeName = element.`type`.map(ctx.toGlobalName).get
//        val (typ, generatedPackage) =
//          ctx.getSimpleTypeByName(globalTypeName) match {
//            case Some(simpleType) =>
//              (Pure(simpleType), ctx.generatedPackage)
//            case None =>
//              val (className, generatedPackage) = ctx.getOrProcessClassName(globalTypeName)
//              (Pure(className), generatedPackage)
//          }
//        (Some(Tag(fetchName, modifyType(typ), namespace, None, None)), generatedPackage)
//
//      case _ => // TODO: Soft mode
//        (None, ctx.generatedPackage)
//    }
//  }
//}
