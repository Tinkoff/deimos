package ru.tinkoff.deimos.structure.operations

import cats.Eval
import ru.tinkoff.deimos.schema.classes.{ComplexContent, ComplexType, Extension}
import ru.tinkoff.deimos.schema.classes.namespaces.xsdUri
import ru.tinkoff.deimos.structure.{operations, _}

import scala.annotation.tailrec

object ProcessComplexType {
  def apply(ctx: XsdContext)(complexType: ComplexType, typeName: Option[String]): LazyClass = {
    val realTypeName = complexType.name
      .orElse(ctx.operationContext.xmlCodecInfo.map(_.name))
      .orElse(typeName)
      .getOrElse(throw InvalidSchema("Don't know name", ctx.operationContext))

    val attributes =
      ctx.deduplicateParams(
        complexType.attribute.map(ProcessAttribute(ctx)) ++
          complexType.attributeGroup.flatMap(ProcessAttributeGroup(ctx)))

    if (complexType.mixed.contains(true)) {
      println("Warning: mixed content")
    }

    def addToIndexIfGlobal(clazz: LazyClass): Unit = {
      (complexType.name, ctx.operationContext.xmlCodecInfo.map(_.name)) match {
        case (Some(localName), _) =>
          ctx.indices.lazyClasses
            .add(ctx.operationContext.currentPath, GlobalName(ctx.targetNamespace.getOrElse(""), localName), clazz)
        case (None, Some(codecName)) =>
          ctx.indices.lazyClasses
            .add(ctx.operationContext.currentPath, GlobalName(ctx.targetNamespace.getOrElse(""), codecName), clazz)
        case _ =>
      }
    }

    def processSimpleContent: LazyClass = {
      def getExtensionParams(textName: String, extension: Extension): Eval[List[Param]] = {
        val globalBaseTypeName = ctx.toGlobalName(extension.base)
        ctx.getSimpleTypeByName(globalBaseTypeName) match {
          case Some(textType) =>
            Eval.now(Text(textName, Pure(textType)) :: extension.attribute.map(ProcessAttribute(ctx)))
          case None =>
            println("Strange place") // Cause it must be always simple type
            ctx
              .getOrProcessClass(globalBaseTypeName)
              .content
              .map(_.params ++ extension.attribute.map(ProcessAttribute(ctx)))
        }
      }

      val simpleContent = complexType.simpleContent.get
      val leafParamName = "Text" // TODO: Make it unique
      simpleContent match {
        case _ if simpleContent.extension.isDefined => // TODO: Attribute groups etc

          val params = getExtensionParams(leafParamName, simpleContent.extension.get)
          val clazz = LazyClass(
            typeName = realTypeName,
            content = params.map(ps => Content(ps ++ attributes, List.empty)),
            xmlCodecInfo = ctx.operationContext.xmlCodecInfo,
            pkg = ""
          )
          addToIndexIfGlobal(clazz)
          clazz

        // TODO: Restriction
        case _ =>
          throw InvalidSchema("Not implemented", ctx.operationContext)
      }
    }

    def processComplexContent: LazyClass = {
      def processExtension(complexContent: ComplexContent): LazyClass = {

        val extension          = complexContent.extension.get
        val globalBaseTypeName = ctx.toGlobalName(extension.base)
        val inheritedParams    = ctx.getOrProcessClass(globalBaseTypeName).content.map(_.params) // TODO: Review not repeated
        val selfParams =
          Eval.later(ProcessElements(ctx.copy(ctx.operationContext.copy(xmlCodecInfo = None)))(extension))
        val attributes =
          extension.attribute.map(ProcessAttribute(ctx)) ++
            extension.attributeGroup.flatMap(ProcessAttributeGroup(ctx))

        def notRepeated(params: List[Tag])(param: Param): Boolean =
          !attributes.exists(_.name == param.name) && !params.exists(_.name == param.name)

        val content =
          for {
            inherited <- inheritedParams
            self      <- selfParams
          } yield Content(attributes ++ inherited.filter(notRepeated(self)) ++ self, List.empty)

        val clazz =
          LazyClass(
            typeName = realTypeName,
            content = content,
            xmlCodecInfo = ctx.operationContext.xmlCodecInfo,
            pkg = ""
          )

        addToIndexIfGlobal(clazz)
        clazz
      }

      def processRestriction(complexContent: ComplexContent): LazyClass = {
        // TODO: review this. Maybe it goes to another file
        @tailrec
        def findRestrictionBase(complexType: ComplexType): GlobalName = {
          val baseOpt = complexType.complexContent.flatMap(_.restriction.flatMap(_.base))
          baseOpt match {
            case Some(base) =>
              val baseGlobalName = ctx.toGlobalName(base)
              if (baseGlobalName == GlobalName(xsdUri, "anyType")) {
                baseGlobalName
              } else {
                val baseType = ctx.indices.complexTypes
                  .get(ctx.availableFiles, baseGlobalName)
                  .getOrElse(throw InvalidSchema(s"Base type is not defined: $base for $complexType", ctx.operationContext))
                if (baseType.complexContent.flatMap(_.restriction.flatMap(_.base)).isDefined) {
                  findRestrictionBase(baseType)
                } else {
                  baseGlobalName
                }
              }
            case None =>
              throw InvalidSchema("Nope", ctx.operationContext)
          }
        }

        val parent = findRestrictionBase(complexType)
        if (parent == GlobalName(xsdUri, "anyType")) {
          val clazz =
            LazyClass(
              typeName = realTypeName,
              content = Eval.now(Content(List.empty, List.empty)),
              xmlCodecInfo = ctx.operationContext.xmlCodecInfo,
              pkg = ""
            )
          addToIndexIfGlobal(clazz)
          clazz
        } else {
          ctx.getOrProcessClass(parent)
        }
      }

      val complexContent = complexType.complexContent.get
      complexContent match {
        case _ if complexContent.extension.isDefined   => processExtension(complexContent)
        case _ if complexContent.restriction.isDefined => processRestriction(complexContent)
        case _                                         => throw InvalidSchema("Strange", ctx.operationContext)
      }
    }

    complexType match {
      case _ if complexType.simpleContent.isDefined  => processSimpleContent
      case _ if complexType.complexContent.isDefined => processComplexContent

      case _ =>
        val elements =
          Eval.later(ProcessElements(ctx.copy(ctx.operationContext.copy(xmlCodecInfo = None)))(complexType))
        val clazz =
          LazyClass(
            typeName = realTypeName,
            content = elements.map(es => Content(es ++ attributes, List.empty)),
            xmlCodecInfo = ctx.operationContext.xmlCodecInfo,
            pkg = ""
          )
        addToIndexIfGlobal(clazz)
        clazz
    }
  }
}
