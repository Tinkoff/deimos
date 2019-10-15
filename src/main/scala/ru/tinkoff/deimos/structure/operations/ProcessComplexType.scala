package ru.tinkoff.deimos.structure.operations

import ru.tinkoff.deimos.schema.classes.{ComplexContent, ComplexType, Extension}
import ru.tinkoff.deimos.schema.classes.namespaces.xsdUri
import ru.tinkoff.deimos.structure._

import scala.annotation.tailrec

object ProcessComplexType {
  def apply(ctx: XsdContext)(complexType: ComplexType,
                             realTypeName: String,
                             globalName: Option[GlobalName]): (GeneratedClass, GeneratedPackage) = {
    val attributes =
      ctx.deduplicateParams(
        complexType.attribute.map(ProcessAttribute(ctx)) ++
          complexType.attributeGroup.flatMap(ProcessAttributeGroup(ctx)))

    if (complexType.mixed.contains(true)) {
      println("Warning: mixed content")
    }

    def addToPackageIfGlobal(ctx: XsdContext, clazz: GeneratedClass): GeneratedPackage = {
//      println((complexType.name, xmlCodecInfo.map(_.name)))
      globalName match {
        case Some(globalName) =>
          ctx.generatedPackage.addClass(
            ctx.operationContext.currentPath,
            globalName,
            clazz
          )
        case _ =>
          println("Not global:")
          println(clazz)
          ctx.generatedPackage
      }
    }

    def processSimpleContent: (GeneratedClass, GeneratedPackage) = {
      def getExtensionParams(textName: String, extension: Extension): (List[Param], GeneratedPackage) = {
        val globalBaseTypeName = ctx.toGlobalName(extension.base)
        ctx.getSimpleTypeByName(globalBaseTypeName) match {
          case Some(textType) =>
            (Text(textName, Pure(textType)) :: extension.attribute.map(ProcessAttribute(ctx)), ctx.generatedPackage)
          case None =>
            println("Strange place") // Cause it must be always simple type
            val (clazz, generatedPackage) = ctx.getOrProcessClass(globalBaseTypeName)
            (clazz.params ++ extension.attribute.map(ProcessAttribute(ctx)), generatedPackage)
        }
      }

      val simpleContent = complexType.simpleContent.get
      val leafParamName = "Text" // TODO: Make it unique
      simpleContent match {
        case _ if simpleContent.extension.isDefined => // TODO: Attribute groups etc

          val (params, generatedPackage) = getExtensionParams(leafParamName, simpleContent.extension.get)
          val clazz = GeneratedClass(
            name = realTypeName,
            params = params ++ attributes,
          )
          val newCtx = ctx.copy(generatedPackage = generatedPackage)
          (clazz, addToPackageIfGlobal(newCtx, clazz))

        // TODO: Restriction
        case _ =>
          throw InvalidSchema("Not implemented", ctx.operationContext)
      }
    }

    def processComplexContent: (GeneratedClass, GeneratedPackage) = {
      def processExtension(complexContent: ComplexContent): (GeneratedClass, GeneratedPackage) = {

        val extension                         = complexContent.extension.get
        val globalBaseTypeName                = ctx.toGlobalName(extension.base)
        val (baseClass, baseGeneratedPackage) = ctx.getOrProcessClass(globalBaseTypeName) // TODO: Review not repeated
        val newCtx: XsdContext                = ctx.copy(generatedPackage = baseGeneratedPackage)
        val (selfParams, generatedPackage)    = ProcessElements.apply(newCtx)(extension)

        val attributes =
          extension.attribute.map(ProcessAttribute(ctx)) ++
            extension.attributeGroup.flatMap(ProcessAttributeGroup(ctx))

        def notRepeated(params: List[Tag])(param: Param): Boolean =
          !attributes.exists(_.name == param.name) && !params.exists(_.name == param.name)

        val params = attributes ++ baseClass.params.filter(notRepeated(selfParams)) ++ selfParams

        val clazz =
          GeneratedClass(
            name = realTypeName,
            params = params,
          )

        val newCtx2 = newCtx.copy(generatedPackage = generatedPackage)
        (clazz, addToPackageIfGlobal(newCtx2, clazz))
      }

      def processRestriction(complexContent: ComplexContent): (GeneratedClass, GeneratedPackage) = {
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
                  .getOrElse(
                    throw InvalidSchema(s"Base type is not defined: $base for $complexType", ctx.operationContext))
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
            GeneratedClass(
              name = realTypeName,
              params = List.empty,
            )
          (clazz, addToPackageIfGlobal(ctx, clazz))
        } else {
          val (clazz, generatedPackage) = ctx.getOrProcessClass(parent)
          val newClass = clazz.copy(name = realTypeName)
          (newClass, addToPackageIfGlobal(ctx.copy(generatedPackage = generatedPackage), newClass))
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
        val (elements, generatedPackage) =
          ProcessElements(ctx)(complexType)
        val clazz =
          GeneratedClass(
            name = realTypeName,
            params = elements ++ attributes,
          )
        val newCtx = ctx.copy(generatedPackage = generatedPackage)
        (clazz, addToPackageIfGlobal(newCtx, clazz))
    }
  }
}
