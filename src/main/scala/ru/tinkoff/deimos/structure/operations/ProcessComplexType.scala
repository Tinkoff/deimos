package ru.tinkoff.deimos.structure.operations

import cats.syntax.flatMap._
import cats.syntax.functor._
import ru.tinkoff.deimos.schema.classes.{ComplexContent, ComplexType, Extension}
import ru.tinkoff.deimos.schema.classes.namespaces.xsdUri
import ru.tinkoff.deimos.structure._

import scala.annotation.tailrec

object ProcessComplexType {
  def apply(complexType: ComplexType,
            realTypeName: String,
            globalName: Option[GlobalName]): XsdMonad[GeneratedClass] = {
    def addToPackageIfGlobal(clazz: GeneratedClass): XsdMonad[Unit] =
      for {
        ctx <- XsdMonad.ctx
        _ <- globalName match {
              case Some(globalName) =>
                XsdMonad.addClass(ctx.currentPath, globalName, clazz)
              case _ =>
                println("Not global:")
                println(clazz)
                XsdMonad.unit
            }
      } yield ()

    def processSimpleContent: XsdMonad[GeneratedClass] = {
      def getExtensionParams(textName: String, extension: Extension): XsdMonad[List[Param]] =
        for {
          ctx                <- XsdMonad.ctx
          globalBaseTypeName = ctx.toGlobalName(extension.base)
          attributes         <- ProcessAttributes(extension)
          params <- XsdMonad.getSimpleTypeByName(globalBaseTypeName).flatMap[List[Param]] {
                     case Some(textType) =>
                       XsdMonad.pure(Text(textName, Pure(textType)) :: attributes)
                     case None =>
                       println("Strange place") // Cause it must be always simple type
                       XsdMonad.getOrProcessClass(globalBaseTypeName).map(clazz => clazz.params ++ attributes)
                   }
        } yield params

      val simpleContent = complexType.simpleContent.get
      val textParamName = "Text" // TODO: Make it unique
      simpleContent match {
        case _ if simpleContent.extension.isDefined => // TODO: Attribute groups etc
          for {
            params <- getExtensionParams(textParamName, simpleContent.extension.get)
            clazz  = GeneratedClass(name = realTypeName, params = params)
            _      <- addToPackageIfGlobal(clazz)
          } yield clazz

        // TODO: Restriction
        case _ =>
          XsdMonad.failure("Not implemented")
      }
    }

    def processComplexContent: XsdMonad[GeneratedClass] = {
      def processExtension(complexContent: ComplexContent): XsdMonad[GeneratedClass] = {
        def notRepeated(params: List[Param])(param: Param): Boolean =
          !params.exists(_.name == param.name)

        val extension = complexContent.extension.get

        for {
          ctx                <- XsdMonad.ctx
          globalBaseTypeName = ctx.toGlobalName(extension.base)
          baseClass          <- XsdMonad.getOrProcessClass(globalBaseTypeName) // TODO: Review not repeated
          selfParams         <- ProcessElements(extension)
          attributes         <- ProcessAttributes(extension)
          params             = attributes ++ selfParams ++ baseClass.params.filter(notRepeated(attributes ++ selfParams))
          clazz              = GeneratedClass(name = realTypeName, params = params)
          _                  <- addToPackageIfGlobal(clazz)
        } yield clazz
      }

      def processRestriction(complexContent: ComplexContent): XsdMonad[GeneratedClass] = {
        // TODO: review this. Maybe it goes to another file
//        @tailrec
        def findRestrictionBase(complexType: ComplexType): XsdMonad[GlobalName] = {
          val base = complexType.complexContent.get.restriction.get.base.get
          for {
            ctx            <- XsdMonad.ctx
            baseGlobalName = ctx.toGlobalName(base)
            res <- if (baseGlobalName == GlobalName(xsdUri, "anyType")) {
                    XsdMonad.pure(baseGlobalName)
                  } else {
                    val baseType = ctx.indices.complexTypes
                      .get(ctx.availableFiles, baseGlobalName)
                      .getOrElse(
                        throw InvalidSchema(s"Base type is not defined: $base for $complexType", ctx.currentPath))
                    if (baseType.complexContent.flatMap(_.restriction.map(_.base)).isDefined) {
                      findRestrictionBase(baseType)
                    } else {
                      XsdMonad.pure(baseGlobalName)
                    }
                  }
          } yield res
        }

        for {
          parent <- findRestrictionBase(complexType)
          clazz <- if (parent == GlobalName(xsdUri, "anyType")) {
                    val clazz = GeneratedClass(name = realTypeName, params = List.empty)
                    addToPackageIfGlobal(clazz).map(_ => clazz)
                  } else {
                    XsdMonad
                      .getOrProcessClass(parent)
                      .map(clazz => clazz.copy(name = realTypeName))
                      .flatTap(addToPackageIfGlobal)
                  }
        } yield clazz
      }

      val complexContent = complexType.complexContent.get
      complexContent match {
        case _ if complexContent.extension.isDefined   => processExtension(complexContent)
        case _ if complexContent.restriction.isDefined => processRestriction(complexContent)
        case _ =>
          XsdMonad.failure("ComplexContent must contain either restriction or extension")
      }
    }

    def processRegularContent: XsdMonad[GeneratedClass] = {
      for {
        attributes <- ProcessAttributes(complexType)
        elements   <- ProcessElements(complexType)
        clazz      = GeneratedClass(name = realTypeName, params = elements ++ attributes)
        _          <- addToPackageIfGlobal(clazz)
      } yield clazz
    }

    if (complexType.mixed.contains(true)) {
      println("Warning: mixed content")
    }

    complexType match {
      case _ if complexType.simpleContent.isDefined  => processSimpleContent
      case _ if complexType.complexContent.isDefined => processComplexContent
      case _                                         => processRegularContent
    }
  }
}
