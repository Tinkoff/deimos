package ru.tinkoff.deimos.structure.operations

import java.nio.file.Path

import ru.tinkoff.deimos.schema.classes.ComplexType
import ru.tinkoff.deimos.structure._

import cats.instances.list._
import cats.syntax.foldable._

final class XsdContext(
    val indices: Indices,
    val operationContext: OperationContext,
    val stack: XsdStack,
    val generatedPackage: GeneratedPackage,
) {

  lazy val availableFiles: List[Path] = indices.availableFiles(operationContext.currentPath)

  def deduplicateParams[T <: Param](params: List[T]): List[T] =
    params.groupBy(_.name).values.toList.map { ambiguousParams =>
// TODO: Check it
//      if (ambiguousParams.forall(_ == ambiguousParams.head)) {
      ambiguousParams.head
//      } else {
//        throw InvalidSchema(s"Ambiguous params: $ambiguousParams", this.operationContext)
//      }
    }

  def toGlobalName(prefixedName: String): GlobalName =
    prefixedName.split(":") match {
      case Array(prefix, localName) =>
        val uri =
          indices.namespaces
            .get(operationContext.currentPath, prefix)
            .getOrElse(throw InvalidSchema(s"Namespace prefix '$prefix' is not defined", operationContext))
        GlobalName(uri, localName)

      case Array(localName) =>
        GlobalName(indices.namespaces.get(operationContext.currentPath, "").getOrElse(""), localName)

      case _ =>
        throw InvalidSchema(s"$prefixedName is not a valid reference", operationContext)
    }

  def getSimpleTypeByName(globalName: GlobalName): Option[String] =
    simpleTypesMap
      .get(globalName)
      .orElse(indices.simpleTypes.getItem(availableFiles, globalName).map {
        case (path, st) => ProcessSimpleType(this.copy(operations.OperationContext(path)))(st)
      })

  def copy(
      operationContext: OperationContext = operationContext,
      stack: XsdStack = stack,
      generatedPackage: GeneratedPackage = generatedPackage
  ) = new XsdContext(indices, operationContext, stack, generatedPackage)

  def getOrProcessClassName(name: GlobalName): (String, GeneratedPackage) =
    availableFiles.flatMap(generatedPackage.files.get).collectFirstSome(_.classes.get(name)) match {
      case Some(clazz) => (clazz.name, generatedPackage)
      case None =>
        indices.complexTypes
          .getItem(availableFiles, name)
          .map {
            case (newFile, ct) =>
              val className = complexTypeRealName(ct, None)
              val newCtx    = copy(operations.OperationContext(newFile), stack = stack.push(name))
              if (stack.contains(name)) {
                (className, generatedPackage)
              } else {
                val (clazz, newPackage) = ProcessComplexType(newCtx)(ct, className, Some(name))
                (clazz.name, newPackage)
              }
          }
          .getOrElse(throw InvalidSchema(s"Complex type $name not found", operationContext))
    }

  def getOrProcessClass(name: GlobalName): (GeneratedClass, GeneratedPackage) =
    availableFiles.flatMap(generatedPackage.files.get).collectFirstSome(_.classes.get(name)) match {
      case Some(clazz) => (clazz, generatedPackage)
      case None =>
        indices.complexTypes
          .getItem(availableFiles, name)
          .map {
            case (newFile, ct) =>
              val newCtx              = copy(operations.OperationContext(newFile), stack = stack.push(name))
              val (clazz, newPackage) = ProcessComplexType(newCtx)(ct, complexTypeRealName(ct, None), Some(name))
              (clazz, newPackage)
          }
          .getOrElse(throw InvalidSchema(s"Complex type $name not found", operationContext))
    }

  lazy val targetNamespace: Option[String] =
    indices.schemas.get(operationContext.currentPath).flatMap(_.targetNamespace)

  def complexTypeRealName(complexType: ComplexType, name: Option[String]): String =
    complexType.name.orElse(name).getOrElse(throw InvalidSchema("Don't know name", operationContext))
}
