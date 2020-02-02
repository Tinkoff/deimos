package ru.tinkoff.deimos.structure.operations

import java.nio.file.Path

import ru.tinkoff.deimos.schema.classes.ComplexType
import ru.tinkoff.deimos.structure._

import cats.instances.list._
import cats.syntax.foldable._
import cats.syntax.functor._

final class XsdContext(
    val indices: Indices,
    val currentPath: Path,
    val stack: XsdStack,
) {

  lazy val availableFiles: List[Path] = indices.availableFiles(currentPath)

  def deduplicateParams[T <: Param](params: List[T]): List[T] =
    params.groupBy(_.name).values.toList.map { ambiguousParams =>
// TODO: Check it
//      if (ambiguousParams.forall(_ == ambiguousParams.head)) {
      ambiguousParams.head
//      } else {
//        throw InvalidSchema(s"Ambiguous params: $ambiguousParams", this.currentPath)
//      }
    }

  def toGlobalName(prefixedName: String): GlobalName =
    prefixedName.split(":") match {
      case Array(prefix, localName) =>
        val uri =
          indices.namespaces
            .get(currentPath, prefix)
            .getOrElse(throw InvalidSchema(s"Namespace prefix '$prefix' is not defined", currentPath))
        GlobalName(uri, localName)

      case Array(localName) =>
        GlobalName(indices.namespaces.get(currentPath, "").getOrElse(""), localName)

      case _ =>
        throw InvalidSchema(s"$prefixedName is not a valid reference", currentPath)
    }

  def copy(
      currentPath: Path = currentPath,
      stack: XsdStack = stack,
      generatedPackage: GeneratedPackage = generatedPackage
  ) = new XsdContext(indices, currentPath, stack, generatedPackage)

//  def getOrProcessClassName(name: GlobalName): (String, GeneratedPackage) =
//    availableFiles.flatMap(generatedPackage.files.get).collectFirstSome(_.classes.get(name)) match {
//      case Some(clazz) => (clazz.name, generatedPackage)
//      case None =>
//        indices.complexTypes
//          .getItem(availableFiles, name)
//          .map {
//            case (newFile, ct) =>
//              val className = complexTypeRealName(ct, None)
//              val newCtx    = copy(operations.OperationContext(newFile), stack = stack.push(name))
//              if (stack.contains(name)) {
//                (className, generatedPackage)
//              } else {
//                val (clazz, newPackage) = ProcessComplexType(newCtx)(ct, className, Some(name))
//                (clazz.name, newPackage)
//              }
//          }
//          .getOrElse(throw InvalidSchema(s"Complex type $name not found", operationContext))
//    }

//  def getOrProcessClass(name: GlobalName): (GeneratedClass, GeneratedPackage) =
//    availableFiles.flatMap(generatedPackage.files.get).collectFirstSome(_.classes.get(name)) match {
//      case Some(clazz) => (clazz, generatedPackage)
//      case None =>
//        indices.complexTypes
//          .getItem(availableFiles, name)
//          .map {
//            case (newFile, ct) =>
//              val newCtx              = copy(operations.OperationContext(newFile), stack = stack.push(name))
//              val (clazz, newPackage) = ProcessComplexType(newCtx)(ct, complexTypeRealName(ct, None), Some(name))
//              (clazz, newPackage)
//          }
//          .getOrElse(throw InvalidSchema(s"Complex type $name not found", operationContext))
//    }

  lazy val targetNamespace: Option[String] =
    indices.schemas.get(currentPath).flatMap(_.targetNamespace)

  def complexTypeRealName(complexType: ComplexType, name: Option[String]): String =
    complexType.name.orElse(name).getOrElse(throw InvalidSchema("Don't know name", currentPath))
}
