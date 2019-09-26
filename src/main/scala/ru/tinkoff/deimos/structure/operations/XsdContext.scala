package ru.tinkoff.deimos.structure.operations

import java.nio.file.Path

import cats.Eval
import ru.tinkoff.deimos.structure.{operations, _}

final class XsdContext(val indices: Indices, val operationContext: OperationContext) {

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

  def copy(operationContext: OperationContext) =
    new XsdContext(indices, operationContext)

  def getOrProcessClass(name: GlobalName): LazyClass = {
    indices.lazyClasses
      .get(availableFiles, name)
      .orElse(indices.complexTypes.getItem(availableFiles, name).map {
        case (newFile, ct) =>
          ProcessComplexType(copy(operations.OperationContext(newFile)))(ct, None)
      })
      .getOrElse(throw InvalidSchema(s"Complex type $name not found", operationContext))
  }

  lazy val targetNamespace: Option[String] =
    indices.schemas.get(operationContext.currentPath).flatMap(_.targetNamespace)
}
