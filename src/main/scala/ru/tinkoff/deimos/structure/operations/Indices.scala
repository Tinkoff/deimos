package ru.tinkoff.deimos.structure.operations

import java.nio.file.Path

import ru.tinkoff.deimos.schema.classes._
import ru.tinkoff.deimos.structure.{GlobalName, ImportsIndex, LazyClass, Tag}

final class Indices(
    val complexTypes: ImportsIndex[Path, GlobalName, ComplexType] = new ImportsIndex,
    val simpleTypes: ImportsIndex[Path, GlobalName, SimpleType] = new ImportsIndex,
    val elements: ImportsIndex[Path, GlobalName, Element] = new ImportsIndex,
    val attributes: ImportsIndex[Path, GlobalName, Attribute] = new ImportsIndex,
    val groups: ImportsIndex[Path, GlobalName, Group] = new ImportsIndex,
    val attributeGroups: ImportsIndex[Path, GlobalName, AttributeGroup] = new ImportsIndex,
//    val classes: ImportsIndex[Path, String, Class] = new ImportsIndex,
    val lazyClasses: ImportsIndex[Path, GlobalName, LazyClass] = new ImportsIndex,
    val namespaces: ImportsIndex[Path, String, String] = new ImportsIndex,
    val schemas: Map[Path, Schema],
    val availableFiles: Map[Path, List[Path]],
    val namespacePrefixes: Map[String, String],
)

object Indices {
  def apply(schemas: Map[Path, Schema], availableFiles: Map[Path, List[Path]], namespacePrefixes: Map[String, String]): Indices =
    new Indices(schemas = schemas, availableFiles = availableFiles, namespacePrefixes = namespacePrefixes)
}
