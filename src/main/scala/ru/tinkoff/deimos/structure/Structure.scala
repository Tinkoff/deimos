package ru.tinkoff.deimos.structure

import java.nio.file.{Path, Paths}

import ru.tinkoff.deimos.schema.FileInfo
import ru.tinkoff.deimos.schema.classes._
import ru.tinkoff.deimos.structure.operations.{Indices, OperationContext, ProcessComplexType, ProcessElement, XsdContext}

object Structure {
  def process(schemas: Map[Path, FileInfo]): GeneratedPackage = {
    val namespacesPrefixes: Map[String, String] =
      schemas.values.flatMap { fileInfo =>
        fileInfo.namespaces.map {
          case (prefix, uri) if prefix.isEmpty => ("ans", uri)
          case ns                              => ns
        }
      }.groupBy(_._1)
        .values
        .flatMap {
          case List((prefix, uri)) =>
            List(uri -> prefix)
          case namespaces =>
            namespaces.zipWithIndex.map { case ((prefix, uri), index) => uri -> s"$prefix$index" }
        }
        .toMap

    val availableFiles = schemas.map {
      case (path, FileInfo(schema, _)) =>
        val importedFiles = // TODO: Namespace in `import`
          (schema.include.map(_.schemaLocation) ++ schema.`import`.map(_.schemaLocation)).map { schemaLocation =>
            val includedPath = Paths.get(schemaLocation)
            path.getParent.resolve(includedPath).normalize()
          }
        path -> (path :: importedFiles)
    }

    val schemasIndex = schemas.map { case (path, fileInfo) => path -> fileInfo.schema }

    val indices = Indices(schemasIndex, availableFiles, namespacesPrefixes)

    schemas.foreach {
      case (path, FileInfo(schema, namespaces)) =>
        val targetNamespace = schema.targetNamespace.getOrElse("")

        def addToIndex[V <: Global](index: ImportsIndex[Path, GlobalName, V], typ: String)(v: V): Unit = {
          val name =
            v.name.orElse(v.id).getOrElse(throw InvalidSchema(s"Neither name nor id was provided for global $typ", ???))
          index.add(path, GlobalName(targetNamespace, name), v)
        }

        schema.complexType.foreach(addToIndex(indices.complexTypes, "complexType"))
        schema.simpleType.foreach(addToIndex(indices.simpleTypes, "simpleType"))
        schema.element.foreach(addToIndex(indices.elements, "element"))
        schema.attribute.foreach(addToIndex(indices.attributes, "attribute"))
        schema.group.foreach(addToIndex(indices.groups, "group"))
        schema.attributeGroup.foreach(addToIndex(indices.attributeGroups, "attributeGroup"))
        for { (prefix, uri) <- namespaces } { indices.namespaces.add(path, prefix, uri) }
    }

    schemas.foreach {
      case (path, FileInfo(schema, _)) =>
        schema.complexType.foreach { complexType =>
          val context = new XsdContext(indices, OperationContext(path, None))
          ProcessComplexType(context)(complexType, None)
        }
        schema.element.foreach { element =>
          val elementName = element.name.getOrElse(throw InvalidSchema("Global element name is missing", ???))
          val operationContext =
            OperationContext(
              path,
              Some(XmlCodecInfo(elementName, schema.targetNamespace.flatMap(namespacesPrefixes.get)))
            )
          val context = new XsdContext(indices, operationContext)
          ProcessElement(context)(element)
        }
    }

    GeneratedPackage(namespacesPrefixes, indices.lazyClasses.map(_.toClass), availableFiles)
  }
}
