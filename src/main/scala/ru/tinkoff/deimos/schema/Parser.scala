package ru.tinkoff.deimos.schema

import java.io.File
import java.nio.file.{Files, Path}

import com.fasterxml.aalto.stax.InputFactoryImpl
import javax.xml.stream.XMLStreamConstants
import ru.tinkoff.deimos.schema.classes.Schema
import ru.tinkoff.deimos.schema.classes.namespaces.xsd
import ru.tinkoff.phobos.Namespace
import ru.tinkoff.phobos.decoding.{Cursor, ElementDecoder, XmlDecoder, XmlStreamReader}
import cats.syntax.option._

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}

object Parser {
  def parseRecursive(root: Path, prefix: Option[String] = None): Map[Path, FileInfo] = {
    def prefixed(f: File) = {
      val name = f.getName
      prefix.fold(name)(p => s"$p/${f.getName}")
    }

    val current = prefix.fold(root)(root.resolve)

    val (dirs, files) = Files.newDirectoryStream(current).iterator.asScala.map(_.toFile).partition(_.isDirectory)

    val schema = files.map { file =>
      val name = prefixed(file)
      parseSingle(root, name)
    }

    dirs.flatMap(dir => parseRecursive(root, Some(prefixed(dir)))).toMap ++ schema
  }

  def parseSingle(root: Path, name: String): (Path, FileInfo) = {
    Try {
      val bytes = Files.readAllBytes(root.resolve(name))
      val inputFactory = new InputFactoryImpl
      val sr: XmlStreamReader = inputFactory.createAsyncForByteArray()
      val cursor = new Cursor(sr)
      sr.getInputFeeder.feedInput(bytes, 0, bytes.length)
      sr.getInputFeeder.endOfInput()
      while (cursor.getEventType != XMLStreamConstants.START_ELEMENT) {
        cursor.next()
      }
      val namespaces =
        (for (i <- 0 until cursor.getNamespaceCount) yield cursor.getNamespacePrefix(i) -> cursor.getNamespaceURI(i)).toMap
      val schema = implicitly[ElementDecoder[Schema]]
        .decodeAsElement(cursor, "schema", implicitly[Namespace[xsd.type]].getNamespace.some)
        .result(cursor.history)
        .fold(err => throw err, identity)

      sr.close()
      root.resolve(name).normalize() -> FileInfo(schema, namespaces)
    } match {
      case Failure(exception) => throw new Exception(s"Error happened while decoding ${root.resolve(name)}", exception)
      case Success(value) => value
    }
  }
}
