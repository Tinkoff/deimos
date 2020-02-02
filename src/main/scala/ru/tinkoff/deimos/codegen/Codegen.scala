package ru.tinkoff.deimos.codegen

import java.nio.file.{Files, Path, Paths}
import java.nio.charset.StandardCharsets.UTF_8

import ru.tinkoff.deimos.structure._
import treehugger.forest._
import treehuggerDSL._
import treehugger.forest.definitions._

object Codegen {

  /**
    * Workaround of treehugger inability to generate empty case class parameter list
    * List of single invisible parameter
    */
  val empty = ValDef(Modifiers(Flags.PARAM), EmptyTree, EmptyTree)

  def pathToPackage(path: Path): String =
    path.toString.split("\\.").head.split("/").map(pack => if (pack.head.isDigit) s"N$pack" else pack).mkString(".")

  def splitPath(path: Path): (String, String) = {
    (pathToPackage(path.getParent), pathToPackage(path.getFileName))
  }

  def addPackage(typ: String, pkg: Option[String]): Type =
    pkg match {
      case Some(p) => TYPE_REF(REF(p) DOT typ)
      case None    => TYPE_REF(REF(typ))
    }

  def mkType(typ: Typ, pkg: Option[String] = None): Type = {
    typ match {
      case Pure(t)     => TYPE_REF(addPackage(t, pkg))
      case Optional(t) => TYPE_REF("scala.Option") TYPE_OF (addPackage(t, pkg))
      case Listing(t)  => TYPE_REF("scala.List") TYPE_OF (addPackage(t, pkg))
    }
  }

  def namespaceAnnotation(namespace: Option[String]): List[AnnotationInfo] =
    namespace.toList.map(ns => ANNOT("xmlns", REF(ns)))

  def generateNamespaces(struct: GeneratedPackageWrapper, dest: Path) = {
    val imports = IMPORT("ru.tinkoff.phobos.Namespace")
    val namespaces = OBJECTDEF("namespaces") := BLOCK(struct.namespaces.toList.flatMap {
      case (uri, prefix) =>
        List(
          CASEOBJECTDEF(prefix).tree,
          VAL(s"${prefix}Namespace")
            .withFlags(Flags.IMPLICIT)
            .withType(TYPE_REF("Namespace") TYPE_OF TYPE_SINGLETON(REF(prefix))) :=
            (REF("Namespace") DOT "mkInstance") APPLY LIT(uri)
        )
    })
    val trees = treeToString(BLOCK(List(imports, namespaces)) inPackage "output")
    val path  = Paths.get("output/namespaces.scala")
    Files.createDirectories(dest.resolve(path.getParent))
    Files.write(dest.resolve(path), trees.getBytes(UTF_8))
    trees
  }

  def generateClass(clazz: GeneratedClass): List[ImplDef] = {
    val params: List[ValDef] = clazz.params.map {
      case Attr(name, typ, namespace) =>
        (PARAM(name, mkType(typ)) withAnnots ANNOT("attr") :: namespaceAnnotation(namespace)).tree
      case Tag(name, typ, namespace, pkg, None) =>
        (PARAM(name, mkType(typ)) withAnnots namespaceAnnotation(namespace)).tree
      case Tag(name, typ, namespace, pkg, Some(inlineDef)) =>
        (PARAM(name, mkType(typ, Some(clazz.name))) withAnnots namespaceAnnotation(namespace)).tree
      case Text(name, typ) =>
        (PARAM(name, mkType(typ)) withAnnots ANNOT("text")).tree
    }

    val inlineDefs: List[MemberDef] = clazz.params.collect {
      case Tag(_, _, _, _, Some(inlineDef)) =>
        generateClass(inlineDef)
    }.flatten

    val elementEncoder =
      VAL("elementEncoder")
        .withFlags(Flags.IMPLICIT)
        .withType(TYPE_REF("ElementEncoder") TYPE_OF TYPE_REF(REF(clazz.name))) :=
        REF("deriveElementEncoder")

    val elementDecoder =
      VAL("elementDecoder")
        .withFlags(Flags.IMPLICIT)
        .withType(TYPE_REF("ElementDecoder") TYPE_OF TYPE_REF(REF(clazz.name))) :=
        REF("deriveElementDecoder")

    val elementInstances: List[MemberDef] = List(elementEncoder, elementDecoder)

    val caseClass = if (params.nonEmpty) {
      (CASECLASSDEF(clazz.name) withParams params).tree
    } else {
      (CASECLASSDEF(clazz.name) withParams empty).tree
    }
    List(caseClass, OBJECTDEF(clazz.name) := BLOCK(elementInstances ++ inlineDefs))
  }

  def generateSources(struct: GeneratedPackageWrapper, dest: Path) = {
    struct.files.map {
      case (fileName, file) =>
        val (pkg, obj)    = splitPath(fileName)
        val pack          = "output." + pkg
        val importedFiles = struct.imports(fileName).map(path => IMPORT("output." + pathToPackage(path) + "._"))
        val imports = List(
          IMPORT("ru.tinkoff.phobos.encoding._"),
          IMPORT("ru.tinkoff.phobos.decoding._"),
          IMPORT("ru.tinkoff.phobos.derivation.semiauto._"),
          IMPORT("ru.tinkoff.phobos.syntax._"),
          IMPORT("output.namespaces._"),
        ) ++ importedFiles

        val classes = file.classes.values.toList.flatMap(generateClass)

        val xmlCodecInstances: List[MemberDef] = file.xmlCodecs.toList.flatMap { codecInfo =>
          val codecArgs = codecInfo match {
            case XmlCodecInfo(_, name, Some(namespace)) => List(LIT(name), REF(namespace))
            case XmlCodecInfo(_, name, None)            => List(LIT(name))
          }
          val (encoderCall, decoderCall) =
            if (codecInfo.namespace.isDefined) {
              ("fromElementEncoderNs", "fromElementDecoderNs")
            } else {
              ("fromElementEncoder", "fromElementDecoder")
            }
          val xmlEncoder =
            VAL(s"${codecInfo.name}XmlEncoder")
              .withFlags(Flags.IMPLICIT)
              .withType(TYPE_REF("XmlEncoder") TYPE_OF TYPE_REF(REF(codecInfo.classType))) :=
              (REF("XmlEncoder") DOT encoderCall) APPLY codecArgs
          val xmlDecoder =
            VAL(s"${codecInfo.name}XmlDecoder")
              .withFlags(Flags.IMPLICIT)
              .withType(TYPE_REF("XmlDecoder") TYPE_OF TYPE_REF(REF(codecInfo.classType))) :=
              (REF("XmlDecoder") DOT decoderCall) APPLY codecArgs
          List(xmlEncoder, xmlDecoder)
        }

        val tree            = BLOCK(imports :+ (OBJECTDEF(obj) := BLOCK(classes ++ xmlCodecInstances))) inPackage pack
        val destinationDir  = Paths.get(pkg.replaceAll("\\.", "/"))
        val destinationFile = obj + ".scala"
        val path            = dest.resolve(Paths.get("output")).resolve(destinationDir).resolve(destinationFile)
        Files.createDirectories(path.getParent)
        Files.write(path, treeToString(tree).getBytes(UTF_8))
        tree
    }
  }

  def generate(struct: GeneratedPackageWrapper, dest: Path) = {
    generateNamespaces(struct, dest)
    generateSources(struct, dest)
  }
}
