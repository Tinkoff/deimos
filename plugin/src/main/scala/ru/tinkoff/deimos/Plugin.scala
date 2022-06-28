package ru.tinkoff.deimos

import ru.tinkoff.deimos.codegen.Codegen
import ru.tinkoff.deimos.schema.Parser
import ru.tinkoff.deimos.structure.Structure

import java.nio.file.Paths

object Plugin {
  def generate(schemaDirName: String, destinationDirName: String): Unit = {
    val schemas        = Parser.parseRecursive(Paths.get(schemaDirName))
    val structure      = Structure.process(schemas)
    val destinationDir = Paths.get(destinationDirName)
    Codegen.generate(structure, destinationDir)
  }
}
