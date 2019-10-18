package ru.tinkoff.deimos.structure

import java.nio.file.Path

final case class InvalidSchema(message: String, path: Path) extends Throwable {
  override val getMessage: String = s"Schema is invalid: $message, $path"
}
