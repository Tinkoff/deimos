package ru.tinkoff.deimos.schema.classes

import ru.tinkoff.phobos.decoding.ElementDecoder
import ru.tinkoff.phobos.derivation
import ru.tinkoff.phobos.syntax.attr

final case class Import(@attr namespace: String, @attr schemaLocation: String)

object Import {
  implicit val importElementDecoder: ElementDecoder[Import] = derivation.semiauto.deriveElementDecoder
}