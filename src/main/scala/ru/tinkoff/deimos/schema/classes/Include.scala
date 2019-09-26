package ru.tinkoff.deimos.schema.classes

import ru.tinkoff.phobos.decoding.ElementDecoder
import ru.tinkoff.phobos.derivation
import ru.tinkoff.phobos.syntax.attr

final case class Include(@attr schemaLocation: String)

object Include {
  implicit val includeElementDecoder: ElementDecoder[Include] = derivation.deriveElementDecoder
}