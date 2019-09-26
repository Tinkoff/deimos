package ru.tinkoff.deimos.schema.classes

import ru.tinkoff.phobos.decoding.ElementDecoder
import ru.tinkoff.phobos.derivation

final case class Union()

object Union {
  implicit val unionElementDecoder: ElementDecoder[Union] = derivation.deriveElementDecoder
}