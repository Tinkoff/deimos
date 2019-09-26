package ru.tinkoff.deimos.schema.classes

import ru.tinkoff.phobos.decoding.ElementDecoder
import ru.tinkoff.phobos.derivation
import ru.tinkoff.phobos.syntax.attr

final case class Restriction(@attr base: Option[String])

object Restriction {
  implicit val restrictionElementDecoder: ElementDecoder[Restriction] = derivation.deriveElementDecoder
}