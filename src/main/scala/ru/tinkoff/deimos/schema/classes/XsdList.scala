package ru.tinkoff.deimos.schema.classes

import ru.tinkoff.phobos.decoding.ElementDecoder
import ru.tinkoff.phobos.derivation

final case class XsdList()

object XsdList {
  implicit val xsdListElementDecoder: ElementDecoder[XsdList] = derivation.deriveElementDecoder
}