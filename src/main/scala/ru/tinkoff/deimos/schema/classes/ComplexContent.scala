package ru.tinkoff.deimos.schema.classes

import ru.tinkoff.deimos.schema.classes.namespaces.xsd
import ru.tinkoff.phobos.decoding.ElementDecoder
import ru.tinkoff.phobos.derivation
import ru.tinkoff.phobos.syntax.{attr, xmlns}

final case class ComplexContent(
    @attr complex: Option[Boolean],
    @xmlns(xsd) extension: Option[Extension],
    @xmlns(xsd) restriction: Option[Restriction]
)

object ComplexContent {
  implicit val complexContentElementDecoder: ElementDecoder[ComplexContent] = derivation.deriveElementDecoder
}
