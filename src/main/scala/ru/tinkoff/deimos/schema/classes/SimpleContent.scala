package ru.tinkoff.deimos.schema.classes

import ru.tinkoff.deimos.schema.classes.namespaces.xsd
import ru.tinkoff.phobos.decoding.ElementDecoder
import ru.tinkoff.phobos.derivation
import ru.tinkoff.phobos.syntax.xmlns

final case class SimpleContent(
    @xmlns(xsd) extension: Option[Extension],
    @xmlns(xsd) restriction: Option[Restriction],
)

object SimpleContent {
  implicit val simpleContentElementDecoder: ElementDecoder[SimpleContent] = derivation.semiauto.deriveElementDecoder
}
