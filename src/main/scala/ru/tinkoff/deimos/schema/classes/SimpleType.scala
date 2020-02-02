package ru.tinkoff.deimos.schema.classes

import ru.tinkoff.deimos.schema.classes.namespaces.xsd
import ru.tinkoff.phobos.decoding.ElementDecoder
import ru.tinkoff.phobos.derivation
import ru.tinkoff.phobos.syntax.{attr, xmlns}

final case class SimpleType(
    @attr name: Option[String],
    @attr id: Option[String],
    @xmlns(xsd) restriction: Option[Restriction],
    @xmlns(xsd) union: Option[Union],
    @xmlns(xsd) list: Option[XsdList],
) extends Global

object SimpleType {
  implicit val simpleTypeElementDecoder: ElementDecoder[SimpleType] = derivation.semiauto.deriveElementDecoder
}
