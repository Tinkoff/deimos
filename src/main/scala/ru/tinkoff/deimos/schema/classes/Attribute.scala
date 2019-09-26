package ru.tinkoff.deimos.schema.classes

import ru.tinkoff.deimos.schema.classes.namespaces.xsd
import ru.tinkoff.phobos.decoding.ElementDecoder
import ru.tinkoff.phobos.derivation
import ru.tinkoff.phobos.syntax.{attr, xmlns}

final case class Attribute(
    @attr name: Option[String],
    @attr id: Option[String],
    @attr ref: Option[String],
    @attr `type`: Option[String],
    @attr use: Option[String],
    @attr fixed: Option[String],
    @attr form: Option[String],
    @xmlns(xsd) simpleType: Option[SimpleType],
) extends Global

object Attribute {
  implicit val attributeElementDecoder: ElementDecoder[Attribute] = derivation.deriveElementDecoder
}
