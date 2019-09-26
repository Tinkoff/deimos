package ru.tinkoff.deimos.schema.classes

import ru.tinkoff.deimos.schema.classes.namespaces.xsd
import ru.tinkoff.phobos.decoding.ElementDecoder
import ru.tinkoff.phobos.derivation
import ru.tinkoff.phobos.syntax.{attr, xmlns}

final case class Element(
    @attr name: Option[String],
    @attr id: Option[String],
    @attr `type`: Option[String],
    @attr ref: Option[String],
    @attr minOccurs: Option[Int],
    @attr maxOccurs: Option[String],
    @attr nillable: Option[Boolean],
    @attr form: Option[String],
    @xmlns(xsd) simpleType: Option[SimpleType],
    @xmlns(xsd) complexType: Option[ComplexType],
) extends Global

object Element {
  implicit val elementElementDecoder: ElementDecoder[Element] = derivation.deriveElementDecoder
}
