package ru.tinkoff.deimos.schema.classes

import ru.tinkoff.deimos.schema.classes.namespaces.xsd
import ru.tinkoff.phobos.decoding.ElementDecoder
import ru.tinkoff.phobos.derivation
import ru.tinkoff.phobos.syntax.{attr, xmlns}

final case class AttributeGroup(
    @attr name: Option[String],
    @attr id: Option[String],
    @attr ref: Option[String],
    @xmlns(xsd) attribute: List[Attribute],
    @xmlns(xsd) attributeGroup: List[AttributeGroup],
) extends Global

object AttributeGroup {
  implicit val attributeGroupElementDecoder: ElementDecoder[AttributeGroup] = derivation.deriveElementDecoder
}