package ru.tinkoff.deimos.schema.classes

import ru.tinkoff.deimos.schema.classes.namespaces.xsd
import ru.tinkoff.phobos.decoding.ElementDecoder
import ru.tinkoff.phobos.derivation
import ru.tinkoff.phobos.syntax.{attr, xmlns}

final case class ComplexType(
    @attr name: Option[String],
    @attr id: Option[String],
    @attr mixed: Option[Boolean],
    @xmlns(xsd) attribute: List[Attribute],
    @xmlns(xsd) simpleContent: Option[SimpleContent],
    @xmlns(xsd) complexContent: Option[ComplexContent],
    @xmlns(xsd) all: List[All],
    @xmlns(xsd) choice: List[Choice],
    @xmlns(xsd) sequence: List[Sequence],
    @xmlns(xsd) element: List[Element],
    @xmlns(xsd) group: List[Group],
    @xmlns(xsd) attributeGroup: List[AttributeGroup],
) extends Global with Elements with Attributes

object ComplexType {
  implicit val complexTypeElementDecoder: ElementDecoder[ComplexType] = derivation.semiauto.deriveElementDecoder
}
