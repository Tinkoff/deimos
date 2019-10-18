package ru.tinkoff.deimos.schema.classes

import ru.tinkoff.deimos.schema.classes.namespaces.xsd
import ru.tinkoff.phobos.decoding.ElementDecoder
import ru.tinkoff.phobos.derivation
import ru.tinkoff.phobos.syntax.{attr, xmlns}

final case class Extension(
  @attr base: String,
  @xmlns(xsd) choice: List[Choice],
  @xmlns(xsd) sequence: List[Sequence],
  @xmlns(xsd) group: List[Group],
  @xmlns(xsd) element: List[Element],
  @xmlns(xsd) all: List[All],
  @xmlns(xsd) attribute: List[Attribute],
  @xmlns(xsd) attributeGroup: List[AttributeGroup],
) extends Elements with Attributes

object Extension {
  implicit val extensionElementDecoder: ElementDecoder[Extension] = derivation.semiauto.deriveElementDecoder
}