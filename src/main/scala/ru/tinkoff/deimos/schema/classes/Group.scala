package ru.tinkoff.deimos.schema.classes

import ru.tinkoff.deimos.schema.classes.namespaces.xsd
import ru.tinkoff.phobos.decoding.ElementDecoder
import ru.tinkoff.phobos.derivation
import ru.tinkoff.phobos.syntax.{attr, xmlns}

final case class Group(
    @attr minOccurs: Option[Int],
    @attr maxOccurs: Option[String],
    @attr ref: Option[String],
    @attr name: Option[String],
    @attr id: Option[String],
    @xmlns(xsd) all: List[All],
    @xmlns(xsd) choice: List[Choice],
    @xmlns(xsd) sequence: List[Sequence],
    @xmlns(xsd) group: List[Group],
    @xmlns(xsd) element: List[Element],
) extends Global with Elements

object Group {
  implicit val groupElementDecoder: ElementDecoder[Group] = derivation.deriveElementDecoder
}