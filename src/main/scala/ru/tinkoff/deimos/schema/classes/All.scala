package ru.tinkoff.deimos.schema.classes

import ru.tinkoff.deimos.schema.classes.namespaces.xsd
import ru.tinkoff.phobos.decoding.ElementDecoder
import ru.tinkoff.phobos.derivation
import ru.tinkoff.phobos.syntax.{attr, xmlns}

final case class All(
    @attr minOccurs: Option[Int],
    @attr maxOccurs: Option[Int],
    @xmlns(xsd) element: List[Element],
    @xmlns(xsd) choice: List[Choice],
    @xmlns(xsd) sequence: List[Sequence],
    @xmlns(xsd) group: List[Group],
    @xmlns(xsd) all: List[All],
) extends Elements

object All {
  implicit val allElementDecoder: ElementDecoder[All] = derivation.semiauto.deriveElementDecoder
}