package ru.tinkoff.deimos.schema.classes

import ru.tinkoff.deimos.schema.classes.namespaces.xsd
import ru.tinkoff.phobos.decoding.ElementDecoder
import ru.tinkoff.phobos.derivation
import ru.tinkoff.phobos.syntax.{attr, xmlns}

final case class Sequence(
    @attr minOccurs: Option[Int],
    @attr maxOccurs: Option[String],
    @xmlns(xsd) element: List[Element],
    @xmlns(xsd) choice: List[Choice],
    @xmlns(xsd) group: List[Group],
    @xmlns(xsd) sequence: List[Sequence],
    @xmlns(xsd) all: List[All],
) extends Elements

object Sequence {
  implicit val sequenceElementDecoder: ElementDecoder[Sequence] = derivation.semiauto.deriveElementDecoder
}
