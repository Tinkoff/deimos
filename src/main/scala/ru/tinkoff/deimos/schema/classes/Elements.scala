package ru.tinkoff.deimos.schema.classes

trait Elements {
  val choice: List[Choice]
  val sequence: List[Sequence]
  val group: List[Group]
  val element: List[Element]
  val all: List[All]
}
