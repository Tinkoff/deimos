package ru.tinkoff.deimos.schema.classes

trait Attributes {
  def attributeGroup: List[AttributeGroup]
  def attribute: List[Attribute]
}
