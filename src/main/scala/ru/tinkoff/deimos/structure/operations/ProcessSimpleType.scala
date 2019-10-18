package ru.tinkoff.deimos.structure.operations

import cats.syntax.functor._

import ru.tinkoff.deimos.schema.classes.SimpleType
import ru.tinkoff.deimos.structure._

object ProcessSimpleType {
  def apply(simpleType: SimpleType): XsdMonad[String] = {
    XsdMonad.ctx.map(ctx =>
      simpleType match { // TODO
        case _ if simpleType.restriction.isDefined =>
          simpleType.restriction.get.base
            .map(base => simpleTypesMap.getOrElse(ctx.toGlobalName(base), "String"))
            .getOrElse("String")
        case _ =>
          "String"
    })
  }
}
