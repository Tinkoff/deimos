package ru.tinkoff.deimos.structure.operations

import ru.tinkoff.deimos.schema.classes.SimpleType
import ru.tinkoff.deimos.structure._

object ProcessSimpleType {
  def apply(ctx: XsdContext)(simpleType: SimpleType): String = {
    simpleType match { // TODO
      case _ if simpleType.restriction.isDefined =>
        simpleType.restriction.get.base
          .flatMap(base => simpleTypesMap.get(ctx.toGlobalName(base)))
          .getOrElse("String")
      case _ =>
        "String"
    }
  }
}
