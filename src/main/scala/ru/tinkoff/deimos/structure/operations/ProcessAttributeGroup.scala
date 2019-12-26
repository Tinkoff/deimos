package ru.tinkoff.deimos.structure.operations

import ru.tinkoff.deimos.schema.classes.AttributeGroup
import ru.tinkoff.deimos.structure._
import cats.syntax.functor._
import cats.syntax.flatMap._

object ProcessAttributeGroup {
  def apply(attributeGroup: AttributeGroup): XsdMonad[List[Attr]] =
    for {
      ctx <- XsdMonad.ask
      (path, realAttributeGroup) = attributeGroup.ref match {
        case Some(ref) =>
          ctx.indices.attributeGroups
            .getItem(ctx.availableFiles, ctx.toGlobalName(ref))
            .getOrElse(throw InvalidSchema(s"$ref references to nothing", ctx.currentPath))
        case None => (ctx.currentPath, attributeGroup)
      }
      attributes <- ProcessAttributes(realAttributeGroup).local(_.copy(currentPath = path))
    } yield attributes
}
