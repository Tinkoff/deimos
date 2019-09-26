package ru.tinkoff.deimos.structure.operations

import ru.tinkoff.deimos.schema.classes.AttributeGroup
import ru.tinkoff.deimos.structure._

object ProcessAttributeGroup {
  def apply(ctx: XsdContext)(attributeGroup: AttributeGroup): List[Attr] = {
    val (path, realAttributeGroup) = attributeGroup.ref match {
      case Some(ref) =>
        ctx.indices.attributeGroups
          .getItem(ctx.availableFiles, ctx.toGlobalName(ref))
          .getOrElse(throw InvalidSchema(s"$ref references to nothing", ctx.operationContext))
      case None => (ctx.operationContext.currentPath, attributeGroup)
    }

    val newCtx = ctx.copy(ctx.operationContext.copy(currentPath = path))
    realAttributeGroup.attribute.map(ProcessAttribute(newCtx)) ++
      realAttributeGroup.attributeGroup.flatMap(ProcessAttributeGroup(newCtx))
  }
}
