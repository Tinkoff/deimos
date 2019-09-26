package ru.tinkoff.deimos.structure.operations

import ru.tinkoff.deimos.schema.classes.Elements
import ru.tinkoff.deimos.structure._

object ProcessElements {
  def apply(ctx: XsdContext)(block: Elements): List[Tag] = {
    val elements = block.element.map(element => ProcessElement(ctx)(element))

    val choices = block.choice.flatMap { choice =>
      val elements  = ProcessElements(ctx)(choice)
      val minOccurs = choice.minOccurs
      val maxOccurs = choice.maxOccurs
      (minOccurs, maxOccurs) match {
        case (_, Some("unbounded"))      => elements.map(element => element.copy(typ = element.typ.toListing))
        case (_, Some(a)) if a.toInt > 1 => elements.map(element => element.copy(typ = element.typ.toListing))
        case (Some(0), _)                => elements.map(element => element.copy(typ = element.typ.toOptional))
        case _                           => elements.map(element => element.copy(typ = element.typ.toOptional))
      }
    }

    val groups = block.group.flatMap { group =>
      val (newFile, realGroup) = group.ref match {
        case Some(ref) =>
          ctx.indices.groups
            .getItem(ctx.availableFiles, ctx.toGlobalName(ref))
            .getOrElse(throw InvalidSchema(s"$ref refrences to nothing", ctx.operationContext))
        case None => (ctx.operationContext.currentPath, group)
      }
    val elements  = ProcessElements(ctx.copy(OperationContext(newFile)))(realGroup) // TODO: Check file here
    val minOccurs = group.minOccurs.orElse(realGroup.minOccurs)
      val maxOccurs = group.maxOccurs.orElse(realGroup.maxOccurs)
      (minOccurs, maxOccurs) match {
        case (_, Some("unbounded"))      => elements.map(element => element.copy(typ = element.typ.toListing))
        case (_, Some(a)) if a.toInt > 1 => elements.map(element => element.copy(typ = element.typ.toListing))
        case (Some(0), _)                => elements.map(element => element.copy(typ = element.typ.toOptional))
        case _                           => elements
      }
    }

    val sequences = block.sequence.flatMap { sequence =>
      val elements  = ProcessElements(ctx)(sequence)
      val minOccurs = sequence.minOccurs
      val maxOccurs = sequence.maxOccurs
      (minOccurs, maxOccurs) match {
        case (_, Some("unbounded"))      => elements.map(element => element.copy(typ = element.typ.toListing))
        case (_, Some(a)) if a.toInt > 1 => elements.map(element => element.copy(typ = element.typ.toListing))
        case (Some(0), _)                => elements.map(element => element.copy(typ = element.typ.toOptional))
        case _                           => elements
      }
    }

    val alls = block.all.flatMap { all =>
      val elements = ProcessElements(ctx)(all)
      if (all.minOccurs.contains(0)) {
        elements.map(element => element.copy(typ = element.typ.toOptional))
      } else {
        elements
      }
    }

    ctx.deduplicateParams(elements.flatten ++ choices ++ groups ++ sequences ++ alls)
  }
}
