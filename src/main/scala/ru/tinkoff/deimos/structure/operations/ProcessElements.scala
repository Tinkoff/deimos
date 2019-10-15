package ru.tinkoff.deimos.structure.operations

import ru.tinkoff.deimos.schema.classes.{All, Choice, Element, Elements, Group, Sequence}
import ru.tinkoff.deimos.structure._

object ProcessElements {
  def processElements(ctx: XsdContext)(elements: List[Element]): (List[Tag], GeneratedPackage) =
    elements.foldLeft((List.empty[Tag], ctx.generatedPackage)) {
      case ((oldTags, oldPackage), element) =>
        val (tag, newPackage) = ProcessLocalElement(ctx.copy(generatedPackage = oldPackage))(element)
        (tag.fold(oldTags)(tag => oldTags :+ tag), newPackage)
    }

  def processChoices(ctx: XsdContext)(choices: List[Choice]): (List[Tag], GeneratedPackage) =
    choices.foldLeft((List.empty[Tag], ctx.generatedPackage)) {
      case ((oldTags, oldPackage), choice) =>
        val (rawNewTags, newPackage) = ProcessElements(ctx.copy(generatedPackage = oldPackage))(choice)
        val minOccurs                = choice.minOccurs
        val maxOccurs                = choice.maxOccurs
        val newTags = (minOccurs, maxOccurs) match {
          case (_, Some("unbounded"))      => rawNewTags.map(element => element.copy(typ = element.typ.toListing))
          case (_, Some(a)) if a.toInt > 1 => rawNewTags.map(element => element.copy(typ = element.typ.toListing))
          case (Some(0), _)                => rawNewTags.map(element => element.copy(typ = element.typ.toOptional))
          case _                           => rawNewTags.map(element => element.copy(typ = element.typ.toOptional))
        }
        (oldTags ++ newTags, newPackage)
    }

  def processGroup(ctx: XsdContext)(groups: List[Group]): (List[Tag], GeneratedPackage) =
    groups.foldLeft((List.empty[Tag], ctx.generatedPackage)) {
      case ((oldTags, oldPackage), group) =>
        val (newFile, realGroup) = group.ref match {
          case Some(ref) =>
            ctx.indices.groups
              .getItem(ctx.availableFiles, ctx.toGlobalName(ref))
              .getOrElse(throw InvalidSchema(s"$ref refrences to nothing", ctx.operationContext))
          case None => (ctx.operationContext.currentPath, group)
        }
        val newCtx                   = ctx.copy(operationContext = OperationContext(newFile), generatedPackage = oldPackage)
        val (rawNewTags, newPackage) = ProcessElements(newCtx)(realGroup) // TODO: Check file here
        val minOccurs                = group.minOccurs.orElse(realGroup.minOccurs)
        val maxOccurs                = group.maxOccurs.orElse(realGroup.maxOccurs)
        val newTags = (minOccurs, maxOccurs) match {
          case (_, Some("unbounded"))      => rawNewTags.map(element => element.copy(typ = element.typ.toListing))
          case (_, Some(a)) if a.toInt > 1 => rawNewTags.map(element => element.copy(typ = element.typ.toListing))
          case (Some(0), _)                => rawNewTags.map(element => element.copy(typ = element.typ.toOptional))
          case _                           => rawNewTags
        }
        (oldTags ++ newTags, newPackage)
    }

  def processSequence(ctx: XsdContext)(sequences: List[Sequence]): (List[Tag], GeneratedPackage) =
    sequences.foldLeft((List.empty[Tag], ctx.generatedPackage)) {
      case ((oldTags, oldPackage), sequence) =>
        val (rawNewTags, newPackage) = ProcessElements(ctx.copy(generatedPackage = oldPackage))(sequence)
        val minOccurs                = sequence.minOccurs
        val maxOccurs                = sequence.maxOccurs
        val newTags = (minOccurs, maxOccurs) match {
          case (_, Some("unbounded"))      => rawNewTags.map(element => element.copy(typ = element.typ.toListing))
          case (_, Some(a)) if a.toInt > 1 => rawNewTags.map(element => element.copy(typ = element.typ.toListing))
          case (Some(0), _)                => rawNewTags.map(element => element.copy(typ = element.typ.toOptional))
          case _                           => rawNewTags
        }
        (oldTags ++ newTags, newPackage)
    }

  def processAlls(ctx: XsdContext)(alls: List[All]): (List[Tag], GeneratedPackage) =
    alls.foldLeft((List.empty[Tag], ctx.generatedPackage)) {
      case ((oldTags, oldPackage), all) =>
        val (rawNewTags, newPackage) = ProcessElements(ctx.copy(generatedPackage = oldPackage))(all)
        val newTags = if (all.minOccurs.contains(0)) {
          rawNewTags.map(element => element.copy(typ = element.typ.toOptional))
        } else {
          rawNewTags
        }
        (oldTags ++ newTags, newPackage)
    }

  def apply(ctx: XsdContext)(block: Elements): (List[Tag], GeneratedPackage) = {
    val (elements, elementsPackage)   = processElements(ctx)(block.element)
    val (choices, choicesPackage)     = processChoices(ctx.copy(generatedPackage = elementsPackage))(block.choice)
    val (groups, groupsPackage)       = processGroup(ctx.copy(generatedPackage = choicesPackage))(block.group)
    val (sequences, sequencesPackage) = processSequence(ctx.copy(generatedPackage = groupsPackage))(block.sequence)
    val (alls, allsPackage)           = processAlls(ctx.copy(generatedPackage = sequencesPackage))(block.all)

    (ctx.deduplicateParams(elements ++ choices ++ groups ++ sequences ++ alls), allsPackage)
  }
}
