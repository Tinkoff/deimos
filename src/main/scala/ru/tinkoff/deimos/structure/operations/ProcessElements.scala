package ru.tinkoff.deimos.structure.operations

import cats.syntax.flatMap._
import cats.syntax.functor._

import ru.tinkoff.deimos.schema.classes.{All, Choice, Element, Elements, Group, Sequence}
import ru.tinkoff.deimos.structure._

object ProcessElements {
  def processElements(elements: List[Element]): XsdMonad[List[Tag]] =
    XsdMonad.traverse(elements)(ProcessLocalElement.apply).map(_.flatten)

  def processChoices(choices: List[Choice]): XsdMonad[List[Tag]] =
    XsdMonad
      .traverse(choices) { choice =>
        for {
          rawNewTags <- ProcessElements(choice)
          minOccurs  = choice.minOccurs
          maxOccurs  = choice.maxOccurs
          newTags = (minOccurs, maxOccurs) match {
            case (_, Some("unbounded"))      => rawNewTags.map(element => element.copy(typ = element.typ.toListing))
            case (_, Some(a)) if a.toInt > 1 => rawNewTags.map(element => element.copy(typ = element.typ.toListing))
            case (Some(0), _)                => rawNewTags.map(element => element.copy(typ = element.typ.toOptional))
            case _                           => rawNewTags.map(element => element.copy(typ = element.typ.toOptional))
          }
        } yield newTags
      }
      .map(_.flatten)

  def processGroup(groups: List[Group]): XsdMonad[List[Tag]] =
    XsdMonad
      .traverse(groups) { group =>
        for {
          ctx <- XsdMonad.ctx
          (newFile, realGroup) = group.ref match {
            case Some(ref) =>
              ctx.indices.groups
                .getItem(ctx.availableFiles, ctx.toGlobalName(ref))
                .getOrElse(throw InvalidSchema(s"$ref refrences to nothing", ctx.currentPath))
            case None => (ctx.currentPath, group)
          }
          rawNewTags <- ProcessElements(realGroup).changeContext(_.copy(currentPath = newFile))
          minOccurs  = group.minOccurs.orElse(realGroup.minOccurs)
          maxOccurs  = group.maxOccurs.orElse(realGroup.maxOccurs)
          newTags = (minOccurs, maxOccurs) match {
            case (_, Some("unbounded"))      => rawNewTags.map(element => element.copy(typ = element.typ.toListing))
            case (_, Some(a)) if a.toInt > 1 => rawNewTags.map(element => element.copy(typ = element.typ.toListing))
            case (Some(0), _)                => rawNewTags.map(element => element.copy(typ = element.typ.toOptional))
            case _                           => rawNewTags
          }
        } yield newTags
      }
      .map(_.flatten)

  def processSequence(sequences: List[Sequence]): XsdMonad[List[Tag]] =
    XsdMonad
      .traverse(sequences) { sequence =>
        for {
          rawNewTags <- ProcessElements(sequence)
          minOccurs  = sequence.minOccurs
          maxOccurs  = sequence.maxOccurs
          newTags = (minOccurs, maxOccurs) match {
            case (_, Some("unbounded"))      => rawNewTags.map(element => element.copy(typ = element.typ.toListing))
            case (_, Some(a)) if a.toInt > 1 => rawNewTags.map(element => element.copy(typ = element.typ.toListing))
            case (Some(0), _)                => rawNewTags.map(element => element.copy(typ = element.typ.toOptional))
            case _                           => rawNewTags
          }
        } yield newTags
      }
      .map(_.flatten)

  def processAlls(alls: List[All]): XsdMonad[List[Tag]] =
    XsdMonad
      .traverse(alls) { all =>
        for {
          rawNewTags <- ProcessElements(all)
          newTags = if (all.minOccurs.contains(0)) {
            rawNewTags.map(element => element.copy(typ = element.typ.toOptional))
          } else {
            rawNewTags
          }
        } yield newTags
      }
      .map(_.flatten)

  def apply(block: Elements): XsdMonad[List[Tag]] = {
    for {
      ctx       <- XsdMonad.ctx
      elements  <- processElements(block.element)
      choices   <- processChoices(block.choice)
      groups    <- processGroup(block.group)
      sequences <- processSequence(block.sequence)
      alls      <- processAlls(block.all)
    } yield ctx.deduplicateParams(elements ++ choices ++ groups ++ sequences ++ alls)
  }
}
