package ru.tinkoff.deimos.structure.operations

import java.nio.file.Path

import cats.{Applicative, Eval, MonadError, Traverse}
import ru.tinkoff.deimos.structure.{
  GeneratedClass,
  GeneratedPackage,
  GlobalName,
  InvalidSchema,
  XmlCodecInfo,
  operations,
  simpleTypesMap
}
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.foldable._
import cats.instances.list._

trait XsdMonad[A] {
  self =>
  def run(ctx: XsdContext, state: GeneratedPackage): Eval[Either[InvalidSchema, (A, GeneratedPackage)]]

  def local(f: XsdContext => XsdContext): XsdMonad[A] =
    new XsdMonad[A] {
      def run(ctx: XsdContext, state: GeneratedPackage): Eval[Either[InvalidSchema, (A, GeneratedPackage)]] =
        self.run(f(ctx), state)
    }
}

case class XsdSuccess[A](successful: (XsdContext, GeneratedPackage) => Eval[(A, GeneratedPackage)])
  extends XsdMonad[A] {
  def run(ctx: XsdContext, state: GeneratedPackage): Eval[Either[InvalidSchema, (A, GeneratedPackage)]] =
    successful(ctx, state).map(Right.apply)
}

case class XsdFailure[A](error: InvalidSchema) extends XsdMonad[A] {
  def run(ctx: XsdContext, state: GeneratedPackage): Eval[Either[InvalidSchema, (A, GeneratedPackage)]] =
    Eval.now(Left(error))
}

object XsdMonad {

  def ask: XsdMonad[XsdContext] =
    XsdSuccess((ctx, generatedPackage) => Eval.now((ctx, generatedPackage)))

  def modify(f: GeneratedPackage => GeneratedPackage): XsdMonad[Unit] =
    XsdSuccess((ctx, generatedPackage) => Eval.now(((), f(generatedPackage))))

  def raiseError[A](message: String): XsdMonad[A] =
    ask.flatMap(ctx => XsdFailure(InvalidSchema(message, ctx.currentPath)))

  def get: XsdMonad[GeneratedPackage] =
    XsdSuccess((ctx, generatedPackage) => Eval.now((generatedPackage, generatedPackage)))

  def pure[A](a: A): XsdMonad[A] =
    XsdSuccess[A]((ctx, generatedPackage) => Eval.now((a, generatedPackage)))

  def unit: XsdMonad[Unit] =
    pure(())

  def addClass(path: Path, globalName: GlobalName, clazz: GeneratedClass): XsdMonad[Unit] =
    XsdMonad.modify(_.addClass(path, globalName, clazz))

  def addXmlCodec(path: Path, xmlCodec: XmlCodecInfo): XsdMonad[Unit] =
    XsdMonad.modify(_.addXmlCodec(path, xmlCodec))

  def getSimpleTypeByName(globalName: GlobalName): XsdMonad[Option[String]] =
    for {
      ctx <- XsdMonad.ask
      res <- simpleTypesMap
        .get(globalName)
        .map(simpleType => XsdMonad.pure(Some(simpleType)))
        .getOrElse(ctx.indices.simpleTypes.getItem(ctx.availableFiles, globalName) match {
          case Some((path, st)) => ProcessSimpleType(st).local(_.copy(currentPath = path)).map(Some.apply)
          case None => XsdMonad.pure(None)
        })
    } yield res

  def getOrProcessClassName(name: GlobalName): XsdMonad[String] =
    for {
      ctx <- XsdMonad.ask
      generatedPackage <- XsdMonad.get
      name <- ctx.availableFiles.flatMap(generatedPackage.files.get).collectFirstSome(_.classes.get(name)) match {
        case Some(clazz) => XsdMonad.pure(clazz.name)
        case None =>
          ctx.indices.complexTypes
            .getItem(ctx.availableFiles, name)
            .map {
              case (newFile, ct) =>
                val className = ctx.complexTypeRealName(ct, None)
                if (ctx.stack.contains(name)) {
                  XsdMonad.pure(className)
                } else {
                  ProcessComplexType(ct, className, Some(name))
                    .local(_.copy(currentPath = newFile, stack = ctx.stack.push(name)))
                    .map(_.name)
                }
            }
            .getOrElse(XsdMonad.raiseError(s"Complex type $name not found"))
      }
    } yield name

  def getOrProcessClass(name: GlobalName): XsdMonad[GeneratedClass] =
    for {
      ctx <- XsdMonad.ask
      generatedPackage <- XsdMonad.get
      clazz <- ctx.availableFiles.flatMap(generatedPackage.files.get).collectFirstSome(_.classes.get(name)) match {
        case Some(clazz) => XsdMonad.pure(clazz)
        case None =>
          ctx.indices.complexTypes
            .getItem(ctx.availableFiles, name)
            .map {
              case (newFile, ct) =>
                ProcessComplexType(ct, ctx.complexTypeRealName(ct, None), Some(name))
                  .local(_.copy(currentPath = newFile, stack = ctx.stack.push(name)))
            }
            .getOrElse(XsdMonad.raiseError(s"Complex type $name not found"))
      }
    } yield clazz

  def traverse[A, B](as: List[A])(f: A => XsdMonad[B]): XsdMonad[List[B]] =
    new XsdMonad[List[B]] {
      def run(ctx: XsdContext, state: GeneratedPackage): Eval[Either[InvalidSchema, (List[B], GeneratedPackage)]] =
        as.foldLeft[Eval[Either[InvalidSchema, (List[B], GeneratedPackage)]]](Eval.now(Right((Nil, state)))) {
          (res, a) =>
            res.flatMap {
              case Right((bs, oldPackage)) =>
                f(a).run(ctx, oldPackage).map {
                  case Right((b, newPackage)) => Right(b :: bs, newPackage)
                  case Left(error) => Left(error)
                }
              case error@Left(_) => Eval.now(error)
            }
        }
    }

  implicit val xsdMonadError: MonadError[XsdMonad, InvalidSchema] =
    new MonadError[XsdMonad, InvalidSchema] {
      def pure[A](a: A): XsdMonad[A] = XsdMonad.pure(a)

      def flatMap[A, B](fa: XsdMonad[A])(f: A => XsdMonad[B]): XsdMonad[B] =
        new XsdMonad[B] {
          def run(ctx: XsdContext, state: GeneratedPackage): Eval[Either[InvalidSchema, (B, GeneratedPackage)]] = {
            fa.run(ctx, state).flatMap {
              case Right((a, newPackage)) => f(a).run(ctx, newPackage)
              case Left(error) => Eval.now(Left(error))
            }
          }
        }

      def tailRecM[A, B](a: A)(f: A => XsdMonad[Either[A, B]]): XsdMonad[B] = ???

      def raiseError[A](e: InvalidSchema): XsdMonad[A] = XsdFailure[A](e)

      def handleErrorWith[A](fa: XsdMonad[A])(f: InvalidSchema => XsdMonad[A]): XsdMonad[A] =
        new XsdMonad[A] {
          def run(ctx: XsdContext, state: GeneratedPackage): Eval[Either[InvalidSchema, (A, GeneratedPackage)]] =
            fa.run(ctx, state).flatMap {
              case right@Right(_) => Eval.later(right)
              case Left(error) => f(error).run(ctx, state)
            }
        }
    }
}
