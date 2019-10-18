package ru.tinkoff.deimos.structure.operations

import java.nio.file.Path

import cats.{Applicative, Eval, MonadError, Traverse}
import ru.tinkoff.deimos.structure.{GeneratedClass, GeneratedPackage, GlobalName, InvalidSchema, XmlCodecInfo, operations, simpleTypesMap}
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.foldable._
import cats.instances.list._

trait XsdMonad[A] { self =>
  def run(ctx: XsdContext): Either[InvalidSchema, (A, GeneratedPackage)]

  def changeContext(f: XsdContext => XsdContext): XsdMonad[A] =
    new XsdMonad[A] {
      def run(ctx: XsdContext): Either[InvalidSchema, (A, GeneratedPackage)] = self.run(f(ctx))
    }
}

case class XsdBase[A](wrapped: XsdContext => (A, GeneratedPackage)) extends XsdMonad[A] {
  def run(ctx: XsdContext): Either[InvalidSchema, (A, GeneratedPackage)] = Right(wrapped(ctx))
}

case class XsdTap[A](wrapped: XsdContext => A) extends XsdMonad[A] {
  def run(ctx: XsdContext): Either[InvalidSchema, (A, GeneratedPackage)] = Right((wrapped(ctx), ctx.generatedPackage))
}

case class XsdFailure[A](error: InvalidSchema) extends XsdMonad[A] {
  def run(ctx: XsdContext): Either[InvalidSchema, (A, GeneratedPackage)] = Left(error)
}

object XsdMonad {

  def ctx: XsdMonad[XsdContext] = XsdTap(identity)
  def failure[A](message: String): XsdMonad[A] =
    ctx.flatMap(ctx => XsdFailure(InvalidSchema(message, ctx.currentPath)))

  def pure[A](a: A): XsdMonad[A] = XsdBase[A](ctx => (a, ctx.generatedPackage))
  def unit: XsdMonad[Unit]       = pure(())

  def addClass(path: Path, globalName: GlobalName, clazz: GeneratedClass): XsdMonad[Unit] =
    XsdBase(ctx => ((), ctx.generatedPackage.addClass(path, globalName, clazz)))
  def addXmlCodec(path: Path, xmlCodec: XmlCodecInfo): XsdMonad[Unit] =
    XsdBase(ctx => ((), ctx.generatedPackage.addXmlCodec(path, xmlCodec)))


  def getSimpleTypeByName(globalName: GlobalName): XsdMonad[Option[String]] =
    for {
      ctx <- XsdMonad.ctx
      res <- simpleTypesMap
            .get(globalName)
            .map(simpleType => XsdMonad.pure(Some(simpleType)))
            .getOrElse(ctx.indices.simpleTypes.getItem(ctx.availableFiles, globalName) match {
              case Some((path, st)) => ProcessSimpleType(st).changeContext(_.copy(currentPath = path)).map(Some.apply)
              case None => XsdMonad.pure(None)
            })
    } yield res

  def getOrProcessClassName(name: GlobalName): XsdMonad[String] =
    XsdMonad.ctx.flatMap { ctx =>
      ctx.availableFiles.flatMap(ctx.generatedPackage.files.get).collectFirstSome(_.classes.get(name)) match {
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
                    .changeContext(_.copy(currentPath = newFile, stack = ctx.stack.push(name)))
                    .map(_.name)
                }
            }
            .getOrElse(XsdMonad.failure(s"Complex type $name not found"))
      }
    }

  def getOrProcessClass(name: GlobalName): XsdMonad[GeneratedClass] =
    XsdMonad.ctx.flatMap { ctx =>
      ctx.availableFiles.flatMap(ctx.generatedPackage.files.get).collectFirstSome(_.classes.get(name)) match {
        case Some(clazz) => XsdMonad.pure(clazz)
        case None =>
          ctx.indices.complexTypes
            .getItem(ctx.availableFiles, name)
            .map {
              case (newFile, ct) =>
                ProcessComplexType(ct, ctx.complexTypeRealName(ct, None), Some(name))
                  .changeContext(_.copy(currentPath = newFile, stack = ctx.stack.push(name)))
            }
            .getOrElse(XsdMonad.failure(s"Complex type $name not found"))
      }
    }

  def traverse[A, B](as: List[A])(f: A => XsdMonad[B]): XsdMonad[List[B]] =
    new XsdMonad[List[B]] {
      def run(ctx: XsdContext): Either[InvalidSchema, (List[B], GeneratedPackage)] =
        as.foldLeft[Either[InvalidSchema, (List[B], GeneratedPackage)]](Right((Nil, ctx.generatedPackage))) {
          (res, a) =>
            res match {
              case Right((bs, oldPackage)) =>
                f(a).run(ctx.copy(generatedPackage = oldPackage)).map { case (b, newPackage) => (b :: bs, newPackage) }
              case error @ Left(_) => error
            }
        }
    }

  implicit val xsdMonadError: MonadError[XsdMonad, InvalidSchema] =
    new MonadError[XsdMonad, InvalidSchema] {
      def pure[A](a: A): XsdMonad[A] = XsdMonad.pure(a)

      def flatMap[A, B](fa: XsdMonad[A])(f: A => XsdMonad[B]): XsdMonad[B] =
        new XsdMonad[B] {
          def run(ctx: XsdContext): Either[InvalidSchema, (B, GeneratedPackage)] = {
            fa.run(ctx) match {
              case Right((a, newPackage)) => f(a).run(ctx.copy(generatedPackage = newPackage))
              case Left(error)            => Left(error)
            }
          }
        }

      def tailRecM[A, B](a: A)(f: A => XsdMonad[Either[A, B]]): XsdMonad[B] = ???

      def raiseError[A](e: InvalidSchema): XsdMonad[A] = XsdFailure[A](e)

      def handleErrorWith[A](fa: XsdMonad[A])(f: InvalidSchema => XsdMonad[A]): XsdMonad[A] =
        new XsdMonad[A] {
          def run(ctx: XsdContext): Either[InvalidSchema, (A, GeneratedPackage)] =
            fa.run(ctx) match {
              case right @ Right(_) => right
              case Left(error)      => f(error).run(ctx)
            }
        }
    }
}
