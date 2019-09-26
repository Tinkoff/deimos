package ru.tinkoff.deimos.structure

import scala.collection.mutable

import cats.syntax.foldable._
import cats.instances.list._

final class ImportsIndex[File, K, V] {
  private val index: mutable.HashMap[File, mutable.HashMap[K, V]] = mutable.HashMap.empty

  def add(file: File, key: K, v: V): Unit = {
    if (!index.contains(file)) {
      index.put(file, mutable.HashMap(key -> v))
    } else {
      index(file).put(key, v)
    }
  }

  def get(file: File, key: K): Option[V] =
    index.get(file).flatMap(_.get(key))

  def get(files: List[File], key: K): Option[V] = {
    files.collectFirstSome(file => get(file, key))
  }

  def getFile(files: List[File], key: K): Option[File] = {
    files.collectFirstSome(file => get(file, key).map(_ => file))
  }

  def getItem(files: List[File], key: K): Option[(File, V)] = {
    files.collectFirstSome(file => get(file, key).map(value => (file, value)))
  }

  def items: List[(File, List[V])] = index.toList.map { case (k, v) => k -> v.values.toList }

  def map[W](f: V => W): ImportsIndex[File, K, W] = {
    val newIndex = new ImportsIndex[File, K, W]
    index.foreach { case (file, contents) =>
      contents.foreach {
        case (key, value) =>
          newIndex.add(file, key, f(value))
      }

    }
    newIndex
  }

  override def toString: String = index.toString()
}
