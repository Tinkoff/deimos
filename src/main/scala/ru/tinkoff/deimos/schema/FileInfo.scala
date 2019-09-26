package ru.tinkoff.deimos.schema

import ru.tinkoff.deimos.schema.classes.Schema

final case class FileInfo(schema: Schema, namespaces: Map[String, String])