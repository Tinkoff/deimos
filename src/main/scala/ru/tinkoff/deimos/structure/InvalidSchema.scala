package ru.tinkoff.deimos.structure

import ru.tinkoff.deimos.structure.operations.OperationContext

final case class InvalidSchema(msg: String, ctx: OperationContext) extends Throwable {
  override val getMessage: String = s"Schema is invalid: $msg, $ctx"
}
