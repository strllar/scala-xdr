package org.strllar.stellarbase

import scala.reflect.macros.Context
import scala.language.experimental.macros

object xdr_generator {
  def XDRGen[T] = macro XDRGenImpl[T]
  def XDRGenImpl[T: c.WeakTypeTag](c: Context) : c.Expr[{def decode():Unit}] = {
    c.universe.reify(
      new {
        def decode() {println("decoding...")}
      }
    )
  }
}