package org.strllar.scalaxdr.xdrbase

import scala.language.experimental.macros
import scala.annotation.{StaticAnnotation, ClassfileAnnotation, compileTimeOnly}
import scala.reflect.macros.{Universe, blackbox}

import java.nio.{ByteBuffer, ByteOrder}

@compileTimeOnly("enable macro paradise to expand macro annotations")
object annotations {
  def xdrfileImpl(c: blackbox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    c.info(c.enclosingPosition, "hello from macro", true)
    annottees.foreach(s => println(showRaw(s)))
    annottees(0)
  }

  final class xdrfile extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro xdrfileImpl
  }
}

object XDRCodec {
}

