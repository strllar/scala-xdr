package org.strllar.scalaxdr.xdrbase

import scala.language.experimental.macros
import scala.annotation.{StaticAnnotation, ClassfileAnnotation, compileTimeOnly}
//import scala.reflect.macros.{Universe, blackbox}
import scala.meta._

import java.nio.{ByteBuffer, ByteOrder}

@compileTimeOnly("enable macro paradise3 to expand macro annotations")
object annotations {
//  def xdrfileImpl(c: blackbox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
//    import c.universe._
//    c.info(c.enclosingPosition, "hello from macro", true)
//    annottees.foreach(s => println(showRaw(s)))
//    annottees(0)
//  }

  final class xdrfile(val ns :String) extends StaticAnnotation {
    //def macroTransform(annottees: Any*): Any = macro xdrfileImpl
//    inline def apply(defn: Any): Any = meta {
//      abort("@xdrfile must annotate an object.")
//    }
  }
}

//class MacroBundle(val c: blackbox.Context) {
//  import c.universe._
//  def fromBytesImpl[T: c.WeakTypeTag](bs: c.Expr[Seq[Byte]]) :c.Expr[T] = {
//    val tpe = weakTypeOf[T]
//    c.Expr[T](q"""new $tpe()""")
//  }
//
//  def toBytesImpl[T: c.WeakTypeTag](v: c.Expr[T]) :c.Expr[ByteBuffer] = {
//    c.Expr[ByteBuffer](q"""java.nio.ByteBuffer.allocate(1)""")
//  }
//
//  def checkAnnotationsImpl[T: c.WeakTypeTag] :c.Expr[Unit] = {
//    weakTypeOf[T].typeSymbol.asClass.annotations.foreach( an => {
//      println(an, showRaw(an.tree))
//    })
//    c.Expr[Unit](q"")
//  }
//}

object XDRCodec {
//  def fromBytes[T](bs :Seq[Byte]) :T = macro MacroBundle.fromBytesImpl[T]
//  def toBytes[T](v :T) :ByteBuffer = macro MacroBundle.toBytesImpl[T]
//  def checkAnnotations[T] :Unit = macro MacroBundle.checkAnnotationsImpl[T]
}

