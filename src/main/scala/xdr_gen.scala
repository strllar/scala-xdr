package org.strllar.stellarbase

import scala.reflect.macros.Context
import scala.language.experimental.macros

object xdr {
  trait SwitchArm
  def armType[T]:SwitchArm = new SwitchArm {}
  def unionSwitch[T](arms: SwitchArm*) {}

}

object xdr_generator {
  type TargetBuffer = Array[Byte]

  trait XDRCodec[T] {
    def toXDR(x:T):TargetBuffer
    def fromXDR(buf: TargetBuffer):T
  }

  def XDRGen[T]:XDRCodec[T] = macro XDRGenImpl[T]


  def XDRGenImpl[T: c.WeakTypeTag](c: Context) = {
    import c.universe._
//    def computeType[T: c.WeakTypeTag]: Type = {
//      val originalTpe = weakTypeOf[T]
//      originalTpe
//      // Note: this makes it so modules work, things like foo.type.
//      //       For some reason we get an issue with not having == defined on Class[_] otherwise.
//      // TODO - fix this for certain primitive types, like Null, etc.
//      if(originalTpe.termSymbol.isModule) originalTpe.widen
//      else originalTpe
//    }
    val tpe = weakTypeOf[T]
    val tpeSym: Symbol = tpe.typeSymbol.companionSymbol
    if (tpeSym == NoSymbol) {
      c.abort(c.enclosingPosition, s"cann't get companionSymbol of ${tpe.typeSymbol}, is it a inner class?")
    }

  c.Expr(
      q"""
          new org.strllar.stellarbase.xdr_generator.XDRCodec[$tpe]{
            override def toXDR(x:$tpe) = {
              println("encoding...");
              Array[Byte]()
            }
            override def fromXDR(buf: org.strllar.stellarbase.xdr_generator.TargetBuffer) = {${tpeSym}(10)}
          }
    """)
  }
}