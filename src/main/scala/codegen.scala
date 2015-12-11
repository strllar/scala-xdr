package org.strllar.scalaxdr

import rfc4506._
package codegen {
  object Extractor {
    def extractConstant(const :XDRConstantLiteral) :String = {
      const match {
        case DecimalConstant(value) => value
        case HexadecimalConstant(value) => value
        //case OctalConstant(value) => value
      }
    }
  }
}

package object codegen {
  def genScala(ns :String, spec :XDRSpecification) = {
    val (tpedefs, constdefs) =  spec.defs.map(_.anydef).foldRight((Vector[XDRTypeDef](), Vector[XDRConstant]()))((e, p) => e.fold(l => (l +: p._1, p._2), r => (p._1, r +: p._2)))

    constdefs.map {
      case XDRConstant(XDRIdentifierLiteral(name), DecimalConstant(value)) => {
        s"val $name = $value"
      }
      case XDRConstant(XDRIdentifierLiteral(name), HexadecimalConstant(value)) => {
        s"val $name = $value"
      }
      case XDRConstant(XDRIdentifierLiteral(name), OctalConstant(value)) => {
        s"val $name = $value"
      }
    }.mkString("\n")

    tpedefs.collect {
      case XDRPlainTypedef(XDRPlainDeclaration(XDRIdentifierTypeSpecifier(XDRIdentifierLiteral(name)), XDRIdentifierLiteral(alias))) => {
        s"type $alias = $name"
      }
      case XDRPlainTypedef(XDRFixedLengthOpaque(XDRIdentifierLiteral(name), XDRConstantValue(len))) => {
        s"val $name:Vector[Byte] //fixlen: ${Extractor.extractConstant(len)}"
      }
      case XDRPlainTypedef(XDRVariableLengthOpaque(XDRIdentifierLiteral(name), olen)) => {
        olen match {
          case None => s"val $name:Vector[Byte] //maxlen: -1"
          case Some(XDRConstantValue(len)) => s"val $name:Vector[Byte] //maxlen: ${Extractor.extractConstant(len)}"
        }
      }
      case XDRStructTypedef(XDRIdentifierLiteral(name), _) => {
        s"struct $name"
      }
      //case _ => ""
    }.mkString("\n")


  }
}