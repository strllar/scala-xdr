package org.strllar.scalaxdr

import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe._
import shapeless._
import rfc4506._

package codegen {

  object Extractors {
    //TODO: use Liftable instead of dig?
    implicit def extractConstant(const :XDRConstantValue) = new {
      def dig = const.value match {
          case DecimalConstant(x) => Integer.parseInt(x)
          case HexadecimalConstant(x) => Integer.parseInt(x.drop(2), 16)
          case OctalConstant(x) => Integer.parseInt(x, 8)
      }
    }
    implicit def extractIdentifier(ident :XDRIdentifierValue) = new {
      def dig = ident.ident.ident
    }
  }

  object ScalaScaffold {
    import Extractors._

    trait CodeBlock {
//      def declare(line :String)
//      def define(line :String)
//      def statement(line :String)
    }

    class Snippet(val pkgname :Option[String]) extends CodeBlock {
      val vStack = ListBuffer.empty[Tree]

      def source :scala.io.Source = {
        scala.io.Source.fromIterable(
          pkgname.map( pn => {
            val pname = TermName(pn)

            showCode(q"""
            package $pname { }
            package object $pname {..$vStack }
            """).toSeq
          }).getOrElse(showCode(q"..$vStack").toSeq)

        )
      }
      def declare(tree :Tree): Unit = {
        vStack.append(tree)
      }

      def refConstant(id :XDRIdentifierValue) = {
        Select(Ident(TermName("gConstants")), TermName(id.dig))
      }
    }

    def newSnippet(pkgname :String, predefs :Tree): Snippet = new Snippet(Some(pkgname)) {
      vStack.append(predefs)
    }
    //def newBlock(line :String) =
  }

  object processTypeDef {
    import Extractors._

    def reifyEnum(name :String, body :XDREnumBody)(implicit c:ScalaScaffold.Snippet) = {

      val stats =
      body.items.map(x => {
        val itemname = TypeName(x._1.ident)
        x._2 match {
            case v@XDRConstantValue(_) => {
              q"case class $itemname() extends Enum(..${List(v.dig)})"
            }
            case x@XDRIdentifierValue(_) => {
              val identref = c.refConstant(x)
              q"case class $itemname() extends Enum(..${List(identref)})"
            }
        }
      })

      c.declare(q"object ${TermName(name)} {abstract class Enum(val value :Int);..$stats} ")
    }
    def reifyStruct(name :String, body :XDRStructBody)(implicit c:ScalaScaffold.Snippet) = {

    }
    def reifyUnion(name :String, body :XDRUnionBody)(implicit c:ScalaScaffold.Snippet) = {

    }
    def apply(x :XDRTypeDef)(implicit c:ScalaScaffold.Snippet) = {
      x match {
        case XDRPlainTypedef(XDRPlainDeclaration(XDRIdentifierTypeSpecifier(XDRIdentifierLiteral(name)), XDRIdentifierLiteral(alias))) => {
          val origtpe = TypeName(name)
          val aliastpe = TypeName(alias)
          c.declare(q"type $aliastpe = $origtpe")
        }
        case XDRPlainTypedef(XDRFixedLengthOpaque(XDRIdentifierLiteral(name), len@XDRConstantValue(_))) => {
          //todo value type
          //todo codec type
          println(s"val $name:Vector[Byte] //fixlen: ${len.dig}")
        }
        case XDRPlainTypedef(XDRVariableLengthOpaque(XDRIdentifierLiteral(name), olen)) => {
          olen match {
            case None => println(s"val $name:Vector[Byte] //maxlen: -1")
            case Some(len@XDRConstantValue(_)) => println(s"val $name:Vector[Byte] //maxlen: ${len.dig}")
          }
        }
        case XDREnumTypedef(XDRIdentifierLiteral(name), body) => {
          reifyEnum(name, body)
        }
        case XDRStructTypedef(XDRIdentifierLiteral(name), _) => {
          println(s"struct $name")
        }
        case _ => {
          //todo
        }
      }
    }
  }

  object processConstDef {
    def apply(x :XDRConstant)(implicit c:ScalaScaffold.Snippet) = {
      x match {
        case XDRConstant(XDRIdentifierLiteral(name), DecimalConstant(value)) => {
          println(s"val $name = $value")
        }
        case XDRConstant(XDRIdentifierLiteral(name), HexadecimalConstant(value)) => {
          println(s"val $name = $value")
        }
        case XDRConstant(XDRIdentifierLiteral(name), OctalConstant(value)) => {
          println(s"val $name = $value")
        }
      }
    }
  }

  object processDeclaration{
    def apply(x :XDRDeclaration) = {

    }
  }

}

package object codegen {
  def genScala(ns :String, spec :XDRSpecification) = {

    implicit val src = ScalaScaffold.newSnippet("xdr_generated",
      q"{ import org.strllar.scalaxdr.xdrbase._ }"
    )
    spec.defs.map(_.anydef).foreach( x => {
      x.fold(
        processTypeDef(_),
        processConstDef(_)
      )
    })
    src.source
  }
}