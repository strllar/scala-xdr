package org.strllar.scalaxdr

import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe._
import shapeless._
import rfc4506._

package codegen {

  object ScalaScaffold {

    class Snippet(val pkgname :Option[String]) {
      val vStack = ListBuffer.empty[Tree]

      def mkAST = {
        pkgname.map( pn => {
          val pname = TermName(pn)
          q"""
            package $pname { }
            package object $pname {..$vStack }
            """
        }).getOrElse(q"..$vStack")
      }

      def declare(tree :Tree): Unit = {
        vStack.append(tree)
      }
    }

    def newSnippet(pkgname :String, predefs :Tree): Snippet = new Snippet(Some(pkgname)) {
      vStack.append(predefs)
    }
  }

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

  object SemanticProcesser {
    import Extractors._

    def reifyDeclaration(xdecl :XDRDeclaration)(implicit c:ScalaScaffold.Snippet)  {
      xdecl match {
        case x@XDRPlainDeclaration(_,_) => {
          parseTypeSpec(x.typespec, x.name)
        }
        case x@XDRFixedLengthArray(_, _, _) => {
          parseTypeSpec(x.typespec, x.name)
        }
        case x@XDRVariableLengthArray(_, _, _) => {
          parseTypeSpec(x.typespec, x.name)
        }
        case x@XDROptional(_, _) => {
          parseTypeSpec(x.typespec, x.name)
        }
        case x@XDRFixedLengthOpaque(_, _) => {

        }
        case x@XDRVariableLengthOpaque(_, _) => {

        }
        case x@XDRString(_, _) => {

        }
        case x@XDRVoid() => {

        }
      }
    }
    def parseTypeSpec (tpe :XDRTypeSpecifier, name :XDRIdentifierLiteral)(implicit c:ScalaScaffold.Snippet) = {
      tpe match {
        case x@XDRInteger => {}
        case x@XDRUnsignedInteger => {}
        case x@XDRHyper(_) => {}
        case x@XDRFloat => {}
        case x@XDRDouble => {}
        case x@XDRQuadruple => {}
        case x@XDRBoolean => {}
        case x@XDREnumeration(_) => {
          reifyEnum("anon_enum_"+name.ident, x.body)
        }
        case x@XDRStructure(_) => {
          reifyStruct("anon_enum"+name.ident, x.body)
        }
        case x@XDRUnion(_) => {
          reifyUnion("anon_enum"+name.ident, x.body)
        }
        case x@XDRIdentifierTypeSpecifier(_) => {}
      }
    }

    def refConstant(id :String) = {
        Select(Ident(TermName("gConstants")), TermName(id))
    }
    def reifyEnum(name :String, body :XDREnumBody)(implicit c:ScalaScaffold.Snippet) = {

      //val enum = c.defineEnum(name) //todo

      val stats =
      body.items.map(x => {
        val itemname = TypeName(x._1.ident)
        x._2 match {
            case v@XDRConstantValue(_) => {
              //enum.defineValue(itemname, v.dig) //todo
              q"case class $itemname() extends Enum(..${List(v.dig)})"
            }
            case x@XDRIdentifierValue(_) => {
              val identref = refConstant(x.dig)
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

    def processConstDef(x :XDRConstant)(implicit c:ScalaScaffold.Snippet) = {
      x match {
        case XDRConstant(XDRIdentifierLiteral(name), DecimalConstant(value)) => {
          //println(s"val $name = $value")
        }
        case XDRConstant(XDRIdentifierLiteral(name), HexadecimalConstant(value)) => {
          //println(s"val $name = $value")
        }
        case XDRConstant(XDRIdentifierLiteral(name), OctalConstant(value)) => {
          //println(s"val $name = $value")
        }
      }
    }

    def processTypeDef(x :XDRTypeDef)(implicit c:ScalaScaffold.Snippet) = {
      x match {
        case XDRPlainTypedef(XDRPlainDeclaration(XDRIdentifierTypeSpecifier(XDRIdentifierLiteral(name)), XDRIdentifierLiteral(alias))) => {
          val origtpe = TypeName(name)
          val aliastpe = TypeName(alias)
          c.declare(q"type $aliastpe = $origtpe")
        }
        case XDRPlainTypedef(XDRFixedLengthOpaque(XDRIdentifierLiteral(name), len)) => {
          //todo value type
          //todo codec type
          //println(s"val $name:Vector[Byte] //fixlen: ${len.dig}")
          //val lenref = c.refConstant(len) //todo
        }
        case XDRPlainTypedef(XDRVariableLengthOpaque(XDRIdentifierLiteral(name), olen)) => {
          olen match {
            case None => {
              //println(s"val $name:Vector[Byte] //maxlen: -1")
            }
            case Some(len@XDRConstantValue(_)) => {
              //println(s"val $name:Vector[Byte] //maxlen: ${len.dig}")
            }
          }
        }
        case XDREnumTypedef(XDRIdentifierLiteral(name), body) => {
          reifyEnum(name, body)
        }
        case XDRStructTypedef(XDRIdentifierLiteral(name), body) => {
          reifyStruct(name, body)
        }
        case XDRUnionTypedef(XDRIdentifierLiteral(name), body) => {
          reifyUnion(name, body)
        }
        case _ => {
          //todo
        }
      }
    }
  }
}

package object codegen {

  def genAST(ns :String, spec :XDRSpecification) = {
    implicit val src = ScalaScaffold.newSnippet("xdr_generated",
      q"{ import org.strllar.scalaxdr.xdrbase._ }"
    )
    spec.defs.map(_.anydef).foreach( x => {
      x.fold(
        SemanticProcesser.processTypeDef(_),
        SemanticProcesser.processConstDef(_)
      )
    })
    src.mkAST
  }

  def genScala(ns :String, spec :XDRSpecification) :scala.io.Source = {
    scala.io.Source.fromIterable(
      showCode(genAST(ns, spec)).toSeq
    )
  }
}