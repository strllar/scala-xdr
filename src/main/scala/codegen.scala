package org.strllar.scalaxdr

import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe._
import rfc4506._

package codegen {

  object ScalaScaffold {

    class Snippet(val pkgname :Option[String]) {
      val vStack = ListBuffer.empty[Tree]

      def mkAST = {
        pkgname.map( pn => {
          val pname = TermName(pn)
          q"""
            package $pname {..$vStack}
            """
        }).getOrElse(q"..$vStack")
      }

      def declare(tree :Tree): Unit = {
        vStack.append(tree)
      }
    }

    def newSnippet(predefs :Tree, pkgname :Option[String]): Snippet = new Snippet(pkgname) {
      if (predefs.isEmpty == false) vStack.append(predefs)
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

//        case XDRPlainTypedef(XDRPlainDeclaration(XDRIdentifierTypeSpecifier(XDRIdentifierLiteral(name)), XDRIdentifierLiteral(alias))) => {
//          val origtpe = TypeName(name)
//          val aliastpe = TypeName(alias)
//          c.declare(q"type $aliastpe = $origtpe")
//        }
//        case XDRPlainTypedef(XDRFixedLengthOpaque(XDRIdentifierLiteral(name), len)) => {
//          //todo value type
//          //todo codec type
//          //println(s"val $name:Vector[Byte] //fixlen: ${len.dig}")
//          //val lenref = c.refConstant(len) //todo
//        }
//        case XDRPlainTypedef(XDRVariableLengthOpaque(XDRIdentifierLiteral(name), olen)) => {
//          olen match {
//            case None => {
//              //println(s"val $name:Vector[Byte] //maxlen: -1")
//            }
//            case Some(len@XDRConstantValue(_)) => {
//              //println(s"val $name:Vector[Byte] //maxlen: ${len.dig}")
//            }
//          }
//        }
//        case XDREnumTypedef(XDRIdentifierLiteral(name), body) => {
//          reifyEnum(name, body)
//        }
//        case XDRStructTypedef(XDRIdentifierLiteral(name), body) => {
//          reifyStruct(name, body)
//        }
//        case XDRUnionTypedef(XDRIdentifierLiteral(name), body) => {
//          reifyUnion(name, body)
//        }
//      }
//    }

    import shapeless._

    type PrimaryType = XDRFixedLengthOpaque :+: XDRVariableLengthOpaque :+: XDRString :+: XDRVoid :+: XDRInteger.type :+: XDRUnsignedInteger.type :+: XDRHyper :+: XDRFloat.type  :+: XDRDouble.type  :+: XDRQuadruple.type :+: XDRBoolean.type :+: CNil
    type CompositeType = XDRFixedLengthArray :+: XDRVariableLengthArray :+: XDROptional :+: CNil
    type FlatType = PrimaryType :+: CompositeType
    type NestedType = XDREnumeration :+: XDRStructBody :+: XDRUnion :+: CNil
    type AllType = FlatType :+: NestedType

    type TypeOrRef = Either[XDRIdentifierLiteral, AllType]

    case class ASTNode(fields :Vector[(String, AllType)])
    case class ASTree(
                       typedefs :Vector[(String, TypeOrRef)],
                       constdefs :Vector[(String, XDRConstantLiteral)]
                     )

//    def mkASTNode(node :AllType) :ASTNode = {
//
//    }

    def reify(ast :ASTree)(implicit c:ScalaScaffold.Snippet) = {

    }

    def transConst(x :XDRConstant) :(String, XDRConstantLiteral) = {
      (x.name.ident, x.value)
    }

    def transType(x :XDRTypeDef) :(String, TypeOrRef) = {
      x match {
        case XDRPlainTypedef(XDRPlainDeclaration(XDRIdentifierTypeSpecifier(alias), XDRIdentifierLiteral(name))) => {
          (name, Left(alias))
        }
      }
      //TODO TODO
    }

    def mkASTRoot(specs :XDRSpecification) :ASTree = {
      val alldef = specs.defs.map(_.anydef).foldLeft(
        Vector.empty[(String, TypeOrRef)],
        Vector.empty[(String, XDRConstantLiteral)]
      )((y, x) => {
        x.fold(
          z => (y._1 :+ transType(z), y._2),
          z => (y._1, y._2 :+ transConst(z))
        )
      })
      ASTree.tupled(alldef)
    }

    def addDefines(ns :String, specs :XDRSpecification) = {
      implicit val src = ScalaScaffold.newSnippet(
        q"",
        Some(ns)
      )
      reify(mkASTRoot(specs))
      src.mkAST
    }
  }
}

package object codegen {

  def genAST(ns :String, specs :XDRSpecification) = {
    SemanticProcesser.addDefines(ns, specs)
  }

  def genScala(ns :String, spec :XDRSpecification) :scala.io.Source = {
    scala.io.Source.fromIterable(
      showCode(genAST(ns, spec)).toSeq
    )
  }
}