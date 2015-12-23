package org.strllar.scalaxdr

import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe._
import rfc4506._

import shapeless._
import shapeless.ops.coproduct.{Mapper}


package object codegen {

  type PrimaryType = XDRFixedLengthOpaque :+: XDRVariableLengthOpaque :+: XDRString :+: XDRVoid :+: XDRInteger.type :+: XDRUnsignedInteger.type :+: XDRHyper :+: XDRFloat.type  :+: XDRDouble.type  :+: XDRQuadruple.type :+: XDRBoolean.type :+: CNil
  type CompositeType = XDRFixedLengthArray :+: XDRVariableLengthArray :+: XDROptional :+: CNil
  type FlatType = PrimaryType :+: CompositeType :+: CNil
  type NestedType = XDREnumeration :+: XDRStructure :+: XDRUnion :+: CNil
  type AllType = FlatType :+: NestedType :+: CNil

  type TypeOrRef = Either[XDRIdentifierLiteral, AllType]

  case class ASTree(
                     typedefs :Vector[(String, TypeOrRef)],
                     constdefs :Vector[(String, XDRConstantLiteral)]
                   )


  //exposed util functions

  def genAST(ns :String, specs :XDRSpecification) = {
    SemanticProcesser.addDefines(ns, specs)
  }

  def genScala(ns :String, spec :XDRSpecification) :scala.io.Source = {
    scala.io.Source.fromIterable(
      showCode(genAST(ns, spec)).toSeq
    )
  }
}

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

      def reifyEnum(x :XDREnumeration) = {
        
      }
      def reifyStruct(x :XDRStructure) = {}
      def reifyUnion(x :XDRUnion) = {}
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

    object scalaType extends Poly1 {
      implicit def casenil = at[CNil](_ => (name :String) => s"$name@nil")
      implicit def caseEnum = at[XDREnumeration](_ => (name :String) => s"$name@enum")
      implicit def caseStruct = at[XDRStructure](_ => (name :String) => s"$name@struct")
      implicit def caseUnion = at[XDRUnion](_ => (name :String) => s"$name@union")
      implicit def caseArrayF = at[XDRFixedLengthArray](_ => (name :String) => s"$name@array[]")
      implicit def caseArrayV = at[XDRVariableLengthArray](_ => (name :String) => s"$name@array<>")
      implicit def caseOption = at[XDROptional](_ => (name :String) => s"$name*")

      implicit def caseOpaqueF = at[XDRFixedLengthOpaque](_ => (name :String) => s"$name@byte[]")
      implicit def caseOpaqueV = at[XDRVariableLengthOpaque](_ => (name :String) => s"$name@byte<>")
      implicit def caseString = at[XDRString](_ => (name :String) => s"$name@string")
      implicit def caseVoid = at[XDRVoid](_ => (name :String) => s"$name@void")
      implicit def caseInt = at[XDRInteger.type](_ => (name :String) => s"$name@int")
      implicit def caseUInt = at[XDRUnsignedInteger.type](_ => (name :String) => s"$name@uint")
      implicit def caseHyper = at[XDRHyper](x => (name :String) => s"$name@${if (x.signed) "" else "u"}hyper")
      implicit def caseFloat = at[XDRFloat.type](_ => (name :String) => s"$name@float")
      implicit def caseDouble = at[XDRDouble.type](_ => (name :String) => s"$name@double")
      implicit def caseQuad = at[XDRQuadruple.type](_ => (name :String) => s"$name@quad")
      implicit def caseBoolean = at[XDRBoolean.type](_ => (name :String) => s"$name@bool")

      implicit def caseNested(implicit mapper: Mapper[scalaType.type, NestedType]) = at[NestedType](
        _.map(scalaType)
      )
      implicit def casePrimary(implicit mapper: Mapper[scalaType.type, PrimaryType]) = at[PrimaryType](
        _.map(scalaType)
      )
      implicit def caseComposite(implicit mapper: Mapper[scalaType.type, CompositeType]) = at[CompositeType](
        _.map(scalaType)
      )
      implicit def caseFlat(implicit mapper: Mapper[scalaType.type, FlatType]) = at[FlatType](
        _.flatMap(scalaType)
      )

      def convert(x :AllType, s :String) = {
        x.flatMap(scalaType).unify.apply(s)
      }
    }

    def reify(ast :ASTree)(implicit c:ScalaScaffold.Snippet) = {


      ast.typedefs.foreach((x) => {
        println(x._2.fold(
          _.ident + "->" + x._1,
          scalaType.convert(_, x._1)
        )
        )
      })

      c.mkAST

    }

    def transType(x :XDRTypeSpecifier) :TypeOrRef = {
      x match {
        case XDRIdentifierTypeSpecifier(alias) => Left(alias)
        case x@XDRInteger => Right(Coproduct[AllType](Coproduct[FlatType](Coproduct[PrimaryType](x))))
        case x@XDRUnsignedInteger => Right(Coproduct[AllType](Coproduct[FlatType](Coproduct[PrimaryType](x))))
        case x@XDRHyper(_) => Right(Coproduct[AllType](Coproduct[FlatType](Coproduct[PrimaryType](x))))
        case x@XDRFloat => Right(Coproduct[AllType](Coproduct[FlatType](Coproduct[PrimaryType](x))))
        case x@XDRDouble => Right(Coproduct[AllType](Coproduct[FlatType](Coproduct[PrimaryType](x))))
        case x@XDRQuadruple => Right(Coproduct[AllType](Coproduct[FlatType](Coproduct[PrimaryType](x))))
        case x@XDRBoolean => Right(Coproduct[AllType](Coproduct[FlatType](Coproduct[PrimaryType](x))))
        case x@XDREnumeration(_) => Right(Coproduct[AllType](Coproduct[NestedType](x)))
        case x@XDRStructure(_) => Right(Coproduct[AllType](Coproduct[NestedType](x)))
        case x@XDRUnion(_) => Right(Coproduct[AllType](Coproduct[NestedType](x)))
      }
    }

    def transDecl(x :XDRDeclaration) :(String, TypeOrRef) = {
      x match {
        case XDRPlainDeclaration(x, XDRIdentifierLiteral(name)) => {
          (name, transType(x))
        }
        case x@XDRFixedLengthArray(_, XDRIdentifierLiteral(name), _) => {
          (name, Right(Coproduct[AllType](Coproduct[FlatType](Coproduct[CompositeType](x)))))
        }
        case x@XDRVariableLengthArray(_, XDRIdentifierLiteral(name), _) => {
          (name, Right(Coproduct[AllType](Coproduct[FlatType](Coproduct[CompositeType](x)))))
        }
        case x@XDRFixedLengthOpaque(XDRIdentifierLiteral(name), _) => {
          (name, Right(Coproduct[AllType](Coproduct[FlatType](Coproduct[PrimaryType](x)))))
        }
        case x@XDRVariableLengthOpaque(XDRIdentifierLiteral(name), _) => {
          (name, Right(Coproduct[AllType](Coproduct[FlatType](Coproduct[PrimaryType](x)))))
        }
        case x@XDRString(XDRIdentifierLiteral(name), _) => {
          (name, Right(Coproduct[AllType](Coproduct[FlatType](Coproduct[PrimaryType](x)))))
        }
        case x@XDROptional(_, XDRIdentifierLiteral(name)) => {
          (name, Right(Coproduct[AllType](Coproduct[FlatType](Coproduct[CompositeType](x)))))
        }
        case x@XDRVoid() => {
          ("void", Right(Coproduct[AllType](Coproduct[FlatType](Coproduct[PrimaryType](x)))))
        }
      }
    }

    def transConst(x :XDRConstant) :(String, XDRConstantLiteral) = {
      (x.name.ident, x.value)
    }

    def transType(x :XDRTypeDef) :(String, TypeOrRef) = {
      x match {
        case XDRPlainTypedef(x) => {
          transDecl(x)
        }
        case XDREnumTypedef(XDRIdentifierLiteral(name), x) => {
          (name, Right(Coproduct[AllType](Coproduct[NestedType](XDREnumeration(x)))))
        }
        case XDRStructTypedef(XDRIdentifierLiteral(name), x) => {
          (name, Right(Coproduct[AllType](Coproduct[NestedType](XDRStructure(x)))))
        }
        case XDRUnionTypedef(XDRIdentifierLiteral(name), x) => {
          (name, Right(Coproduct[AllType](Coproduct[NestedType](XDRUnion(x)))))
        }
      }
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
    }
  }
}
