package org.strllar.scalaxdr

import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe._
import rfc4506._

object Extractors {

  type ValueOrRef = Either[XDRIdentifierLiteral, XDRConstantLiteral]

  //TODO: use Liftable instead of dig?
  implicit def extractConstant(const :XDRConstantLiteral) = new {
    def dig = const match {
      case DecimalConstant(x) => Integer.parseInt(x)
      case HexadecimalConstant(x) => Integer.parseInt(x.drop(2), 16)
      case OctalConstant(x) => Integer.parseInt(x, 8)
    }
  }

  implicit def extractIdentifier(ident :XDRIdentifierLiteral) = new {
    def dig = ident.ident
  }

  implicit def extracValue(v :XDRValue) = new {
    def dig :ValueOrRef = v match {
      case XDRConstantValue(value) => Right(value)
      case XDRIdentifierValue(ident) => Left(ident)
    }
  }
}

import shapeless._
import shapeless.ops.coproduct.{Mapper}


package object codegen {

  type PrimaryType = XDRFixedLengthOpaque :+: XDRVariableLengthOpaque :+: XDRString :+: XDRVoid :+: XDRInteger.type :+: XDRUnsignedInteger.type :+: XDRHyper :+: XDRFloat.type  :+: XDRDouble.type  :+: XDRQuadruple.type :+: XDRBoolean.type :+: CNil
  type CompositeType = XDRFixedLengthArray :+: XDRVariableLengthArray :+: XDROptional :+: CNil
  type FlatType = PrimaryType :+: CompositeType :+: CNil
  type NestedType = XDREnumeration :+: XDRStructure :+: XDRUnion :+: CNil
  type AnyType = FlatType :+: NestedType :+: CNil

  type TypeOrRef = Either[XDRIdentifierLiteral, AnyType]

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
    import Extractors._

    trait scalaType {
      def defineAs(name :String) :Option[Tree]
      def declareAs(name :String) :Option[Tree]
//      def companionName() :String
    }
    class DummyScalaType extends scalaType {
      def defineAs(name :String) = None
      def declareAs(name :String) = None
    }

    object scalaMapping extends Poly1 {
      implicit def casenil = at[CNil](_ => new DummyScalaType)
      implicit def caseEnum = at[XDREnumeration](_ => new DummyScalaType)
      implicit def caseStruct = at[XDRStructure](_ => new DummyScalaType)
      implicit def caseUnion = at[XDRUnion](_ => new DummyScalaType)
      implicit def caseArrayF = at[XDRFixedLengthArray](_ => new DummyScalaType)
      implicit def caseArrayV = at[XDRVariableLengthArray](_ => new DummyScalaType)
      implicit def caseOption = at[XDROptional](_ => new DummyScalaType)

      implicit def caseOpaqueF = at[XDRFixedLengthOpaque](_ => new DummyScalaType)
      implicit def caseOpaqueV = at[XDRVariableLengthOpaque](_ => new DummyScalaType)
      implicit def caseString = at[XDRString](_ => new DummyScalaType)
      implicit def caseVoid = at[XDRVoid](_ => new DummyScalaType)
      implicit def caseInt = at[XDRInteger.type](_ => new DummyScalaType)
      implicit def caseUInt = at[XDRUnsignedInteger.type](_ => new DummyScalaType)
      implicit def caseHyper = at[XDRHyper](x => new DummyScalaType)
      implicit def caseFloat = at[XDRFloat.type](_ => new DummyScalaType)
      implicit def caseDouble = at[XDRDouble.type](_ => new DummyScalaType)
      implicit def caseQuad = at[XDRQuadruple.type](_ => new DummyScalaType)
      implicit def caseBoolean = at[XDRBoolean.type](_ => new DummyScalaType)

      implicit def caseNested(implicit mapper: Mapper[scalaMapping.type, NestedType]) = at[NestedType](
        _.map(scalaMapping)
      )
      implicit def casePrimary(implicit mapper: Mapper[scalaMapping.type, PrimaryType]) = at[PrimaryType](
        _.map(scalaMapping)
      )
      implicit def caseComposite(implicit mapper: Mapper[scalaMapping.type, CompositeType]) = at[CompositeType](
        _.map(scalaMapping)
      )
      implicit def caseFlat(implicit mapper: Mapper[scalaMapping.type, FlatType]) = at[FlatType](
        _.flatMap(scalaMapping)
      )

      def from(x :AnyType) :scalaType = {
        x.flatMap(scalaMapping).unify
      }
    }

    class Snippet(val pkgname :Option[String]) {

      val constStack = ListBuffer.empty[Tree]
      val declStack = ListBuffer.empty[Tree]
      val defStack = ListBuffer.empty[Tree]

      def reifyAST(ast :ASTree) = {

        ast.constdefs.foreach(cd => defineConst(cd._1, cd._2))
        declStack ++= ast.typedefs.map(td => reify(td._1, td._2)).collect({case Some(t) => t}) //TBD

        pkgname.map( pn => {
          val pname = TermName(pn)
          q"""
            package object $pname {
            ..$constStack
            ..$declStack
            }
            package $pname {
            ..$defStack
            }
            """
        }).getOrElse(q"..${constStack ++ declStack ++ defStack}")
      }

      def defineConst(name :String, value :XDRConstantLiteral): Unit = {
        constStack.append(q"val ${TermName(name)} :Int = ${Constant(value.dig)}")
      }

      def reify(n :String, x :TypeOrRef) :Option[Tree] = {
        x.fold(
          (idref) => Some(q"type ${TypeName(n)} = ${TypeName(idref.dig)}"),
          scalaMapping.from(_).defineAs(n)
        )

      }
      def reifyEnum(n :String, x :XDREnumeration) :Tree = {
//        x.body.items.map(kv => {
//          kv._1.dig
//          kv._2.dig.fold(
//          idref => q"${TermName(idref.dig)}",
//          value => q"${Constant(value)}"
//          )
//        })

        q"object ${TermName(n)}"
      }
      def reifyStruct(n :String, x :XDRStructure) :Tree = {
        val innerdefs = x.body.components.map(SemanticProcesser.transDecl(_)).collect({
          case (s, Left(id)) => {}
          case (s, Right(children)) => {

          }
        })
        q"object ${TermName(n)} {}"
      }
      def reifyUnion(n :String, x :XDRUnion) :Tree = {
        q"object ${TermName(n)}"
      }
    }

    def newSnippet(pkgname :Option[String]): Snippet = new Snippet(pkgname)
  }

  object SemanticProcesser {

    def transType(x :XDRTypeSpecifier) :TypeOrRef = {
      x match {
        case XDRIdentifierTypeSpecifier(alias) => Left(alias)
        case x@XDRInteger => Right(Coproduct[AnyType](Coproduct[FlatType](Coproduct[PrimaryType](x))))
        case x@XDRUnsignedInteger => Right(Coproduct[AnyType](Coproduct[FlatType](Coproduct[PrimaryType](x))))
        case x@XDRHyper(_) => Right(Coproduct[AnyType](Coproduct[FlatType](Coproduct[PrimaryType](x))))
        case x@XDRFloat => Right(Coproduct[AnyType](Coproduct[FlatType](Coproduct[PrimaryType](x))))
        case x@XDRDouble => Right(Coproduct[AnyType](Coproduct[FlatType](Coproduct[PrimaryType](x))))
        case x@XDRQuadruple => Right(Coproduct[AnyType](Coproduct[FlatType](Coproduct[PrimaryType](x))))
        case x@XDRBoolean => Right(Coproduct[AnyType](Coproduct[FlatType](Coproduct[PrimaryType](x))))
        case x@XDREnumeration(_) => Right(Coproduct[AnyType](Coproduct[NestedType](x)))
        case x@XDRStructure(_) => Right(Coproduct[AnyType](Coproduct[NestedType](x)))
        case x@XDRUnion(_) => Right(Coproduct[AnyType](Coproduct[NestedType](x)))
      }
    }

    def transDecl(x :XDRDeclaration) :(String, TypeOrRef) = {
      x match {
        case XDRPlainDeclaration(x, XDRIdentifierLiteral(name)) => {
          (name, transType(x))
        }
        case x@XDRFixedLengthArray(_, XDRIdentifierLiteral(name), _) => {
          (name, Right(Coproduct[AnyType](Coproduct[FlatType](Coproduct[CompositeType](x)))))
        }
        case x@XDRVariableLengthArray(_, XDRIdentifierLiteral(name), _) => {
          (name, Right(Coproduct[AnyType](Coproduct[FlatType](Coproduct[CompositeType](x)))))
        }
        case x@XDRFixedLengthOpaque(XDRIdentifierLiteral(name), _) => {
          (name, Right(Coproduct[AnyType](Coproduct[FlatType](Coproduct[PrimaryType](x)))))
        }
        case x@XDRVariableLengthOpaque(XDRIdentifierLiteral(name), _) => {
          (name, Right(Coproduct[AnyType](Coproduct[FlatType](Coproduct[PrimaryType](x)))))
        }
        case x@XDRString(XDRIdentifierLiteral(name), _) => {
          (name, Right(Coproduct[AnyType](Coproduct[FlatType](Coproduct[PrimaryType](x)))))
        }
        case x@XDROptional(_, XDRIdentifierLiteral(name)) => {
          (name, Right(Coproduct[AnyType](Coproduct[FlatType](Coproduct[CompositeType](x)))))
        }
        case x@XDRVoid() => {
          ("void", Right(Coproduct[AnyType](Coproduct[FlatType](Coproduct[PrimaryType](x)))))
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
          (name, Right(Coproduct[AnyType](Coproduct[NestedType](XDREnumeration(x)))))
        }
        case XDRStructTypedef(XDRIdentifierLiteral(name), x) => {
          (name, Right(Coproduct[AnyType](Coproduct[NestedType](XDRStructure(x)))))
        }
        case XDRUnionTypedef(XDRIdentifierLiteral(name), x) => {
          (name, Right(Coproduct[AnyType](Coproduct[NestedType](XDRUnion(x)))))
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
      val src = ScalaScaffold.newSnippet(
        Some(ns)
      )
      src.reifyAST(mkASTRoot(specs))
    }
  }
}
