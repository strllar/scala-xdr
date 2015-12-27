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

  implicit def extractCase(c :XDRCaseSpec) = new {
    val allstr = for (
      v <- c.values;
      str = extracValue(v).dig.fold(
      _.ident,
      _.dig.toString
      )
    ) yield str

    //def ident = TermName("discriminant_" + allstr.mkString("_")
    def ident = allstr.mkString("_")
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
    SemanticProcesser.transSpec(ns, specs)
  }

  def genScala(ns :String, spec :XDRSpecification) :scala.io.Source = {
    scala.io.Source.fromIterable(
      showCode(genAST(ns, spec)).toSeq
    )
  }
}

package codegen {

  object ScalaScaffold {

    trait scalaType {
      def defineAs(name :String) :Tree
      def declareAs(name :String) :Typed
      //def codecAs(name :String) :TermName

    }
    class DummyScalaType extends scalaType {
      def defineAs(name :String) = q"{}"
      def declareAs(name :String) = Typed(q"${TermName(name)}", tq"DummyType")
    }

    class XDREnumType(body :XDREnumBody) extends scalaType {
      def defineAs(name :String) = q"{}" //reifyEnum(name, body)
      def declareAs(name :String) =  Typed(q"${TermName(name)}", tq"DummyEnum")
    }
    class XDRStructType(body :XDRStructBody) extends scalaType {
      def defineAs(name :String) = q"{}" //reifyStruct(name ,body)
      def declareAs(name :String) = Typed(q"${TermName(name)}", tq"DummyStruct")
    }
    class XDRUnionType(body :XDRUnionBody) extends scalaType {
      def defineAs(name :String) = q"{}" //reifyUnion(name, body)
      def declareAs(name :String) = Typed(q"${TermName(name)}", tq"DummyUnion")
    }

    object scalaMapping extends Poly1 {
      implicit def casenil = at[CNil](_ => new DummyScalaType)
      implicit def caseEnum = at[XDREnumeration](x => new XDREnumType(x.body))
      implicit def caseStruct = at[XDRStructure](x => new XDRStructType(x.body))
      implicit def caseUnion = at[XDRUnion](x => new XDRUnionType(x.body))
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
    }

    def trans(x :AnyType) :scalaType = {
      x.flatMap(scalaMapping).unify
    }

    class Snippet(val pkgname :Option[String]) {
      import Extractors._

      val constStack = ListBuffer.empty[Tree]
      val pkgStack = ListBuffer.empty[Tree]
      val pobjStack = ListBuffer.empty[Tree]

      def reifyAST(implicit ast :ASTree) = {

        ast.constdefs.foreach(cd => defineConst(cd._1, cd._2))
        val (pobjstats, pkgstats) = ast.typedefs.map(td => reifyType(td._1, td._2)).
          foldLeft(List.empty[Tree], List.empty[Tree])(
            (acc, curr) => (
              if (curr._1.children.length > 1) acc._1 ++ curr._1.children.init else acc._1 ,
              if (curr._2.children.length > 1) acc._2 ++ curr._2.children.init else acc._2
              )
          )
        pkgStack ++= pkgstats
        pobjStack ++= pobjstats

        pkgname.map( pn => {
          val pname = TermName(pn)
          q"""
            package object $pname {
            ..$constStack
            ..$pobjStack
            }
            package $pname {
            ..$pkgStack
            }
            """
        }).getOrElse(q"..${constStack ++ pobjStack ++ pkgStack}")
      }

      def defineConst(name :String, value :XDRConstantLiteral)(implicit ast :ASTree): Unit = {
        constStack.append(q"val ${TermName(name)} :Int = ${Constant(value.dig)}")
      }

      def reifyType(n :String, x :TypeOrRef)(implicit ast :ASTree) :(Tree, Tree) = {
        x.fold(
          (idref) => (q"{type ${TypeName(n)} = ${TypeName(idref.dig)}}", q""),
          (tpe) => (q"", trans(tpe).defineAs(n))
        )
      }

    }

    def newSnippet(pkgname :Option[String]): Snippet = new Snippet(pkgname)
  }

  object SemanticProcesser {
    import Extractors._

    def resolveType(name :String)(implicit ast :ASTree) :Option[(String, AnyType)] = {
      val found = ast.typedefs.find(_._1 == name)
      found.flatMap(_._2.fold(
        refid => resolveType(refid.ident),
        Some(name, _)
      ))
    }

    def resolveConst(name :String)(implicit ast :ASTree) :Option[XDRConstantLiteral] = {
      ast.constdefs.find(_._1 == name).map(_._2)
    }


    def reifyEnum(n :String, items :Seq[(String, XDRConstantLiteral)]) :Tree = {
      val itemstats = items.map(item =>
        q"case object ${TermName(item._1)} extends Enum(${item._2.dig})"
      )

      val codeccases = items.map(item =>
        cq"${item._2.dig} => Attempt.successful(${TermName(item._1)})"
      ) :+ cq"""x@_ => Attempt.failure(Err(s"unknow enum value $$x"))"""

      q"""{
          object ${TermName(n)} {
          abstract class Enum(val value :Int)
          ..$itemstats
          implicit def codec :Codec[Enum] = codecs.int32.narrow[Enum]({
          case ..$codeccases
          }, ${pq"_"}.value)
          }
        }"""
    }

    def reifyEnum(n :String, x :XDREnumBody)(implicit ast :ASTree) :Tree = {
      val items = x.items.map(item => (
        item._1.dig,
        item._2.dig.fold(
          idref => SemanticProcesser.resolveConst(idref.dig).get,
          value => value
        )))

      reifyEnum(n, items)
    }

    def reifyStruct(n :String, x :XDRStructBody)(implicit ast :ASTree) :Tree = {

      val components = x.components.map(SemanticProcesser.transDecl(_))

      val innerdefs = components.collect({
        case (s, Right(children)) => {
          expandNested(s"anon_$s", children)
        }
      }).flatten

      val decls = components.map({
        case (s, Left(x)) => q"${TermName(s)} :${TypeName(x.dig)}"
        case (s, Right(x)) => {
          val tpe = ScalaScaffold.trans(x)
          tpe.declareAs(s)
        }
      })

      val codec = q"${TermName("XDROptional")}(AccountID.codec) :: Operation.Union_body.codec"
      //components.foldLeft(


      q"""{
         object ${TermName(n)} {
         ..$innerdefs
         case class Components(..$decls)
         type Struct = ${TypeName(n)}
         implicit def codec :Codec[Struct] = (
         $codec
         ).as[Components].xmap(new Struct(${pq"_"}), ${pq"_"}.${TermName("*")})
         }
         class ${TypeName(n)}(val ${TermName("*")} :${TermName(n)}.Components)
        }"""
    }

    def reifyUnion(n :String, x :XDRUnionBody) :Tree = {
      val arms = x.arms.map(arm => SemanticProcesser.transDecl(arm.declaration)) ++ x.defdecl.map(identity).toSeq

      //      val innerdefs = arms.collect({
      //        case (s, Right(children)) => {
      //          expandNested(s"discriminant_$s", children)
      //        }
      //      }).flatten

      q"""{
          object ${TermName(n)} {
          trait Arm {
          val v :Int
          }
          type Union = discriminant_0 :+: CNil
          }
          }"""
    }

    def expandNested(n :String, anyType: AnyType)(implicit ast :ASTree) :List[Tree] = {
      val shallow_defs =
        anyType.select[NestedType].toList.flatMap( nt =>
          nt.select[XDREnumeration].map(enum => {
            reifyEnum(n, enum.body).children.init
          }).getOrElse(
            nt.select[XDRStructure].map(struct => {
              reifyStruct(n, struct.body).children.init
            }).getOrElse(
              nt.select[XDRUnion].toList.flatMap(union => {
                reifyUnion(n, union.body).children.init
              })
            )
          )
        )
      shallow_defs
      //val more_defs =

      //      ++
      //      anyType.select[FlatType].flatMap(
      //      _.select[CompositeType]
      //      ).toList.flatMap({
      //        //todo
      //        case XDRFixedLengthArray(tpe, _, _) => {
      //          SemanticProcesser.transType(tpe).fold(
      //          _ => list.empty[Tree],
      //            expandNested(n, _)
      //          )
      //
      //        }
      //        case XDRVariableLengthArray(tpe, _, _) => {expandNested(n, SemanticProcesser.transType(tpe))}
      //        case XDROptional(tpe, _) => {expandNested(n, SemanticProcesser.transType(tpe))}
      //      })
    }


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

    def transSpec(ns :String, specs :XDRSpecification) = {
      val src = ScalaScaffold.newSnippet(
        Some(ns)
      )
      src.reifyAST(mkASTRoot(specs))
    }
  }
}
