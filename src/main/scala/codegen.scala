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
import shapeless.poly._
import shapeless.ops.coproduct.{Mapper}
//import ops.nat._
//import LT._

package object codegen {
  type TypeSNested =  XDRStructure :+: XDRUnion :+: CNil
  type TypeSPrimary = XDRInteger.type :+: XDRUnsignedInteger.type :+: XDRHyper :+: XDRFloat.type  :+: XDRDouble.type  :+: XDRQuadruple.type :+: XDRBoolean.type :+: XDREnumeration :+: CNil
  type TypeS = TypeSPrimary :+: TypeSNested :+: CNil
  type TypeSOrRef = Either[TypeS, XDRIdentifierLiteral]
  type TypeAComposite = XDRFixedLengthArray :+: XDRVariableLengthArray :+: XDROptional :+: CNil
  type TypeAPrimary =  XDRFixedLengthOpaque :+: XDRVariableLengthOpaque :+: XDRString :+: XDRVoid.type :+: CNil
  type TypeA = TypeAPrimary :+: TypeAComposite :+: CNil
  type TypeAny = Either[TypeA, TypeSOrRef]

  type PureType = Either[TypeS, TypeA]
  object asPureType extends Poly1{
    implicit def caseS = at[TypeS](Left(_) :PureType)
    implicit def caseA = at[TypeA](Right(_) :PureType)
  }

  type ReifiedInfo = (PureType, String)
  type ReifiableInfoS = Either[TypeS, ReifiedInfo]
  type ReifiableInfoA =  Either[PureType, ReifiedInfo] //seems useless

  def ReifiedInfo(ident :String, tpe :PureType) :ReifiedInfo = (tpe, ident)

  type FullReifiableInfo = Either[ReifiableInfoS, (TypeA, Option[ReifiableInfoS])]

  case class TypeDef(ident :String, tpe :TypeAny)
  case class EntityDecl(ident :String, info :FullReifiableInfo)



  case class ASTree(
                     typedefs :Vector[TypeDef],
                     constdefs :Vector[(String, XDRConstantLiteral)]
                   )


  object liftTypePoly extends Poly1 {
    implicit def caseTypeS = at[TypeS](x => Left(x) :TypeSOrRef)
    implicit def caseTypeSPrimary = at[TypeSPrimary](x => liftTypePoly(Coproduct[TypeS](x)))
    implicit def caseTypeSNested = at[TypeSNested](x => liftTypePoly(Coproduct[TypeS](x)))

    implicit def caseTypeA = at[TypeA](x => Left(x) :TypeAny)
    implicit def caseTypeSOrRef = at[TypeSOrRef](x => Right(x) :TypeAny)
    implicit def caseTypeAPrimary = at[TypeAPrimary](x => liftTypePoly(Coproduct[TypeA](x)))
    implicit def caseTypeAComposite = at[TypeAComposite](x => liftTypePoly(Coproduct[TypeA](x)))
    implicit def caseTypeVoid = at[XDRVoid.type](x => liftTypePoly(Coproduct[TypeAPrimary](x)))
  }

  def liftType[T](t :T)(implicit cse: Case1[liftTypePoly.type, T]) = cse(t)
  //exposed util functions

  def genAST(ns :String, specs :XDRSpecification) = {
    SemanticProcesser.genSpec(ns, specs)
  }

  def genScala(ns :String, spec :XDRSpecification) :scala.io.Source = {
    scala.io.Source.fromIterable(
      genAST(ns, spec).children.init.flatMap(
        showCode(_) +  System.lineSeparator()
      )
    )
  }
}

package codegen {

import scala.annotation.tailrec

object ScalaScaffold {
    import Extractors._

    trait scalaType {
      def defineAs(name :String)(implicit ast :ASTree) :Tree = q"{}"
      def declareAs(tpe :String) :Tree = tq"${TypeName(tpe)}"
      def codecAs(name :String) :Tree = q"${TermName(name)}.codec"
    }
    object DummyScalaType extends scalaType
    val DummyStatement = q""

  object
    class XDREnumType(enum :SemanticProcesser.EnumScheme) extends scalaType {
      override def defineAs(name :String)(implicit ast :ASTree) = {
        val itemstats = enum.map(item =>
          q"case object ${TermName(item._1)} extends Enum(${item._2.dig})"
        )

        val codeccases = enum.map(item =>
          cq"${item._2.dig} => Attempt.successful(${TermName(item._1)})"
        ) :+ cq"""x@_ => Attempt.failure(Err(s"unknow enum value $$x"))"""

        q"""{
          object ${TermName(name)} {
          abstract class Enum(val value :Int)
          ..$itemstats
          implicit def codec :Codec[Enum] = codecs.int32.narrow[Enum]({
          case ..$codeccases
          }, ${pq"_"}.value)
          }
        }"""
      }
      override def declareAs(tpe :String) =  tq"${TermName(tpe)}.Enum"
    }

  def getRefiedName(tpe :ReifiableInfoS) :Option[String] = {
    tpe.right.toOption.map(_._2)
  }

  def transReifiedField(fd :EntityDecl)(implicit ast :ASTree) = {
    new {
      val (tpehint, tpe) = fd.info.fold(
        _.fold(
          tpes => (None, transS(tpes)),
          refied => (Some(refied._2), trans(refied._1))
        ),
        x => (x._1.map(getRefiedName))
      )
    }
  }
    class XDRStructType(items :SemanticProcesser.StructScheme, inplacedefs :List[Tree]) extends scalaType {
      override def defineAs(name :String)(implicit ast :ASTree) = {
        val components =  items.map(f => SemanticProcesser.reifyField(f)._2)

        val codec = components.init.foldRight(q"${components.last.codec}")(
          (c, acc) => {
            q"$acc.::(${c.codec})"
          }
        )
        val decls = components.map(c => Typed(q"${TermName(c.fieldname)}", c.declare))

        q"""{
           object ${TermName(name)} {
           ..$inplacedefs
           case class Components(..$decls)
           type Struct = ${TypeName(name)}
           implicit def codec :Codec[Struct] = (
           $codec
           ).as[Components].xmap(new Struct(${pq"_"}), ${pq"_"}.${TermName("*")})
           }
           class ${TypeName(name)}(val ${TermName("*")} :${TermName(name)}.Components)
        }"""
      }
    }

    class XDRUnionType(union :SemanticProcesser.UnionScheme) extends scalaType {
//
//      def wrapCaseValue(xDRValue: XDRValue, namehint :String, tpe: scalaType) =  {
//        //quick dirty hack
//        xDRValue.dig.fold(
//          id => q"${TermName(namehint)}.${TermName(id.ident)}",
//          num => q"${Constant(num.dig)}"
//        )
//      }
//      def clzName(xDRValue: XDRValue) = {
//        "discriminant_" + xDRValue.dig.fold(_.ident, _.dig.toString)
//      }
//
//      def defaultClzName = "discriminant_default"
//
//      override def defineAs(name :String)(implicit ast :ASTree) = {
//        val (discdecl, arms, deftarm, inplacedefs) = (
//          SemanticProcesser.reifyField(union._1)._2,
//          (union._2.map(x => (SemanticProcesser.reifyField(x._1)._2, x._2))),
//          union._3.map(x => SemanticProcesser.reifyField(x)._2),
//          SemanticProcesser.reifyField(union._1)._1 ++ union._2.flatMap(x => (SemanticProcesser.reifyField(x._1)._1)) ++ union._3.toSeq.flatMap(x => SemanticProcesser.reifyField(x)._1)
//          )
//
//
//        val codecstat0 = arms.flatMap(arm => {arm._2.map(single => (arm._1, single))}).
//          foldLeft( q"codecs.discriminated[ArmsUnion].by(${discdecl.codec})")((acc, arm) => {
//            val (field, casevalue) = arm
//            val discriminClz =  TypeName(clzName(casevalue))
//          q"$acc.caseO(${wrapCaseValue(casevalue, discdecl.ident, discdecl.tpe)})(${pq"_"}.select[$discriminClz].map(${pq"_"}.${TermName(field.fieldname)}))((x) => Coproduct[ArmsUnion](new $discriminClz(x)))(${field.codec})"
//        })
//
//
//        val (totclz0, uniondef0) = arms.flatMap(arm => {arm._2.map(single => (arm._1, single))}).foldRight((List.empty[Tree], tq"${TypeName("CNil")}":Tree))(
//          (arm, acc) => {
//            val (field, casevalue) = arm
//
//            val armclzname = TypeName(clzName(casevalue))
//            val clzdef = q"class $armclzname(val ${TermName(field.fieldname)} :${field.declare}) extends Arm { val ${TermName(discdecl.fieldname)} :${discdecl.declare} = ${wrapCaseValue(casevalue, discdecl.ident, discdecl.tpe)} }"
//
//            (clzdef +: acc._1,  q":+:[$armclzname, ${acc._2}]")
//        })
//
//        val (totclz, uniondef, codecstat) = deftarm.map(arm => {
//          val field = arm
//          val deftarmclz = TypeName(defaultClzName)
//          val deftcodec = q"(${field.codec}.::(${discdecl.codec})).as[$deftarmclz]"
//          (totclz0 :+ q"class $deftarmclz(val ${TermName(discdecl.fieldname)} :${discdecl.declare}, val ${TermName(field.fieldname)} :${field.declare}) extends Arm {}",
//            q"Either[$deftarmclz, ${TypeName("ArmsUnion")}]",
//            q"codecs.discriminatorFallback($deftcodec, $codecstat0)")
//        }).getOrElse((totclz0, tq"ArmsUnion", codecstat0))
//
//
//        val n = name
//        q"""{
//          object ${TermName(n)} {
//              trait Arm {
//              val ${TermName(discdecl.fieldname)} :${discdecl.declare}
//              }
//              ..$inplacedefs
//              ..$totclz
//              type ArmsUnion = $uniondef0
//              type Union = $uniondef
//              implicit def codec :Codec[Union] = $codecstat
//          }
//        }"""
//      }
//      override def declareAs(tpe :String) = tq"${TermName(tpe)}.Union"
    }

    object nestedMapping extends Poly1 {
      implicit def caseStruct(implicit ast :ASTree) = at[XDRStructure](x => {
        val (innerdefs, scheme) = SemanticProcesser.reshape(x.body).map( c => {
          val (deps, newc) = SemanticProcesser.reifyField(c)
          (deps.toSeq.flatMap(SemanticProcesser.reify), newc)
        }).unzip
        new XDRStructType(scheme, innerdefs.flatten.toList)
      })
      implicit def caseUnion(implicit ast :ASTree) = at[XDRUnion](x => {
        new XDRUnionType(SemanticProcesser.reshape(x.body))
      })
    }

    object compositeMapping extends Poly1 {
      implicit def caseArrayF(implicit ast :ASTree) = at[XDRFixedLengthArray](x => {
//        lazy val (namehint, undertpe) = SemanticProcesser.transComposite(Coproduct[CompositeType](x)).fold(x => (None, trans(x.tpe)), x => (Some(x.ident), trans(x.tpe)))
//
//        val len = x.length.dig.fold(
//          (id) => q"${TermName(id.ident)}",
//          (value) => q"${Constant(value.dig)}")

        new scalaType {
//          override def declareAs(tpe :String) :Tree ={
//            tq"Vector[${(undertpe).declareAs(namehint.getOrElse(tpe))}]"
//          }
//          override def codecAs(name :String) :Tree = {
//            q"XDRFixedLengthArray(${(undertpe).codecAs(namehint.getOrElse(name))})($len)"
//          }
        }
      })

      implicit def caseArrayV(implicit ast :ASTree) = at[XDRVariableLengthArray](x => {
//        lazy val (namehint, undertpe) = SemanticProcesser.transComposite(Coproduct[CompositeType](x)).fold(x => (None, trans(x.tpe)), x => (Some(x.ident), trans(x.tpe)))
//        val len = x.maxlength.map(_.dig.fold(
//          (id) => q"Some(${TermName(id.ident)})",
//          (value) => q"Some(${Constant(value.dig)})")
//        ).getOrElse(q"None")

        new scalaType {
//          override def declareAs(tpe :String) :Tree = {
//            tq"Vector[${(undertpe).declareAs(namehint.getOrElse(tpe))}]"
//          }
//          override def codecAs(name :String) :Tree = {
//            q"XDRVariableLengthArray($len, ${(undertpe).codecAs(namehint.getOrElse(name))})"
//          }
        }
      })

      implicit def caseOption(implicit ast :ASTree) = at[XDROptional](x => {
//        lazy val (namehint, undertpe) = SemanticProcesser.transComposite(Coproduct[CompositeType](x)).fold(x => (None, trans(x.tpe)), x => (Some(x.ident), trans(x.tpe)))

        new scalaType {
//          override def declareAs(tpe :String) :Tree = {
//            tq"Option[${(undertpe).declareAs(namehint.getOrElse(tpe))}]"
//          }
//          override def codecAs(name :String) :Tree = {
//            q"XDROptional(${(undertpe).codecAs(namehint.getOrElse(name))})"
//          }
        }
      })

    }

    object scalaMapping extends Poly1 {
      implicit def casenil = at[CNil](_ => DummyScalaType)

      implicit def caseOpaqueF = at[XDRFixedLengthOpaque](x =>
        new scalaType {
          override def declareAs(tpe :String) :Tree = tq"Vector[Byte]"
          override def codecAs(name :String) :Tree = x.length.dig.fold(
            (id) => q"XDRFixedLengthOpaque(${TermName(id.ident)})",
            (value) => q"XDRFixedLengthOpaque(${Constant(value.dig)})"
          )
        })
      implicit def caseOpaqueV = at[XDRVariableLengthOpaque](x =>
        new scalaType {
          override def declareAs(tpe :String) :Tree = tq"Vector[Byte]"
          override def codecAs(name :String) :Tree = x.maxlength.map(
            _.dig.fold(
            (id) => q"XDRVariableLengthOpaque(Some(${TermName(id.ident)}))",
            (value) => q"XDRVariableLengthOpaque(Some(${Constant(value.dig)}))")
          ).getOrElse(q"XDRVariableLengthOpaque(None)")
        })
      implicit def caseString = at[XDRString](x =>
        new scalaType {
          override def declareAs(tpe :String) :Tree = tq"String"
          override def codecAs(name :String) :Tree = x.maxlength.map(
            _.dig.fold(
              (id) => q"XDRString(Some(${TermName(id.ident)}))",
              (value) => q"XDRString(Some(${Constant(value.dig)}))")
          ).getOrElse(q"XDRString(None)")
        })
      implicit def caseVoid = at[XDRVoid.type](x =>
        new scalaType {
          override def declareAs(tpe :String) :Tree = tq"Unit"
          override def codecAs(name :String) :Tree = q"XDRVoid"
        })
      implicit def caseInt = at[XDRInteger.type](x =>
        new scalaType {
          override def declareAs(tpe :String) :Tree = tq"Int"
          override def codecAs(name :String) :Tree = q"XDRInteger"
        })
      implicit def caseUInt = at[XDRUnsignedInteger.type](x =>
        new scalaType {
          override def declareAs(tpe :String) :Tree = tq"Long"
          override def codecAs(name :String) :Tree = q"XDRUnsignedInteger"
        })
      implicit def caseHyper = at[XDRHyper](x =>
        new scalaType {
          override def declareAs(tpe :String) :Tree =
            if (x.signed) tq"Long"
            else tq"BigInt"
          override def codecAs(name :String) :Tree =
            if (x.signed) q"XDRHyper"
            else q"XDRUnsignedHyper"
        })
      implicit def caseFloat = at[XDRFloat.type](x =>
        new scalaType {
          override def declareAs(tpe :String) :Tree = tq"Float"
          override def codecAs(name :String) :Tree = q"XDRFloat"
        })
      implicit def caseDouble = at[XDRDouble.type](x =>
        new scalaType {
          override def declareAs(tpe :String) :Tree = tq"Double"
          override def codecAs(name :String) :Tree = q"XDRDouble"
        })
      implicit def caseQuad = at[XDRQuadruple.type](_ => DummyScalaType) //TODO
//      (x =>
//        new scalaType {
//          override def declareAs(name :String, tpe :String) :Tree = Typed(q"${TermName(name)}", tq"BigDecimal")
//          override def codecAs(name :String) :Tree = q"XDRQuadruple"
//        })

      implicit def caseBoolean = at[XDRBoolean.type](x =>
        new scalaType {
          override def declareAs(tpe :String) :Tree = tq"Boolean"
          override def codecAs(name :String) :Tree = q"XDRBoolean"
        })

      implicit def caseEnum(implicit ast :ASTree) = at[XDREnumeration](x =>  {
        new XDREnumType(SemanticProcesser.reshape(x.body))
      })

      implicit def caseNested(implicit mapper: Mapper[nestedMapping.type, TypeSNested], ast :ASTree) = at[TypeSNested](
        _.map(nestedMapping)
      )
      implicit def caseSPrimary(implicit mapper: Mapper[scalaMapping.type, TypeSPrimary]) = at[TypeSPrimary](
        _.map(scalaMapping)
      )

      implicit def caseComposite(implicit mapper: Mapper[compositeMapping.type, TypeAComposite], ast :ASTree) = at[TypeAComposite](
        _.map(compositeMapping)
      )
      implicit def caseAPrimary(implicit mapper: Mapper[scalaMapping.type, TypeAPrimary]) = at[TypeAPrimary](
        _.map(scalaMapping)
      )
    }

  def transS(x :TypeS)(implicit ast :ASTree) :scalaType = {
    x.flatMap(scalaMapping).unify
  }

  def transA(x :TypeA)(implicit ast :ASTree) :scalaType = {
    x.flatMap(scalaMapping).unify
  }

  def trans(x :PureType)(implicit ast :ASTree) :scalaType = {
    x.fold(transS, transA)
  }

  private def transAny(x :TypeAny)(implicit ast :ASTree) :scalaType = {
    x.fold(
      transA,
      _.fold(
        transS,
        id => SemanticProcesser.resolveTypeIdent(id.ident)._2.fold(transS, transA)
      )
    )
  }


  class Snippet(val pkgname :Option[String]) {
      import Extractors._

      def reifyAST(implicit ast :ASTree) = {

        val conststat = ast.constdefs.map(kv => {
          val (name, value) = kv
          q"val ${TermName(name)} :Int = ${Constant(value.dig)}"
        })
        val pkgstats = ast.typedefs.flatMap(td => SemanticProcesser.reifyTypeDef(td))

        pkgname.map( pn => {
          val pname = TermName(pn)
          q"""
            package object $pname {
            ..$conststat
            }
            package $pname {
            ..$pkgstats
            }
            """
        }).getOrElse(q"..${conststat ++ pkgstats}")
        q""
      }
    }

    def newSnippet(pkgname :Option[String]): Snippet = new Snippet(pkgname)
  }

  object SemanticProcesser {
    import Extractors._

    object transCompositePoly extends Poly1 {
      implicit def caseArrayF = at[XDRFixedLengthArray](x => transTypeSpec(x.typespec))
      implicit def caseArrayV = at[XDRVariableLengthArray](x => transTypeSpec(x.typespec))
      implicit def caseOption = at[XDROptional](x => transTypeSpec(x.typespec))
    }

    def resolveComposite(ct :TypeAComposite)(implicit ast :ASTree) :ReifiableInfoS = {
      resolveTypeS(ct.map(transCompositePoly).unify)
    }
    def liftComposite(anyType :TypeA)(implicit ast :ASTree) :Option[ReifiableInfoS] =  {
      anyType.select[TypeAComposite].map(resolveComposite)
    }

    def resolveTypeIdent(name :String)(implicit ast :ASTree) :(String, PureType) = {
      val found = ast.typedefs.find(_.ident == name)
      found.get.tpe.fold(
        tpe => (name, Right(tpe)),
        _.fold(
          tpe => (name , Left(tpe)),
          refid => resolveTypeIdent(refid.ident)
        )
      )
    }

    def resolveConst(name :String)(implicit ast :ASTree) :Option[XDRConstantLiteral] = {
      ast.constdefs.find(_._1 == name).map(_._2)
    }

    def resolveTypeS(typeSOrRef: TypeSOrRef)(implicit ast :ASTree) :ReifiableInfoS = {
      typeSOrRef.fold(
        tpe => Left(tpe),
        refid => Right(resolveTypeAlias(refid.ident))
      )
    }

    def resolveTypeA(typeAny: TypeAny)(implicit ast :ASTree) :ReifiableInfoA = {
      typeAny.fold(
        tpe => Left(Right(tpe)),
        _.fold(
          tpe => Left(Left(tpe)),
          refid => Right(resolveTypeAlias(refid.ident))
        )
      )
    }

    def resolveTypeAlias(name :String)(implicit ast :ASTree) :ReifiedInfo = {
      val (bottomname, bottomtpe) = resolveTypeIdent(name)
      (bottomtpe, name)
    }

    def resolveTypeFull(typeAny: TypeAny)(implicit ast :ASTree) :FullReifiableInfo = {
      typeAny.fold(
        tpea => Right(tpea, liftComposite(tpea)),
        _.fold(
          tpes => Left(Left(tpes)),
          refid => Left(Right(resolveTypeAlias(refid.ident)))
        )
      )
    }

    def reshapeDeclAsEntity(x :XDRDeclaration)(implicit ast :ASTree): EntityDecl = {
      val (name :String, anyType: TypeAny) = transDecl(x)
      EntityDecl(name, resolveTypeFull(anyType))
    }

    def isNested(tpes :TypeS) :Boolean = {
      if (tpes.select[TypeSNested].isDefined) true
      else if (
        tpes.select[TypeSPrimary].flatMap(
          _.select[XDREnumeration]
        ).isDefined
      ) true
      else false
    }

    def reifyField(fd :EntityDecl)(implicit ast :ASTree) : (Option[ReifiedInfo], EntityDecl) = {
      fd.info match {
        case Left(Left(tpes)) if isNested(tpes) => {
          val refied = ReifiedInfo(s"anony", asPureType(tpes))
          (Some(refied), fd.copy(info = Left(Right(refied))))
        }
        case Right((comptpe, Some(Left(tpes)))) if isNested(tpes) => {
          val refied = ReifiedInfo(s"anony", asPureType(tpes))
          (Some(refied), fd.copy(info = Right((comptpe, Some(Right(refied))))))
        }
        case _ => {
          (None, fd)
        }
      }
    }

    def reify(info :ReifiedInfo) :List[Tree] = {
      val stats = ScalaScaffold.trans(info._1).defineAs(info._2).children
      if (stats.size > 0) stats.init
      else stats
    }

    def reifyTypeDef(td :TypeDef)(implicit ast :ASTree) :List[Tree] = {
//      td.tpe.fold(
//      _.fold(
//      idref =>
//      )
//      )
//      aliasReifier(n).left.toSeq.flatMap(mkDeps).toList
      List.empty[Tree]
    }

    type EnumScheme = Seq[(String, XDRConstantLiteral)]
    def reshape(x :XDREnumBody)(implicit ast :ASTree) :EnumScheme = {
      val items = x.items.map(item => (
        item._1.ident,
        item._2.dig.fold(
          idref => SemanticProcesser.resolveConst(idref.ident).get,
          value => value
        )))
      items
    }

    type StructScheme = Seq[EntityDecl]
    def reshape(x :XDRStructBody)(implicit ast :ASTree) :StructScheme = {
      val components = x.components.map(reshapeDeclAsEntity)
      components
    }

    type UnionScheme = (EntityDecl, Seq[(EntityDecl, Seq[XDRValue])], Option[EntityDecl])
    def reshape(x :XDRUnionBody)(implicit ast :ASTree) :UnionScheme = {
      val defaularm = x.defdecl.map(reshapeDeclAsEntity)
      val discr = reshapeDeclAsEntity(x.discriminant)
      val arms = x.arms.map(arm => (reshapeDeclAsEntity(arm.declaration), arm.values))
      (discr, arms, defaularm)
    }

    def transTypeSpec(x :XDRTypeSpecifier) :TypeSOrRef = {
      x match {
        case XDRIdentifierTypeSpecifier(alias) => Right(alias)
        case x@XDRInteger => liftType(Coproduct[TypeSPrimary](x))
        case x@XDRUnsignedInteger => liftType(Coproduct[TypeSPrimary](x))
        case x@XDRHyper(_) => liftType(Coproduct[TypeSPrimary](x))
        case x@XDRFloat => liftType(Coproduct[TypeSPrimary](x))
        case x@XDRDouble => liftType(Coproduct[TypeSPrimary](x))
        case x@XDRQuadruple => liftType(Coproduct[TypeSPrimary](x))
        case x@XDRBoolean => liftType(Coproduct[TypeSPrimary](x))
        case x@XDREnumeration(_) => liftType(Coproduct[TypeSPrimary](x))
        case x@XDRStructure(_) => liftType(Coproduct[TypeSNested](x))
        case x@XDRUnion(_) => liftType(Coproduct[TypeSNested](x))
      }
    }

    def transDecl(x :XDRDeclaration) :(String, TypeAny) = {
      x match {
        case XDRPlainDeclaration(x, XDRIdentifierLiteral(name)) => (name, liftType(transTypeSpec(x)))
        case x@XDRFixedLengthArray(_, XDRIdentifierLiteral(name), _) => (name, liftType(Coproduct[TypeAComposite](x)))
        case x@XDRVariableLengthArray(_, XDRIdentifierLiteral(name), _) => (name, liftType(Coproduct[TypeAComposite](x)))
        case x@XDRFixedLengthOpaque(XDRIdentifierLiteral(name), _) => (name, liftType(Coproduct[TypeAPrimary](x)))
        case x@XDRVariableLengthOpaque(XDRIdentifierLiteral(name), _) => (name, liftType(Coproduct[TypeAPrimary](x)))
        case x@XDRString(XDRIdentifierLiteral(name), _) => (name, liftType(Coproduct[TypeAPrimary](x)))
        case x@XDROptional(_, XDRIdentifierLiteral(name)) => (name, liftType(Coproduct[TypeAComposite](x)))
        case x@XDRVoid => ("void", liftType(Coproduct[TypeAPrimary](x)))
      }
    }

    def transConstDef(x :XDRConstant) :(String, XDRConstantLiteral) = {
      (x.name.ident, x.value)
    }

    def reshapeTypeDef(x :XDRTypeDef) :TypeDef = {
      x match {
        case XDRPlainTypedef(x) => TypeDef.tupled(transDecl(x))
        case XDREnumTypedef(XDRIdentifierLiteral(name), x) => TypeDef(name, liftType(liftType(Coproduct[TypeSPrimary](XDREnumeration(x))):TypeSOrRef))
        case XDRStructTypedef(XDRIdentifierLiteral(name), x) => TypeDef(name, liftType(liftType(Coproduct[TypeSNested](XDRStructure(x))):TypeSOrRef))
        case XDRUnionTypedef(XDRIdentifierLiteral(name), x) => TypeDef(name, liftType(liftType(Coproduct[TypeSNested](XDRUnion(x))):TypeSOrRef))
      }
    }

    def mkASTRoot(specs :XDRSpecification) :ASTree = {
      val alldef = specs.defs.map(_.anydef).foldLeft(
        Vector.empty[TypeDef],
        Vector.empty[(String, XDRConstantLiteral)]
      )((y, x) => {
        x.fold(
          z => (y._1 :+ reshapeTypeDef(z), y._2),
          z => (y._1, y._2 :+ transConstDef(z))
        )
      })
      ASTree.tupled(alldef)
    }

    def genSpec(ns :String, specs :XDRSpecification) = {
      val src = ScalaScaffold.newSnippet(
        Some(ns)
      )
      src.reifyAST(mkASTRoot(specs))
    }
  }
}
