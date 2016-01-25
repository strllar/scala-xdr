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

  type ReifiedInfo = (String) // (PureType, String)
  type ReifiableInfoS = Either[TypeS, ReifiedInfo]
  type ReifiableInfoA =  Either[PureType, ReifiedInfo] //seems useless

  case class TypeDef(ident :String, tpe :TypeAny)
  case class EntityDecl(ident :String, info :TypeAny)



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

  def createObject(name :String, stats :List[Tree]) :Tree = {
    q"""
         object ${TermName(name)} {
         ..$stats
         }
       """
  }

  sealed trait ScalaRawType {
    def tree: Tree
  }
  sealed trait ScalaRawHolder {

  }
  case class ScalaTerm(name :TermName, tpe :ScalaRawType) extends ScalaRawHolder
  case class ScalaCodecExpr(tree :Tree, tpe :ScalaFragmentType)
  case class ScalaPrimaryType(tree :Tree) extends ScalaRawType
  case class ScalaHType(tree :Tree) //TODO: extends ScalaRawType(tree.children...)
  case class ScalaCType(tree :Tree) //TODO: extends ScalaRawType(tree.children...)

  case class ScalaFragmentType(mirror :ScalaRawType)
  case class ScalaFragmentHolder(mirror :ScalaTerm, tpe :ScalaFragmentType)
  case class ScalaTerms(terms :List[ScalaTerm])
  case class ScalaWrappedHolder(tree :Tree, tpe :ScalaFragmentType) extends ScalaRawHolder
  case class ScalaTermWrapper(trans :ScalaTerm => ScalaWrappedHolder)  //TODO: deps
  case class ScalaTermsWrapper(trans :ScalaTerms => ScalaWrappedHolder) //TODO: deps

  trait scalaTemplate {
    def defaultCodec :ScalaCodecExpr
    def defaultRepr : Option[ScalaTerm]
    def concreteRepr :Seq[(ScalaRawType, ScalaTerms => ScalaWrappedHolder)]
    //def placedAs(name :String) :List[Tree]
  }
  case class scalaPrimaryTemplate(tpe :Tree, codec :Tree) extends scalaTemplate {
    val rawtpe = new ScalaRawType{
      override def tree = tpe
    }
    override def defaultCodec = ScalaCodecExpr(codec, ScalaFragmentType(rawtpe))
    override def defaultRepr = None
    override def concreteRepr = Seq.empty
  }
  case class scalaDefualtCompoundTemplate() extends scalaTemplate {
    override def defaultCodec = ScalaCodecExpr(q"XDRVoid", ScalaFragmentType(ScalaPrimaryType(tq"Unit")))
    override def defaultRepr = None
    override def concreteRepr = Seq.empty
  }

//  case class scalaEnumTemplate() extends scalaTemplate {
//
//  }

  def mkCodec(tpe :TypeAny) :Tree = {
    tpe.fold(
      tpea => transA(tpea).defaultCodec.tree,
      _.fold(
        tpes => transS(tpes).defaultCodec.tree,
        id => q"${TermName(id.ident)}.codec"
      )
    )
  }
  def mkRepr(tpe :TypeAny) :Tree = {
    tpe.fold(
      tpea => transA(tpea).defaultCodec.tpe.mirror.tree,
      _.fold(
        tpes => transS(tpes).defaultCodec.tpe.mirror.tree,
        id => q"${TermName(id.ident)}.Expr"
      )
    )
  }
  case class scalaStructTemplate(body: XDRStructBody) extends scalaTemplate {
    val components = body.components.map(decl => SemanticProcesser.transDecl(decl))

    val codec = components.init.foldRight(q"${mkCodec(components.last._2)}")(
      (c, acc) => {
        q"$acc.::(${mkCodec(c._2)})"
      }
    )
    val decls = components.map(c => Typed(q"${TermName(c._1)}", mkRepr(c._2)))

    override def defaultCodec = ScalaCodecExpr(codec, ScalaFragmentType(ScalaPrimaryType(tq"Unit")))
    override def defaultRepr = None
    override def concreteRepr = Seq.empty
  }

  object scalaMapping2 extends Poly1 {
    //implicit def casenil = at[CNil](_ => scalaDefualtCompoundType())

    implicit def caseOpaqueF = at[XDRFixedLengthOpaque](x =>
      scalaPrimaryTemplate(tq"Vector[Byte]",
        x.length.dig.fold(
          (id) => q"XDRFixedLengthOpaque(${TermName(id.ident)})",
          (value) => q"XDRFixedLengthOpaque(${Constant(value.dig)})"
        )
      )
    )

    implicit def caseOpaqueV = at[XDRVariableLengthOpaque](x =>
      scalaPrimaryTemplate(tq"Vector[Byte]",
        x.maxlength.map(
          _.dig.fold(
            (id) => q"XDRVariableLengthOpaque(Some(${TermName(id.ident)}))",
            (value) => q"XDRVariableLengthOpaque(Some(${Constant(value.dig)}))")
        ).getOrElse(q"XDRVariableLengthOpaque(None)")
      )
    )

    implicit def caseString = at[XDRString](x =>
      scalaPrimaryTemplate(tq"String",
        x.maxlength.map(
          _.dig.fold(
            (id) => q"XDRString(Some(${TermName(id.ident)}))",
            (value) => q"XDRString(Some(${Constant(value.dig)}))")
        ).getOrElse(q"XDRString(None)")
      )
    )

    implicit def caseVoid = at[XDRVoid.type](x =>
      scalaPrimaryTemplate(tq"Unit", q"XDRVoid")
    )

    implicit def caseInt = at[XDRInteger.type](x =>
      scalaPrimaryTemplate(tq"Int", q"XDRInteger")
    )

    implicit def caseUInt = at[XDRUnsignedInteger.type](x =>
      scalaPrimaryTemplate(tq"Long", q"XDRUnsignedInteger")
    )

    implicit def caseHyper = at[XDRHyper](x =>
      scalaPrimaryTemplate(
        if (x.signed) tq"Long"
        else tq"BigInt",
        if (x.signed) q"XDRHyper"
        else q"XDRUnsignedHyper"
      )
    )

    implicit def caseFloat = at[XDRFloat.type](x =>
      scalaPrimaryTemplate(tq"Float", q"XDRFloat")
    )

    implicit def caseDouble = at[XDRDouble.type](x =>
      scalaPrimaryTemplate(tq"Double", q"XDRDouble")
    )

    implicit def caseQuad = at[XDRQuadruple.type](_ => scalaPrimaryTemplate(tq"BigDecimal", q"XDRQuadruple"))

    implicit def caseBoolean = at[XDRBoolean.type](x =>
      scalaPrimaryTemplate(tq"Boolean", q"XDRBoolean")
    )

    implicit def caseEnum = at[XDREnumeration](x =>  {
      //new XDREnumType(SemanticProcesser.reshape(x.body))
      scalaDefualtCompoundTemplate()
    })

    implicit def caseStruct = at[XDRStructure](x => {
      scalaStructTemplate(x.body)
    })

    implicit def caseUnion = at[XDRUnion](x => {
      scalaDefualtCompoundTemplate()
    })

    implicit def caseArrayF = at[XDRFixedLengthArray](x => {
      scalaDefualtCompoundTemplate()
    })

    implicit def caseArrayV = at[XDRVariableLengthArray](x => {
      scalaDefualtCompoundTemplate()
    })

    implicit def caseOption = at[XDROptional](x => {
      scalaDefualtCompoundTemplate()
    })

    implicit def caseNested(implicit mapper: Mapper[scalaMapping2.type, TypeSNested]) = at[TypeSNested](
      _.map(scalaMapping2)
    )
    implicit def caseSPrimary(implicit mapper: Mapper[scalaMapping2.type, TypeSPrimary]) = at[TypeSPrimary](
      _.map(scalaMapping2)
    )

    implicit def caseComposite(implicit mapper: Mapper[scalaMapping2.type, TypeAComposite]) = at[TypeAComposite](
      _.map(scalaMapping2)
    )
    implicit def caseAPrimary(implicit mapper: Mapper[scalaMapping2.type, TypeAPrimary]) = at[TypeAPrimary](
      _.map(scalaMapping2)
    )
  }


  def transS(x :TypeS) :scalaTemplate = {
    x.flatMap(scalaMapping2).unify
  }

  def transA(x :TypeA) :scalaTemplate = {
    x.flatMap(scalaMapping2).unify
  }

  def trans(x :PureType) :scalaTemplate = {
    x.fold(transS, transA)
  }

  private def transAny(x :TypeAny, scope :SemanticProcesser.HomomorphicScope) :Option[scalaTemplate] = {
    x.fold(
      tpea => Some(transA(tpea)),
      _.fold(
        tpes => Some(transS(tpes)),
        id => scope.resolveType(id.ident).map(_._2.fold(transS, transA))
      )
    )
  }

  def statementsInScope(tree :Tree) :List[Tree] = {
    val chlds = tree.children
    if (chlds.isEmpty) chlds
    else chlds.init
  }

  case class NestedScope(under :SemanticProcesser.HomomorphicScope)
    extends SemanticProcesser.HomomorphicScope(Some(under)) {
    def resolveType(name :String) = parent.flatMap(_.resolveType(name))
    def resolveConst(name :String) = parent.flatMap(_.resolveConst(name))
  }

  object transCompositePoly extends Poly1 {
    implicit def caseArrayF = at[XDRFixedLengthArray](x => SemanticProcesser.transTypeSpec(x.typespec))
    implicit def caseArrayV = at[XDRVariableLengthArray](x => SemanticProcesser.transTypeSpec(x.typespec))
    implicit def caseOption = at[XDROptional](x => SemanticProcesser.transTypeSpec(x.typespec))
  }

  def resolveComposite(ct :TypeAComposite) :ReifiableInfoS = {
    resolveTypeS(ct.map(transCompositePoly).unify)
  }
   def liftComposite(anyType :TypeA) :Option[ReifiableInfoS] =  {
     anyType.select[TypeAComposite].map(resolveComposite)
   }
  def resolveTypeS(typeSOrRef: TypeSOrRef) :ReifiableInfoS = {
    typeSOrRef.fold(
      tpe => Left(tpe),
      refid => Right(refid.ident)
    )
  }
  def resolveTypeA(typeAny: TypeAny) :ReifiableInfoA = {
    typeAny.fold(
      tpe => Left(Right(tpe)),
      _.fold(
        tpe => Left(Left(tpe)),
        refid => Right(refid.ident)
      )
    )
  }

//  def expandNested(tpe :TypeAny, scope :NestedScope): List[Tree] = {
//    tpe.fold(
//      tpea => tpea.select[TypeAComposite] match {
//        case Some(XDRFixedLengthArray) =>
//      },
//      _.fold(
//        tpes => Some(transS(tpes)),
//        id => List.empty
//      )
//    )
//  }

  def reify(tpe :TypeAny, scope :NestedScope): List[Tree] = {
    val Some(tpl) = transAny(tpe, scope)
    val ScalaCodecExpr(codec, ScalaFragmentType(ftype)) = tpl.defaultCodec
    statementsInScope(
      q"""
          type Expr = ${ftype.tree}
          def codec :Codec[Expr] = $codec
          """
    )
  }

  class Snippet(val pkgname :Option[String]) {
      import Extractors._

      def reifyAST(implicit ast :ASTree) = {

        implicit val ns = SemanticProcesser.NamespaceScope(ast)

        val conststat = ast.constdefs.map(kv => {
          val (name, value) = kv
          q"val ${TermName(name)} :Int = ${Constant(value.dig)}"
        })
        val pkgstats = ast.typedefs.map(td => SemanticProcesser.reifyTypeDef(td))

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
      }
    }

    def newSnippet(pkgname :Option[String]): Snippet = new Snippet(pkgname)
  }

  object SemanticProcesser {
    import Extractors._

    abstract class HomomorphicScope(val parent :Option[HomomorphicScope]) {
      //scopes rule in rfc4506 section 6.4
      //
      def resolveType(name :String) :Option[(String, PureType, HomomorphicScope)]
      def resolveConst(name :String) :Option[XDRConstantLiteral]
    }

    case class NamespaceScope(ast :ASTree) extends HomomorphicScope(None) {
      override def resolveType(name :String) = {
        val found = ast.typedefs.find(_.ident == name)
        found.get.tpe.fold(
          tpe => Some((name, Right(tpe), this)),
          _.fold(
            tpe => Some((name , Left(tpe), this)),
            refid => resolveType(refid.ident)
          )
        )
      }
      override def resolveConst(name :String) :Option[XDRConstantLiteral] = {
        ast.constdefs.find(_._1 == name).map(_._2)
      }
    }

    def reifyTypeDef(td :TypeDef)(implicit ns :NamespaceScope) :Tree = {
      val curr = ScalaScaffold.NestedScope(ns)
      ScalaScaffold.createObject(td.ident,
        ScalaScaffold.reify(td.tpe, curr)
      )
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
