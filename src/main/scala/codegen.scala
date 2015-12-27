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
      genAST(ns, spec).children.init.flatMap(
        showCode(_) +  System.lineSeparator()
      )
    )
  }
}

package codegen {

  object ScalaScaffold {
    import Extractors._

    trait scalaType {
      def defineAs(name :String) :Tree = q"{}"
      def declareAs(name :String, tpe :String) :Tree = Typed(q"${TermName(name)}", tq"${TypeName(tpe)}")
      def codecAs(name :String) :Tree = q"${TermName(name)}.codec"
    }
    class DummyScalaType extends scalaType

    class XDREnumType(enum :SemanticProcesser.EnumScheme) extends scalaType {
      override def defineAs(name :String) = {
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
      override def declareAs(name :String, tpe :String) =   Typed(q"${TermName(name)}", tq"${TermName(tpe)}.Enum")
    }

    class XDRStructType(struct :SemanticProcesser.StructScheme) extends scalaType {
      override def defineAs(name :String) = {
        val (components, inplacedefs) = struct
        val codec = components.init.foldRight(q"${components.last._3.codecAs(components.last._2)}")(
          (c, acc) => {
            q"$acc.::(${c._3.codecAs(c._2)})"
          }
        )
        val decls = components.map(c => c._3.declareAs(c._1, c._2))

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
      override def defineAs(name :String) = {
        val n = name
        q"""{
          object ${TermName(n)} {
              trait Arm {
              val v :Int
              }
              class discriminant_0() extends Arm {
                  val v = 0
              }
              type Union = discriminant_0 :+: CNil
              implicit def codec :Codec[Union] = codecs.discriminated[Union].by(XDRInteger).caseO(0)(
                _.select[discriminant_0].map(x => ()))(
                  (x) => Coproduct[Union](new discriminant_0()))(
                     XDRVoid
                     )
          }
        }"""
      }
      override def declareAs(name :String, tpe :String) =   Typed(q"${TermName(name)}", tq"${TermName(tpe)}.Union")
    }

    object nestedMapping extends Poly1 {
      implicit def caseEnum(implicit ast :ASTree) = at[XDREnumeration](x =>  {
        new XDREnumType(SemanticProcesser.transEnum(x.body))
      })
      implicit def caseStruct(implicit ast :ASTree) = at[XDRStructure](x => {
        new XDRStructType(SemanticProcesser.transStruct(x.body))
      })
      implicit def caseUnion(implicit ast :ASTree) = at[XDRUnion](x => {
        new XDRUnionType(SemanticProcesser.transUnion(x.body))
      })
    }

    object compoundMapping extends Poly1 {
      implicit def caseArrayF(implicit ast :ASTree) = at[XDRFixedLengthArray](x => {
        //TODO FIX
//        val (name, undertpe) = SemanticProcesser.transType(x.typespec).fold(
//          (idref) => {
//            SemanticProcesser.resolveType(idref.dig).map((found) => {
//              (Some(found._1), ScalaScaffold.trans(found._2))
//            }).getOrElse({
//              throw new Exception(s"can't resolve type ${idref.dig}")
//              (None, null:scalaType)
//            })
//          },
//          (tpe) => (None, ScalaScaffold.trans(tpe))
//        )
//        val len = x.length.dig.fold(
//          (id) => q"${TermName(id.ident)}",
//          (value) => q"${Constant(value.dig)}")
//
//        new scalaType {
//          override def declareAs(name :String, tpe :String) :Tree = Typed(q"${TermName(name)}", tq"Vector[${undertpe.declareAs(name, tpe)}]")
//          override def codecAs(name :String) :Tree = q"XDRFixedLengthArray(${undertpe.codecAs(name)})(len)"
//        }
        new DummyScalaType
      })
      implicit def caseArrayV(implicit ast :ASTree) = at[XDRVariableLengthArray](x => {

        //TODO FIX
//        val (name, undertpe) = SemanticProcesser.transType(x.typespec).fold(
//          (idref) => {
//            SemanticProcesser.resolveType(idref.dig).map((found) => {
//              (Some(found._1), ScalaScaffold.trans(found._2))
//            }).getOrElse({
//              throw new Exception(s"can't resolve type ${idref.dig}")
//              (None, null:scalaType)
//            })
//          },
//          (tpe) => (None, ScalaScaffold.trans(tpe))
//        )
//        val len = x.maxlength.map(_.dig.fold(
//          (id) => q"Some(${TermName(id.ident)})",
//          (value) => q"Some(${Constant(value.dig)})")
//        ).getOrElse(q"None")
//
//        //TODO...
//        new scalaType {
//          override def declareAs(name :String, tpe :String) :Tree = Typed(q"${TermName(name)}", tq"Vector[${undertpe.declareAs(name, tpe)}]")
//          override def codecAs(name :String) :Tree = q"XDRVariableLengthArray(${undertpe.codecAs(name)})(len)"
//        }
        new DummyScalaType
      })
      implicit def caseOption(implicit ast :ASTree) = at[XDROptional](x => {
        new DummyScalaType
      })

    }

    object scalaMapping extends Poly1 {
      implicit def casenil = at[CNil](_ => new DummyScalaType)

      implicit def caseOpaqueF = at[XDRFixedLengthOpaque](x =>
        new scalaType {
          override def declareAs(name :String, tpe :String) :Tree = Typed(q"${TermName(name)}", tq"Vector[Byte]")
          override def codecAs(name :String) :Tree = x.length.dig.fold(
            (id) => q"XDRFixedLengthOpaque(${TermName(id.ident)})",
            (value) => q"XDRFixedLengthOpaque(${Constant(value.dig)})"
          )
        })
      implicit def caseOpaqueV = at[XDRVariableLengthOpaque](x =>
        new scalaType {
          override def declareAs(name :String, tpe :String) :Tree = Typed(q"${TermName(name)}", tq"Vector[Byte]")
          override def codecAs(name :String) :Tree = x.maxlength.map(
            _.dig.fold(
            (id) => q"XDRVariableLengthOpaque(Some(${TermName(id.ident)}))",
            (value) => q"XDRVariableLengthOpaque(Some(${Constant(value.dig)}))")
          ).getOrElse(q"XDRVariableLengthOpaque(None)")
        })
      implicit def caseString = at[XDRString](x =>
        new scalaType {
          override def declareAs(name :String, tpe :String) :Tree = Typed(q"${TermName(name)}", tq"String")
          override def codecAs(name :String) :Tree = x.maxlength.map(
            _.dig.fold(
              (id) => q"XDRString(Some(${TermName(id.ident)}))",
              (value) => q"XDRString(Some(${Constant(value.dig)}))")
          ).getOrElse(q"XDRString(None)")
        })
      implicit def caseVoid = at[XDRVoid](x =>
        new scalaType {
          override def declareAs(name :String, tpe :String) :Tree = Typed(q"${TermName(name)}", tq"Unit")
          override def codecAs(name :String) :Tree = q"XDRVoid"
        })
      implicit def caseInt = at[XDRInteger.type](x =>
        new scalaType {
          override def declareAs(name :String, tpe :String) :Tree = Typed(q"${TermName(name)}", tq"Int")
          override def codecAs(name :String) :Tree = q"XDRInteger"
        })
      implicit def caseUInt = at[XDRUnsignedInteger.type](x =>
        new scalaType {
          override def declareAs(name :String, tpe :String) :Tree = Typed(q"${TermName(name)}", tq"Long")
          override def codecAs(name :String) :Tree = q"XDRUnsignedInteger"
        })
      implicit def caseHyper = at[XDRHyper](x =>
        new scalaType {
          override def declareAs(name :String, tpe :String) :Tree =
            if (x.signed) Typed(q"${TermName(name)}", tq"Long")
            else Typed(q"${TermName(name)}", tq"BigInt")
          override def codecAs(name :String) :Tree =
            if (x.signed) q"XDRHyper"
            else q"XDRUnsignedHyper"
        })
      implicit def caseFloat = at[XDRFloat.type](x =>
        new scalaType {
          override def declareAs(name :String, tpe :String) :Tree = Typed(q"${TermName(name)}", tq"Float")
          override def codecAs(name :String) :Tree = q"XDRFloat"
        })
      implicit def caseDouble = at[XDRDouble.type](x =>
        new scalaType {
          override def declareAs(name :String, tpe :String) :Tree = Typed(q"${TermName(name)}", tq"Double")
          override def codecAs(name :String) :Tree = q"XDRDouble"
        })
      implicit def caseQuad = at[XDRQuadruple.type](_ => new DummyScalaType) //TODO
//      (x =>
//        new scalaType {
//          override def declareAs(name :String, tpe :String) :Tree = Typed(q"${TermName(name)}", tq"BigDecimal")
//          override def codecAs(name :String) :Tree = q"XDRQuadruple"
//        })

      implicit def caseBoolean = at[XDRBoolean.type](x =>
        new scalaType {
          override def declareAs(name :String, tpe :String) :Tree = Typed(q"${TermName(name)}", tq"Boolean")
          override def codecAs(name :String) :Tree = q"XDRBoolean"
        })

      implicit def caseNested(implicit mapper: Mapper[nestedMapping.type, NestedType], ast :ASTree) = at[NestedType](
        _.map(nestedMapping)
      )
      implicit def casePrimary(implicit mapper: Mapper[scalaMapping.type, PrimaryType]) = at[PrimaryType](
        _.map(scalaMapping)
      )
      implicit def caseComposite(implicit mapper: Mapper[compoundMapping.type, CompositeType]) = at[CompositeType](
        _.map(compoundMapping)
      )
      implicit def caseFlat(implicit mapper: Mapper[scalaMapping.type, FlatType] ,ast :ASTree) = at[FlatType](
        _.flatMap(scalaMapping)
      )
    }

    def trans(x :AnyType)(implicit ast :ASTree) :scalaType = {
      x.flatMap(scalaMapping).unify
    }

    class Snippet(val pkgname :Option[String]) {
      import Extractors._

      def reifyAST(implicit ast :ASTree) = {

        val conststat = ast.constdefs.map(kv => {
          val (name, value) = kv
          q"val ${TermName(name)} :Int = ${Constant(value.dig)}"
        })
        val pkgstats = ast.typedefs.flatMap(td => SemanticProcesser.reifyType(td._1, td._2))

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

    def reifyType(n :String, x :TypeOrRef)(implicit ast :ASTree) :List[Tree] = {
      val compoundtree = x.fold(
        (idref) => {
          resolveType(idref.dig).map((found) => {
            q""
            //uncomment to instantial aliased type
            //ScalaScaffold.trans(found._2).defineAs(n)
          }).getOrElse({
            throw new Exception(s"can't resolve type ${idref.dig}")
            q""
          })
        },
        (tpe) => ScalaScaffold.trans(tpe).defineAs(n)
      )
      if (compoundtree.children.length > 1)
        compoundtree.children.init
      else List.empty[Tree]
    }

    type EnumScheme = Seq[(String, XDRConstantLiteral)]
    def transEnum(x :XDREnumBody)(implicit ast :ASTree) :EnumScheme = {
      val items = x.items.map(item => (
        item._1.dig,
        item._2.dig.fold(
          idref => SemanticProcesser.resolveConst(idref.dig).get,
          value => value
        )))
      items
    }

    type StructScheme = (Seq[(String, String, ScalaScaffold.scalaType)],List[Tree])
    def transStruct(x :XDRStructBody)(implicit ast :ASTree) :StructScheme = {

      val inplacedefs = List.newBuilder[Tree]

      val components = x.components.map(transDecl(_) match {
        case (fieldname, Left(idref)) => {
          resolveType(idref.dig).
            map((found) =>(fieldname, found._1, ScalaScaffold.trans(found._2))).
            getOrElse({
              throw new Exception(s"can't resolve type ${idref.dig} of field $fieldname in struct")
              (fieldname, "", null:ScalaScaffold.scalaType)
            })
        }
        case (fieldname, Right(tpe)) => {
          val inplacetype = s"anontype_$fieldname"
          inplacedefs ++= expandNested(inplacetype, tpe)
          (fieldname, inplacetype, ScalaScaffold.trans(tpe))
        }
      })

      (components, inplacedefs.result())
    }

    type UnionScheme = Unit
    def transUnion(x :XDRUnionBody) :UnionScheme = {
    }

    def expandNested(n :String, anyType: AnyType)(implicit ast :ASTree) :List[Tree] = {
////      val shallow_defs =
////        anyType.select[NestedType].toList.flatMap( nt =>
////          nt.select[XDREnumeration].map(enum => {
////            reifyEnum(n, enum.body).children.init
////          }).getOrElse(
////            nt.select[XDRStructure].map(struct => {
////              reifyStruct(n, struct.body).children.init
////            }).getOrElse(
////              nt.select[XDRUnion].toList.flatMap(union => {
////                reifyUnion(n, union.body).children.init
////              })
////            )
////          )
////        )
////      shallow_defs
      List.empty[Tree]
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
