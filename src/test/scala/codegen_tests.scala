package mytests

import org.scalatest._
import org.strllar.scalaxdr.XDRSyntax.TopParser
import org.strllar.scalaxdr.codegen
import shapeless.{HNil, ::}
import scala.reflect.runtime.universe._

import scala.util.Success

object CodeGenSpec extends FlatSpec with Matchers  {
  def verifyCodeGenFull(source :String, ast :Tree): Unit = {
    val parser = new TopParser(source)
    val Success(ns :: spec  :: HNil)  = parser.XDRFiles.run()
    codegen.genAST("test_package", spec).syntax shouldEqual showCode(ast)
  }

  def verifyCodeGenLite(source :String, ast :Tree): Unit = {
    val parser = new TopParser(source)
    val Success(ns :: spec  :: HNil)  = parser.XDRFiles.run()
    val q"package object test_package {..$dontcare}; package test_package {..$stats}"
    = codegen.genAST("test_package", spec)
    showCode(q"..$stats") shouldEqual showCode(ast)
  }

  it should "CodeGen" in {
    it should "generate simple structure" in {
      verifyCodeGenFull(
        """
        |namespace stellar {
        |}
      """.stripMargin,
        q"""
            package object test_package {}
            package test_package { }
          """)

      verifyCodeGenLite(
        """
          |namespace stellar {
          |typedef unsigned hyper uint64;
          |}
        """.stripMargin,
        q"""{
            object uint64 {
              type Expr = BigInt;
              def codec :Codec[Expr] = XDRUnsignedHyper
            }
          }""")
      //TODO simple types
      //TODO enum
      //TODO union
      //      verifyCodeGen(
      //        """
      //          |namespace stellar {
      //          |enum EnvelopeType
      //          |{
      //          |    ENVELOPE_TYPE_SCP = 1,
      //          |    ENVELOPE_TYPE_TX = 2,
      //          |    ENVELOPE_TYPE_AUTH = 3
      //          |};
      //          |
      //          |}
      //        """.stripMargin,
      //        q"""
      //            package test_package {
      //            object EnvelopeType {
      //            abstract class Enum(val value: Int);
      //            case class ENVELOPE_TYPE_SCP() extends Enum(1);
      //            case class ENVELOPE_TYPE_TX() extends Enum(2);
      //            case class ENVELOPE_TYPE_AUTH() extends Enum(3)
      //            }
      //            }
      //          """)
    }
    it should "handle nested anonymous structure" in {
      verifyCodeGenLite(
        """
          |namespace stellar {
          |struct OuterStruct
          |{
          |    int f1;
          |    int f2;
          |    struct {
          |      int f2;
          |    } inner;
          |};
          |}
        """.stripMargin,
        q"""{
            object OuterStruct {
              object inner_aux {
                case class Struct(f2 :Long)
                type Expr = Struct
                def codec :Codec[Expr] = XDRInteger.as[Struct]

                def apply(f2 :Long) = Struct(arg1)
              }
              case class Struct(f1 :Long, f2 :Long, inner :inner_aux.Struct)
              type Expr = Struct;
              def codec :Codec[Expr] = (inner_aux.codec).::(XDRInteger).::(XDRInteger).as[Struct]

              def apply(f1 :Long, f2 :Long, inner :inner_aux.Struct) = Struct(f1, f2, inner)
            }
          }""")
    }
    //TODO optional field
    //TODO array of anonymous struct
    //TODO anonaymous enum
    //TODO array of anonymous enum
    //TODO anoymous union
    //TODO enum in union
    //TODO deeply nested anonymous struct
  }
}