package mytests

import com.inthenow.zcheck.SpecLite
import org.strllar.scalaxdr.XDRSyntax.TopParser
import org.strllar.scalaxdr.codegen
import shapeless.{HNil, ::}
import scala.reflect.runtime.universe._

import scala.util.Success

object CodeGenSpec extends SpecLite {
  def verifyCodeGen(source :String, ast :Tree): Unit = {
    val parser = new TopParser(source)
    val Success(ns :: spec  :: HNil)  = parser.XDRFiles.run()
    //showRaw(codegen.genAST("test_package", spec)) must_== showRaw(ast)
    showCode(codegen.genAST("test_package", spec)) must_== showCode(ast)
  }
  "CodeGen" should {
    "generate simple structure" in {
      verifyCodeGen(
        """
        |namespace stellar {
        |}
      """.stripMargin,
        q"""
            package test_package { }
          """)

      verifyCodeGen(
        """
          |namespace stellar {
          |enum EnvelopeType
          |{
          |    ENVELOPE_TYPE_SCP = 1,
          |    ENVELOPE_TYPE_TX = 2,
          |    ENVELOPE_TYPE_AUTH = 3
          |};
          |
          |}
        """.stripMargin,
        q"""
            package test_package {
            object EnvelopeType {
            abstract class Enum(val value: Int);
            case class ENVELOPE_TYPE_SCP() extends Enum(1);
            case class ENVELOPE_TYPE_TX() extends Enum(2);
            case class ENVELOPE_TYPE_AUTH() extends Enum(3)
            }
            }
          """)
    }
  }
}