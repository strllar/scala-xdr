package mytests

import com.inthenow.zcheck.SpecLite
import org.strllar.scalaxdr.XDRSyntax.TopParser
import org.strllar.scalaxdr.codegen
import shapeless.{HNil, ::}
import scala.reflect.runtime.universe._

import scala.util.Success

object CodeGenSpec extends SpecLite {
  "CodeGen" should {
    "generate simple structure" in {
      val input = """
        |namespace stellar {
        |}
      """.stripMargin

      val parser = new TopParser(input)
      val Success(ns :: spec  :: HNil)  = parser.XDRFiles.run()

      showRaw(codegen.genAST(ns.ident, spec)) must_==
        showRaw(q"""
            package xdr_generated { }
            package object xdr_generated {{import org.strllar.scalaxdr.xdrbase._;()}}
            """)

    }
  }
}