package org.strllar.scalaxdr

import java.io.{FileOutputStream, PrintWriter}

import org.strllar.scalaxdr.rfc4506.{XDRDefinition, XDRSpecification}

object XDRSyntax {
  import org.parboiled2._

  trait exactRules {self: Parser =>
    import CharPredicate.{Digit, Digit19, HexDigit}

    def WhiteSpaceChar = CharPredicate(" \n\r\t\f")
    def WhiteSpaceSep = rule { oneOrMore(WhiteSpaceChar) }
    def Newline = rule { "\r\n" | "\n" }
    def MultilineComment: Rule0 = rule { "/*" ~ zeroOrMore(MultilineComment | !"*/" ~ ANY) ~ "*/" }
    def Comment: Rule0 = rule {
      MultilineComment |
        ("//" | "%#") ~ zeroOrMore(!Newline ~ ANY) ~ &(Newline | EOI)
    }
    def WL = rule{ zeroOrMore(WhiteSpaceChar | Comment) }

  }

  class TopParser(val input: ParserInput) extends Parser with exactRules {
    import CharPredicate.{Digit, Digit19, HexDigit}

    implicit private[this] def wspStr(s: String): Rule0 = rule { str(s) ~ WL  }
    implicit private[this] def wspChar(s: Char): Rule0 = rule { ch(s) ~WL }

    def KeyWord(s: String) = rule {
      str(s) ~ WhiteSpaceSep
    }
    // the root rule
    def XDRFiles = rule { WL ~ KeyWord("namespace") ~  Identifier ~ "{" ~ Specification ~ "}" ~ EOI }

    def Identifier = rule { capture(CharPredicate.Alpha ~ zeroOrMore(CharPredicate.AlphaNum | ch('_'))) ~> rfc4506.XDRIdentifierLiteral ~ WL }
    def DecimalConstant = rule( capture(optional(ch('-')) ~ oneOrMore(Digit)) ~> rfc4506.DecimalConstant ~WL)
    def HexadecimalConstant =  rule { capture("0x" ~ oneOrMore(HexDigit)) ~> rfc4506.HexadecimalConstant ~WL }
    def OctalConstant = rule( capture("0" ~ oneOrMore("0" | ("1" - "7"))) ~> rfc4506.OctalConstant ~WL )

    def Declaration :Rule1[rfc4506.XDRDeclaration] = rule {
      ("void" ~ push(rfc4506.XDRVoid())) |
      (KeyWord("opaque") ~ Identifier ~ "[" ~ Value ~ "]" ~> rfc4506.XDRFixedLengthOpaque) |
      (KeyWord("opaque") ~ Identifier ~ "<" ~ optional(Value) ~ ">" ~> rfc4506.XDRVariableLengthOpaque) |
      (KeyWord("string") ~ Identifier ~ "<" ~ optional(Value) ~  ">" ~> rfc4506.XDRString) |
      (TypeSpecifier ~ "*" ~ Identifier ~> rfc4506.XDROptional) |
      (TypeSpecifier ~ Identifier ~ "[" ~ Value ~ "]" ~> rfc4506.XDRFixedLengthArray) |
      (TypeSpecifier ~ Identifier ~ "<" ~ optional(Value) ~ ">" ~> rfc4506.XDRVariableLengthArray) |
      (TypeSpecifier ~ Identifier ~> rfc4506.XDRPlainDeclaration)
    }


    def Value :Rule1[rfc4506.XDRValue] = rule { (Constant ~>  rfc4506.XDRConstantValue) |
      (Identifier ~> rfc4506.XDRIdentifierValue) }

    def Constant :Rule1[rfc4506.XDRConstantLiteral] = rule { HexadecimalConstant | OctalConstant | DecimalConstant }

    def TypeSpecifier :Rule1[rfc4506.XDRTypeSpecifier] = rule {
      (optional(KeyWord("unsigned")~push(true)) ~ KeyWord("int") ~> ((s :Option[Boolean]) => if (s.isEmpty) rfc4506.XDRInteger else rfc4506.XDRUnsignedInteger)) |
      (optional(KeyWord("unsigned")~push(true)) ~ KeyWord("hyper") ~> ((s :Option[Boolean]) => rfc4506.XDRHyper(s.isEmpty))) |
        (KeyWord("float") ~ push(rfc4506.XDRFloat)) |
        (KeyWord("double") ~ push(rfc4506.XDRDouble)) |
        (KeyWord("quadruple") ~ push(rfc4506.XDRQuadruple)) |
        (KeyWord("bool") ~ push(rfc4506.XDRBoolean)) |
        (EnumTypeSpec ~> rfc4506.XDREnumeration) |
        (StructTypeSpec ~> rfc4506.XDRStructure) |
        (UnionTypeSpec ~> rfc4506.XDRUnion) |
        (Identifier ~> rfc4506.XDRIdentifierTypeSpecifier)
    }

    def EnumTypeSpec = rule {"enum" ~ EnumBody }
    def EnumBody = rule {
      "{" ~
        ( Identifier ~ "=" ~ Value)  ~> ((x :rfc4506.XDRIdentifierLiteral, y :rfc4506.XDRValue) => Vector((x, y))) ~
        zeroOrMore("," ~ Identifier ~ "=" ~ Value ~> ((_:Vector[(rfc4506.XDRIdentifierLiteral, rfc4506.XDRValue)]) :+ (_, _))) ~
       "}" ~> rfc4506.XDREnumBody
    }

    def StructTypeSpec = rule {"struct" ~ StructBody }
    def StructBody = rule {
      "{" ~
        (Declaration ~ ";") ~> (Vector(_:rfc4506.XDRDeclaration)) ~
        zeroOrMore(Declaration ~ ";" ~> ((_:Vector[rfc4506.XDRDeclaration]) :+ _)) ~
        "}" ~> rfc4506.XDRStructBody
    }

    def UnionTypeSpec = rule {"union" ~ UnionBody}
    def UnionBody = rule {
      "switch" ~ "(" ~ Declaration ~ ")" ~ "{" ~
        CaseSpec ~> (Vector(_:rfc4506.XDRCaseSpec)) ~
        zeroOrMore( CaseSpec ~> ((_:Vector[rfc4506.XDRCaseSpec]) :+ _)) ~
        optional("default" ~ ":" ~ Declaration ~ ";") ~
        "}" ~> rfc4506.XDRUnionBody
    }

    def CaseSpec = rule {
      ( "case" ~ Value  ~ ":") ~> (Vector(_:rfc4506.XDRValue)) ~
        zeroOrMore( "case" ~ Value ~ ":" ~> ((_:Vector[rfc4506.XDRValue]) :+ _)) ~
        Declaration ~ ";" ~> rfc4506.XDRCaseSpec
    }

    def ConstantDef = rule { ("const" ~ Identifier ~ "=" ~ Constant ~ ";") ~> rfc4506.XDRConstant}

    def TypeDef :Rule1[rfc4506.XDRTypeDef] = rule {
      ("typedef" ~ Declaration ~ ";" ~> rfc4506.XDRPlainTypedef) |
        ("enum" ~ Identifier ~ EnumBody ~ ";" ~> rfc4506.XDREnumTypedef) |
        ("struct" ~ Identifier ~ StructBody ~ ";" ~> rfc4506.XDRStructTypedef) |
        ("union" ~ Identifier ~ UnionBody ~ ";" ~> rfc4506.XDRUnionTypedef)
    }

    type TypeOrConst = Either[rfc4506.XDRTypeDef, rfc4506.XDRConstant]
    def Definition = rule { ((TypeDef ~> (Left(_:rfc4506.XDRTypeDef):TypeOrConst)) | (ConstantDef ~> (Right(_:rfc4506.XDRConstant):TypeOrConst))) ~> rfc4506.XDRDefinition }

    def Specification = rule { push(Vector.empty[rfc4506.XDRDefinition]) ~ zeroOrMore(Definition ~> ((_:Vector[rfc4506.XDRDefinition]) :+ _)) ~> rfc4506.XDRSpecification }

  }

}

object xdrGenApp extends App {
  import org.parboiled2.{ErrorFormatter, ParseError}
  import scala.util.{Success, Failure}
  import shapeless._

  def xdrPathes = Seq(
    "/xdr/Stellar-SCP.x",
    "/xdr/Stellar-types.x",
    "/xdr/Stellar-ledger-entries.x",
    "/xdr/Stellar-transaction.x",
    "/xdr/Stellar-ledger.x",
    "/xdr/Stellar-overlay.x"
  )

  def run() {
    val (ns, defs) =
      xdrPathes.foldLeft(("stellar", Vector.empty[XDRDefinition]))(
        (acc, file) => {
          val input = scala.io.Source.fromFile("../../.." + file).mkString
          val parser = new XDRSyntax.TopParser(input)
          val result = parser.XDRFiles.run()
          result match {
            case Failure(error: ParseError) => {
              println(s"//$file: parse error")
              println(parser.formatError(error, new ErrorFormatter(showTraces = true)))
              acc
            }
            case Success(ns :: spec  :: HNil) => {
              println(s"//$file: parse done")
              if (ns.ident == acc._1) (acc._1, acc._2 ++ spec.defs)
              else {
                println(s"//unmatched namespace of $file: Expected ${acc._1} and Found ${ns.ident}")
                acc
              }
            }
          }
        }
      )

    val outfile = new PrintWriter(new FileOutputStream("../../src/main/scala/xdr_generated.scala"))
    org.strllar.scalaxdr.codegen.genScala("xdr_generated", XDRSpecification(defs)).getLines.foreach(outfile.println)
    outfile.close()
    //org.strllar.scalaxdr.codegen.genScala(ns, XDRSpecification(defs)).getLines.foreach(println)
  }

  run()
}