package org.strllar.scalaxdr

object xdr_generator {
  import org.parboiled2._

  trait exactRules {self: Parser =>
    import CharPredicate.{Digit, Digit19, HexDigit}

    def WhiteSpaceChar = CharPredicate(" \n\r\t\f")
    def WhiteSpace = rule { zeroOrMore(WhiteSpaceChar) }
    def Newline = rule { "\r\n" | "\n" }
    def MultilineComment: Rule0 = rule { "/*" ~ zeroOrMore(MultilineComment | !"*/" ~ ANY) ~ "*/" }
    def Comment: Rule0 = rule {
      MultilineComment |
        "//" ~ zeroOrMore(!Newline ~ ANY) ~ &(Newline | EOI)
    }
    def WL = rule{ zeroOrMore(WhiteSpaceChar | Comment) }

  }

  class XDRParser(val input: ParserInput) extends Parser with exactRules {
    import CharPredicate.{Digit, Digit19, HexDigit}

    implicit private[this] def wspStr(s: String): Rule0 = rule { str(s) ~ WL  }
    implicit private[this] def wspChar(s: Char): Rule0 = rule { ch(s) ~WL }

    // the root rule
    def XDRFiles = rule { WL ~ "namespace" ~  Identifier ~ "{" ~ Specification ~ "}" ~ EOI }

    def Identifier = rule {CharPredicate.Alpha ~ zeroOrMore(CharPredicate.AlphaNum | ch('_')) ~ WL }
    def DecimalConstant = rule(oneOrMore(Digit))
    def HexadecimalConstant =  rule { "0x" ~ oneOrMore(HexDigit) }
    def OctalConstant = rule( "0" ~ oneOrMore("0" | ("1" - "7")))

    def Declaration :Rule0 = rule {(
      (TypeSpecifier ~ Identifier) |
      (TypeSpecifier ~ Identifier ~ "[" ~ Value ~ "]") |
      (TypeSpecifier ~ Identifier ~ "<" ~ optional(Value) ~ ">") |
      ("opaque" ~ Identifier ~ "[" ~ Value ~ "]") |
      ("opaque" ~ Identifier ~ "<" ~ optional(Value) ~ ">") |
      ("string" ~ Identifier ~ "<" ~ optional(Value) ~  ">") |
      (TypeSpecifier ~ "*" ~ Identifier) |
      ("void")
    )}


    def Value = rule { Constant | Identifier }

    def Constant = rule { DecimalConstant | HexadecimalConstant | OctalConstant }

    def TypeSpecifier = rule {
      (optional("unsigned") ~ "int") |
      (optional("unsigned") ~ "hyper") |
        ("float") |
        ("double") |
        ("quadruple") |
        ("bool") |
        EnumTypeSpec |
        StructTypeSpec |
        UnionTypeSpec |
        Identifier
    }

    def EnumTypeSpec = rule {"enum" ~ EnumBody }
    def EnumBody = rule {
      "{" ~
      ( Identifier ~ "=" ~ Value ) ~
      zeroOrMore("," ~ Identifier ~ "=" ~ Value) ~
       "}"
    }

    def StructTypeSpec = rule {"struct" ~ StructBody }
    def StructBody = rule {
      "{" ~
        (Declaration ~ ";") ~
        zeroOrMore(Declaration ~ ";") ~
        "}"
    }

    def UnionTypeSpec = rule {"union" ~ UnionBody}
    def UnionBody = rule {
      "switch" ~ "(" ~ Declaration ~ ")" ~ "{" ~
        CaseSpec ~
        zeroOrMore( CaseSpec) ~ optional("default" ~ ":" ~ Declaration ~ ";") ~
        "}"
    }

    def CaseSpec = rule {
      ( "case" ~ Value  ~ ":") ~
        zeroOrMore( "case" ~ Value ~ ":") ~
        Declaration ~ ";"
    }

    def ConstantDef = rule { ("const" ~ Identifier ~ "=" ~ Constant ~ ";") }

    def TypeDef = rule {
      ("typedef" ~ Declaration ~ ";") |
        ("enum" ~ Identifier ~ EnumBody ~ ";") |
        ("struct" ~ Identifier ~ StructBody ~ ";") |
        ("union" ~ Identifier ~ UnionBody ~ ";")
    }

    def Definition = rule { TypeDef | ConstantDef }

    def Specification = rule { zeroOrMore(Definition) }

  }

  import scala.util.Failure

  def run() {
    val input = scala.io.Source.fromFile("../../../xdr/Stellar-types.x").mkString
    print(input)
    val parser = new XDRParser(input)
    val result = parser.XDRFiles.run()
    println(result)
    result match {
      case Failure(error :ParseError) => {
        println(parser.formatError(error, new ErrorFormatter(showTraces = true)))

      }
      case _ => {}
    }
  }
}

object XDRParser extends App {
  xdr_generator.run()
}