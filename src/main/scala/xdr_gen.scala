package org.strllar.scalaxdr

import org.parboiled2.RuleTrace.ZeroOrMore

object xdr_generator {
  import org.parboiled2._

  class XDRParser(val input: ParserInput) extends Parser {
    import CharPredicate.{Digit, Digit19, HexDigit}
    def WhiteSpaceChar = CharPredicate(" \n\r\t\f")
    def QuoteBackslash = CharPredicate("\"\\")
    def QuoteSlashBackSlash = QuoteBackslash ++ "/"
    def WhiteSpace = rule { zeroOrMore(WhiteSpaceChar) }

    // the root rule
    def XDRFiles = rule { WhiteSpace ~ Identifier ~ EOI }

    def Identifier = rule {""} //todo
    def DecimalConstant = rule {""} //todo
    def HexadecimalConstant = rule {""} //todo
    def OctalConstant = rule {""} //todo

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

  def run() {
    val input = scala.io.Source.fromFile("../../../xdr/Stellar-types.x").mkString
    print(input)
    val parser = new XDRParser(input)
    val result = parser.XDRFiles.run()
    println(result)
  }
}

object XDRParser extends App {
  xdr_generator.run()
}