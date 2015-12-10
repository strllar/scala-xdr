package org.strllar.scalaxdr

package rfc4506 {
//refer http://tools.ietf.org/html/rfc4506.html

case class XDRIdentifierLiteral(ident :String) //identifier in section 6.2

//concrete XDRDeclaration
//
//    declaration:
//    type-specifier identifier
//    | type-specifier identifier "[" value "]"
//    | type-specifier identifier "<" [ value ] ">"
//    | "opaque" identifier "[" value "]"
//    | "opaque" identifier "<" [ value ] ">"
//    | "string" identifier "<" [ value ] ">"
//    | type-specifier "*" identifier
//    | "void"
//
sealed trait XDRDeclaration
case class XDRPlainDeclaration(name :XDRIdentifierLiteral, typespec :XDRTypeSpecifier) extends XDRDeclaration
case class XDRFixedLengthArray(name :XDRIdentifierLiteral,typespec :XDRTypeSpecifier, length :XDRValue) extends XDRDeclaration //section 4.12
case class XDRVariableLengthArray(name :XDRIdentifierLiteral,typespec :XDRTypeSpecifier, maxlength :Option[XDRValue]) extends XDRDeclaration //section 4.13
case class XDRFixedLengthOpaque(name :XDRIdentifierLiteral, length :XDRValue) extends XDRDeclaration //section 4.9
case class XDRVariableLengthOpaque(name :XDRIdentifierLiteral, maxlength :Option[XDRValue]) extends XDRDeclaration //section 4.10
case class XDRString(name :XDRIdentifierLiteral, maxlength :Option[XDRValue]) extends XDRDeclaration //section 4.11
case class XDROptional(name :XDRIdentifierLiteral,typespec :XDRTypeSpecifier) extends XDRDeclaration //section 4.19
case class XDRVoid() extends XDRDeclaration //section 4.16

//concrete XDRValue
//
//    value:
//    constant
//    | identifier
//
sealed trait XDRValue
case class XDRConstantValue(value :String) extends XDRValue
case class XDRIdentifierValue(ident :String) extends XDRValue

//concrete XDRConstant
//
//    constant:
//    decimal-constant | hexadecimal-constant | octal-constant
//
sealed trait XDRConstantLiteral
case class DecimalConstant(value :String) extends XDRConstantLiteral
case class HexadecimalConstant(value :String) extends XDRConstantLiteral
case class OctalConstant(value :String) extends XDRConstantLiteral

//concrete XDRTypeSpecifier
//
//    type-specifier:
//    [ "unsigned" ] "int"
//    | [ "unsigned" ] "hyper"
//    | "float"
//    | "double"
//    | "quadruple"
//    | "bool"
//    | enum-type-spec
//    | struct-type-spec
//    | union-type-spec
//    | identifier
//
sealed trait XDRTypeSpecifier
case object XDRInteger extends XDRTypeSpecifier  //section 4.1
case object XDRUnsignedInteger extends XDRTypeSpecifier //section 4.2
case class XDRHyper(signed :Boolean) extends XDRTypeSpecifier //section 4.5
case object XDRFloat extends XDRTypeSpecifier //section 4.6
case object XDRDouble extends XDRTypeSpecifier  //section 4.7
case object XDRQuadruple extends XDRTypeSpecifier //section 4.8
case object XDRBoolean extends XDRTypeSpecifier //section 4.4
case class XDREnumeration(body :XDREnumBody) extends XDRTypeSpecifier //section 4.3 (anonymous)
case class XDRStructure(body :XDRStructBody) extends XDRTypeSpecifier //section 4.14 (anonymous)
case class XDRUnion(body :XDRUnionBody) extends XDRTypeSpecifier //section 4.15 (anonymous)
case class XDRIdentifierTypeSpecifier(body :XDRIdentifierLiteral) extends XDRTypeSpecifier

//XDREnumBody
//
//    enum-body:
//    "{"
//    ( identifier "=" value )
//    ( "," identifier "=" value )*
//    "}"
//
case class XDREnumBody(items :Vector[(XDRIdentifierLiteral, XDRValue)])

//XDRStructBody
//
//    struct-body:
//    "{"
//    ( declaration ";" )
//    ( declaration ";" )*
//    "}"
//
case class XDRStructBody(components :Vector[XDRDeclaration])

//XDRUnionBody
//
//    union-body:
//    "switch" "(" declaration ")" "{"
//    case-spec
//    case-spec *
//    [ "default" ":" declaration ";" ]
//    "}"
//
case class XDRUnionBody(discriminant :XDRDeclaration, arms :Vector[XDRCaseSpec], defdecl :Option[XDRDeclaration])

//XDRCaseSpec
//
//    case-spec:
//    ( "case" value ":")
//    ( "case" value ":") *
//    declaration ";"
//
case class XDRCaseSpec(values :Vector[XDRValue], declaration :XDRDeclaration)

//XDRConstantDef
//
//    constant-def:
//    "const" identifier "=" constant ";"
//
case class XDRConstant(name :XDRIdentifierLiteral, value :XDRConstantLiteral) //section 4.17

//XDRTypeDef
//
//    type-def:
//    "typedef" declaration ";"
//    | "enum" identifier enum-body ";"
//    | "struct" identifier struct-body ";"
//    | "union" identifier union-body ";"
//
sealed trait XDRTypeDef
case class XDRPlainTypedef(declaration :XDRDeclaration) extends XDRTypeDef //section 4.18
case class XDREnumType(name :XDRIdentifierLiteral, body :XDREnumBody) extends XDRTypeDef //section 4.3
case class XDRStructType(name :XDRIdentifierLiteral, body :XDRStructBody) extends XDRTypeDef //section 4.14
case class XDRUnionType(name :XDRIdentifierLiteral, body :XDRUnionBody) extends XDRTypeDef //section 4.15

//XDRDefinition
//
//    definition:
//    type-def
//    | constant-def
//
case class XDRDefinition(anydef :Either[XDRTypeDef, XDRConstant])

//XDRSpecification
//
//    specification:
//    definition *
//
case class XDRSpecification(defs :Vector[XDRDefinition])


}

package object rfc4506 {

}
