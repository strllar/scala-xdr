package org.strllar.scalaxdr

package rfc4506 {
//refer http://tools.ietf.org/html/rfc4506.html

trait XDRIdentifier {val ident :String} //identifier in section 6.3
trait XDRConstant {val value :String} //constant in section 6.3
sealed trait XDRDeclaration {val name :String} //declaration in section 6.3
sealed trait XDRValue //value in section 6.3
sealed trait XDRTypeSpecifier //type-specifier in section 6.3
sealed trait XDRTypeDef{val name :String} //type-def in section 6.3

//concrete XDRValue
case class XDRConstantValue(value :String) extends XDRValue with XDRConstant
case class XDRIdentifierValue(ident :String) extends XDRValue with XDRIdentifier

//concrete XDRDeclaration
case class XDRRegularDeclaration(name :XDRIdentifier, typespec :XDRTypeSpecifier) extends XDRDeclaration
case class XDRFixedLengthArray(name :XDRIdentifier,typespec :XDRTypeSpecifier, length :XDRValue) extends XDRDeclaration //section 4.12
case class XDRVariableLengthArray(name :XDRIdentifier,typespec :XDRTypeSpecifier, maxlength :Option[XDRValue]) extends XDRDeclaration //section 4.13
case class XDRFixedLengthOpaque(name :XDRIdentifier, length :XDRValue) extends XDRDeclaration //section 4.9
case class XDRVariableLengthOpaque(name :XDRIdentifier, maxlength :Option[XDRValue]) extends XDRDeclaration //section 4.10
case class XDRString(name :XDRIdentifier, maxlength :Option[XDRValue]) extends XDRDeclaration //section 4.11
case class XDROptional(name :XDRIdentifier,typespec :XDRTypeSpecifier) //section 4.19
case class XDRVoid() extends XDRDeclaration {val name = ""}//section 4.16

//concrete XDRTypeSpecifier
case object XDRInteger extends XDRTypeSpecifier  //section 4.1
case object XDRUnsignedInteger extends XDRTypeSpecifier //section 4.2
case class XDRHyper(signed :Boolean) extends XDRTypeSpecifier //section 4.5
case object XDRFloat extends XDRTypeSpecifier //section 4.6
case object XDRDouble extends XDRTypeSpecifier  //section 4.7
case object XDRQuadruple extends XDRTypeSpecifier //section 4.8
case object XDRBoolean extends XDRTypeSpecifier //section 4.4
case class XDREnumeration() extends XDRTypeSpecifier //section 4.3
case class XDRStructure() extends XDRTypeSpecifier //section 4.14
case class XDRUnion() extends XDRTypeSpecifier //section 4.15
case class XDRIdentifierTypeSpecifier(ident :String) extends XDRTypeSpecifier  with XDRIdentifier

//XDRTypeDef
case class XDRRegularTypedef(val decl :XDRDeclaration) extends XDRTypeDef { val name = decl.name}//section 4.18
//case class  //TODO
//case class XDRConstant(value :String) ////section 4.17





}

package object rfc4506 {

}
