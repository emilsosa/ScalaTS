package sts

sealed trait TSType

case object TSString extends TSType

case object TSNumber extends TSType

case object TSBoolean extends TSType

case object TSUnknown extends TSType

case class TSGeneric(index: Int) extends TSType

case class TSArray(value: TSType) extends TSType

case class TSMap(keyName: String = "key", keyType: TSType, valueType: TSType) extends TSType

case class TSOption(value: TSType) extends TSType

case class TSInterface(name: String) extends TSType

case class TSEnum(name: String) extends TSType

sealed trait TSIType {
  def name: String

}

case class TSIInterface(name: String, members: Map[String, TSType], takesGenericArguments: Boolean) extends TSIType

case class TSIEnum(name: String, members: List[(String, Int)]) extends TSIType

case class TSImport(name: String, location: String)
