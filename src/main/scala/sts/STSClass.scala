package sts

import sts.Utils.{getConstructorParameters, runtimeTypeToTSType}

import scala.reflect.runtime.{universe => ru}

case class STSClass[T: ru.TypeTag](options: Option[STSScannerOptions] = None) extends STSScanner with Logging {
  self =>

  private val typeTag                       = ru.typeTag[T]
  private val name: String                  = ru.typeOf[T].typeSymbol.asClass.name.decodedName.toString
  private val fieldNameToTypeInformationMap = getConstructorParameters(typeTag.tpe.typeSymbol.asClass)
  private val interface = TSIInterface(
    name = name,
    members = fieldNameToTypeInformationMap.map {
      case (fieldName, t) =>
        fieldName -> runtimeTypeToTSType(t)(options)
    },
    takesGenericArguments = typeTag.tpe.takesTypeArgs
  )
  override val imports: Map[String, TSImport] = Map.empty
  override val types: Map[String, TSIType]    = Map(name -> interface)

}
