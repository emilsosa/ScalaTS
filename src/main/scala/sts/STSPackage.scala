package sts

import org.reflections.Reflections
import org.reflections.scanners.SubTypesScanner
import sts.Utils.{getAllCaseClassesInTypeRecursive, getConstructorParameters, runtimeTypeToTSType}

import scala.collection.JavaConverters.iterableAsScalaIterableConverter
import scala.reflect.runtime.{universe => ru}

case class STSPackage(targetPackage: String, implicit val options: Option[STSScannerOptions] = None)
    extends STSScanner
    with Logging {
  self =>

  private val mirror: ru.Mirror = ru.runtimeMirror(getClass.getClassLoader)
  private val reflections: Iterable[String] =
    new Reflections(targetPackage, new SubTypesScanner(false)).getAllTypes.asScala.filter { reflection =>
      val classSymbol             = mirror.staticClass(reflection)
      val isADirectClassInPackage = classSymbol.owner.fullName == targetPackage

      (options.exists(
        _.includeSubPackages
      ) || isADirectClassInPackage) && classSymbol.isClass && classSymbol.asClass.isCaseClass
    }

  private val nameToTSIType: Map[String, TSIType] = reflections.foldRight(Map.empty[String, TSIType]) {
    (reflection, map) =>
      val classSymbol = mirror.staticClass(reflection)

      def classSymbolToTsInterface(symbol: ru.ClassSymbol): TSIType = {
        val fieldNameToTypeInformationMap = getConstructorParameters(symbol)
        val name = {
          val className = symbol.name.decodedName.toString
          options.map(_.classNameTransformer).map(_.transform(className)).getOrElse(className)
        }

        TSIInterface(
          name = name,
          members = fieldNameToTypeInformationMap.map {
            case (fieldName, typeInformation) =>
              fieldName -> runtimeTypeToTSType(typeInformation)
          },
          takesGenericArguments = symbol.typeSignature.takesTypeArgs
        )
      }

      val fieldNameToTypeInformationMap = getConstructorParameters(classSymbol)
      val dependencies                  = fieldNameToTypeInformationMap.values.flatMap(getAllCaseClassesInTypeRecursive)
      val dependenciesInterfaces =
        dependencies.map(_.typeSymbol.asClass).map(classSymbolToTsInterface).map(x => x.name -> x).toMap
      val interface = classSymbolToTsInterface(classSymbol)

      dependenciesInterfaces ++ map ++ Map(interface.name -> interface)
  }

  override val imports: Map[String, TSImport] = {
    def getAllTSEnum(tsType: TSType): List[TSType] = {
      List(tsType).flatMap {
        case TSArray(value)                     => getAllTSEnum(value)
        case TSMap(keyName, keyType, valueType) => getAllTSEnum(keyType) ++ getAllTSEnum(valueType)
        case TSOption(value)                    => getAllTSEnum(value)
        case TSEnum(name)                       => List(tsType)
        case _                                  => Nil
      }
    }

    nameToTSIType.values.collect {
      case TSIInterface(name, members, takesGenericArguments) =>
        val types1 = members.values.flatMap(getAllTSEnum)
        types1.collect {
          case t: TSEnum => t.name -> TSImport(t.name, "./enumerations")
        }
    }
  }.flatten.toMap

  override val types: Map[String, TSIType] = nameToTSIType
}
