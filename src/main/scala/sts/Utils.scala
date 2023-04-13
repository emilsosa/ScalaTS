package sts

import sts.annotations.Exclude

import java.io.File
import java.time.{LocalDate, LocalTime, OffsetDateTime}
import scala.annotation.tailrec
import scala.reflect.runtime.{universe => ru}

protected[sts] object Utils extends Logging {

  private def isCaseClass(t: ru.Type): Boolean = {
    t.typeSymbol.isClass && t.typeSymbol.asClass.isCaseClass
  }

  @tailrec
  def getAllCaseClassesInTypeRecursive(t: ru.Type): Option[ru.Type] = {
    t match {
      case t if t.typeSymbol.isClass && t.typeSymbol.asClass.isCaseClass && !isATypeBasedEnumeration(t) => Some(t)
      case t if t.typeArgs.nonEmpty => getAllCaseClassesInTypeRecursive(t.typeArgs.head)
      case _ => None
    }
  }

  def isATypeBasedEnumeration(t: ru.Type): Boolean = {
    if (
      t.typeSymbol.isClass && t.typeSymbol.asClass.isCaseClass && t.typeSymbol.asClass.isAbstract && t.typeSymbol.asClass.isSealed
    ) {
      val classSymbol = t.typeSymbol.asClass
      val packageSymbol = ru.runtimeMirror(getClass.getClassLoader).staticPackage(classSymbol.owner.fullName)
      val subtypeSymbol = packageSymbol.typeSignature.decl(ru.TermName(classSymbol.name.decodedName.toString))
      subtypeSymbol.isModule
    } else {
      false
    }
  }

  def runtimeTypeToTSType(t: ru.Type)(implicit options: Option[STSScannerOptions]): TSType = {
    t match {
      case t if t =:= ru.weakTypeOf[Long] || t =:= ru.weakTypeOf[Int] || t =:= ru.weakTypeOf[BigDecimal] => TSNumber
      case t
        if t =:= ru.weakTypeOf[String] || t =:= ru.weakTypeOf[LocalTime] || t =:= ru.weakTypeOf[LocalDate] || t =:= ru
          .weakTypeOf[OffsetDateTime] =>
        TSString
      case t if t =:= ru.weakTypeOf[Boolean] => TSBoolean
      case t if t =:= ru.weakTypeOf[File] => TSUnknown
      case t if t <:< ru.typeOf[Option[_]] => TSOption(runtimeTypeToTSType(t.typeArgs.head))
      case t if t <:< ru.weakTypeOf[Iterable[_]] && !(t <:< ru.weakTypeOf[Map[_, _]]) =>
        TSArray(runtimeTypeToTSType(t.typeArgs.head))
      case t if t <:< ru.weakTypeOf[Map[_, _]] =>
        TSMap("key", runtimeTypeToTSType(t.typeArgs(0)), runtimeTypeToTSType(t.typeArgs(1)))
      case t if isATypeBasedEnumeration(t) => TSEnum(t.typeSymbol.asClass.name.decodedName.toString)
      case t if isCaseClass(t) =>
        val classSymbol = t.typeSymbol.asClass
        val className = classSymbol.name.decodedName.toString
        val transformedClassName = options.map(_.classNameTransformer.transform(className)).getOrElse(className)
        TSInterface(transformedClassName)
      case t if t.typeSymbol.isAbstract => TSGeneric(1)
      case t =>
        logger.warn(s"Type is unknown... $t")
        logger.warn(s"Using unknown TS type")
        TSUnknown
    }
  }

  def getConstructorParameters(classSymbol: ru.ClassSymbol): Map[String, ru.Type] =
    classSymbol.toType
      .member(ru.termNames.CONSTRUCTOR)
      .asMethod
      .paramLists
      .head
      .foldRight(Map.empty[String, ru.Type])((p, a) => {
        // Todo: Handle exclusions somehow.
        // Todo: Extract exclusion of sessionInformation from here.
        if (
          p.annotations.exists(_.tree.tpe =:= ru.typeOf[Exclude])
        ) {
          a
        } else {
          a + (p.name.decodedName.toString -> p.typeSignature)
        }
      })
}
