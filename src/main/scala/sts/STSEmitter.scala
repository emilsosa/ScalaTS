package sts

import java.io.File
import java.io.FileWriter

object STSEmitter {

  case class Options(
    folder: File,
    fileName: File,
    header: Option[String] = Some("// AUTO-GENERATED FILE, DO NOT EDIT!")
  )

  def emit(scanner: STSScanner, options: Options): Unit = {
    val targetFile = new File(options.folder, s"${options.fileName}")
    Option(targetFile.getParentFile).foreach(_.mkdirs())
    if (targetFile.exists()) {
      targetFile.delete();
    }
    targetFile.createNewFile()

    val output = options.header.map(_ + "\n\n").getOrElse("") + scanner.imports.values
      .map(printExternalReferences)
      .mkString("", "\n", "\n\n\n") + scanner.types.values.map(printIType).mkString("\n")

    val fw = new FileWriter(targetFile)

    fw.write(output)

    fw.close()
  }

  private def printExternalReferences(reference: TSImport): String = {
    s"""import type { ${reference.name} } from \"${reference.location}\";"""
  }

  private def printIType(tsIType: TSIType): String = {
    tsIType match {
      case TSIInterface(name, members, takesGenericArguments) =>
        val mappedMembers = members.map {
          case (memberName, memberType) =>
            s"\t$memberName: ${printType(memberType)};"
        }
        val generic = Some("<T1>").filter(_ => takesGenericArguments).getOrElse("")

        s"""export interface $name$generic {
           |${mappedMembers.mkString("\n")}
           |}
           |""".stripMargin
      case TSIEnum(name, members) =>
        val mappedMembers = members.map { case (name, value) => s"\t$name = $value" }
        s"""export enum $name {
           |${mappedMembers.mkString(",\n")}
           |}
           |""".stripMargin
    }
  }

  private def printType(tsType: TSType): String = {
    tsType match {
      case TSString                           => "string"
      case TSNumber                           => "number"
      case TSBoolean                          => "boolean"
      case TSUnknown                          => "unknown"
      case TSGeneric(index)                   => s"T$index"
      case TSArray(value)                     => s"${printType(value)}[]"
      case TSOption(value)                    => s"${printType(value)} | undefined"
      case TSMap(keyName, keyType, valueType) => s"{ [$keyName: ${printType(keyType)}]: ${printType(valueType)} }"
      case TSInterface(name)                  => name
      case TSEnum(name)                       => name
    }
  }

}
