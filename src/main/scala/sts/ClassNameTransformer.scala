package sts

trait ClassNameTransformer {

  def transform(className: String): String

}

object ClassNameTransformer {

  def defaultNameTransformer(string: String, replacement: String): ClassNameTransformer = (className: String) =>
    className.replace(string, replacement)

}
