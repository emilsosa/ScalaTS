package sts

trait STSScanner {
  def options: Option[STSScannerOptions]

  val imports: Map[String, TSImport]

  val types: Map[String, TSIType]

  override def toString: String = s"$types"

}
