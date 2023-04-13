package sts

import org.slf4j.{Logger, LoggerFactory}

import java.time.format.DateTimeFormatter

trait Logging {

  val formatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")

  val logger: Logger = LoggerFactory.getLogger(getClass)


}
