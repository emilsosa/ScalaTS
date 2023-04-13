package sts.annotations

import java.lang.annotation.Retention
import java.lang.annotation.RetentionPolicy

@Retention(RetentionPolicy.RUNTIME)
case class Exclude() extends scala.annotation.StaticAnnotation
