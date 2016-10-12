package com.agilogy.validare.parsing.parsers

import com.agilogy.validare.parsing.Transformation

trait StringTransformations {

  case object trim extends Transformation[String] {

    override def transform(value: String): String = value.trim

  }

}
