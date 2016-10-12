package com.agilogy.validare.utils

import scala.util.matching.Regex

object RegexUtils {

  implicit class RegexOps(self: Regex) {
    def matches(s: String): Boolean = self.pattern.matcher(s).matches()
  }

}
