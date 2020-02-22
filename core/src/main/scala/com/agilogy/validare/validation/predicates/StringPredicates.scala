package com.agilogy.validare.validation.predicates

import scala.util.matching.Regex

import com.agilogy.validare.utils.RegexUtils._
import com.agilogy.validare.validation.{ AtomicPredicate, NotPredicate, Predicate }

trait StringPredicates {

  case class startsWith(prefix: String) extends AtomicPredicate[String] {

    override def satisfiedBy(value: String): Boolean = value.startsWith(prefix)

    override def opposite: NotPredicate[String] = NotPredicate(this)
  }

  case class endsWith(suffix: String) extends AtomicPredicate[String] {

    override def satisfiedBy(value: String): Boolean = value.endsWith(suffix)

    override def opposite: NotPredicate[String] = NotPredicate(this)
  }

  case class matches(validationId: String, re: Regex) extends AtomicPredicate[String] {

    override val id: String = validationId

    override def satisfiedBy(value: String): Boolean = re.matches(value)

    override def opposite: NotPredicate[String] = NotPredicate(this)
  }

  private val blank = """\s*""".r

  case object nonBlank extends AtomicPredicate[String] {

    override def satisfiedBy(value: String): Boolean = !blank.matches(value)

    override def opposite: isBlank.type = isBlank

  }

  case object isBlank extends AtomicPredicate[String] {

    override def satisfiedBy(value: String): Boolean = blank.matches(value)

    override def opposite: nonBlank.type = nonBlank

  }

  case object isTrimmed extends AtomicPredicate[String] {
    override def satisfiedBy(value: String): Boolean = value.trim == value

    override def opposite: Predicate[String] = NotPredicate(this)
  }

}
