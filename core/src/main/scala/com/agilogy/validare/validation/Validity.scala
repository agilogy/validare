package com.agilogy.validare.validation

import scala.language.existentials

sealed trait Validity extends Product with Serializable {

  def isValid: Boolean = this match {
    case Validity.Valid => true
    case Validity.Invalid(_) => false
  }

  def &&(other: Validity): Validity = (this, other) match {
    case (Validity.Valid, _) => other
    case (_, Validity.Valid) => this
    case (Validity.Invalid(p1), Validity.Invalid(p2)) => Validity.Invalid(p1 && p2)
  }

  def ||(other: Validity): Validity = (this, other) match {
    case (Validity.Valid, _) => this
    case (_, Validity.Valid) => other
    case (Validity.Invalid(p1), Validity.Invalid(p2)) => Validity.Invalid(p1 || p2)
  }

}

object Validity {

  case object Valid extends Validity

  final case class Invalid(failing: Predicate[_]) extends Validity

}