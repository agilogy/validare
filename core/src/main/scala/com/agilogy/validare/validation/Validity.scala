package com.agilogy.validare.validation

sealed trait Validity[-A] extends Product with Serializable {

  def isValid: Boolean = this match {
    case Validity.Valid      => true
    case Validity.Invalid(_) => false
  }

  def &&[AA <: A](other: Validity[AA]): Validity[AA] = (this, other) match {
    case (Validity.Valid, _)                          => other
    case (_, Validity.Valid)                          => this
    case (Validity.Invalid(p1), Validity.Invalid(p2)) => Validity.Invalid(p1 && p2)
  }

  def ||[AA <: A](other: Validity[AA]): Validity[AA] = (this, other) match {
    case (Validity.Valid, _)                          => this
    case (_, Validity.Valid)                          => other
    case (Validity.Invalid(p1), Validity.Invalid(p2)) => Validity.Invalid(p1 || p2)
  }

}

object Validity {

  case object Valid extends Validity[Any]

  final case class Invalid[A](failing: Predicate[A]) extends Validity[A]

}
