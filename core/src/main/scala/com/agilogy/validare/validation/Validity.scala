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

  //   trait Invalid[A] extends Validity[A] {
  //    type PredicateType <: Predicate[A]
  //    def failing: PredicateType
  //  }
  //
  //  object Invalid {
  //    def unapply[P <: Predicate[A], A](i: Invalid[A] { type PredicateType = P }): Option[P] = Some(i.failing)
  //    def apply[P <: Predicate[A], A](failing: P): Invalid[A] { type PredicateType = P } = InvalidImpl[P, A](failing)
  //  }
  //
  //  private final case class InvalidImpl[P <: Predicate[A], A](failing: P) extends Invalid[A] {
  //    override type PredicateType = P
  //  }
}
