package com.agilogy.validare.validation

import com.agilogy.validare.validation.Validity.Invalid
import cats.implicits._

trait Transformation[A, B] {
  def apply(value: A): Either[Invalid[A], B] = transform(value).toRight(Invalid(requirement.getOrElse(is(this))))
  def transform(value: A): Option[B]
  def apply(other: NonTransformedPredicate[B]): TransformedPredicate[A] = this.satisfies(other)
  def andThen(other: TransformedPredicate[B]): TransformedPredicate[A] { type Result = other.Result } =
    (this andThen other.transformation).satisfies(other.verification)
  def satisfies(other: NonTransformedPredicate[B]): TransformedPredicate[A] { type Result = B } =
    SimpleTransformedPredicate(this, other)
  def requirement: Option[Predicate[A]]                         = Some(is(this))
  def andThen[C](b: Transformation[B, C]): Transformation[A, C] = AndThen(this, b)
}

final case class AndThen[A, B, C](t1: Transformation[A, B], t2: Transformation[B, C]) extends Transformation[A, C] {

  override def apply(value: A): Either[Invalid[A], C] =
    t1(value).flatMap(
      b =>
        t2(b).leftMap {
          case Invalid(p: TransformedPredicate[B])    => Invalid(t1.andThen(p))
          case Invalid(p: NonTransformedPredicate[B]) => Invalid(t1.satisfies(p))
        }
    )

  override def transform(value: A): Option[C] = apply(value).toOption

  override def toString: String = s"$t1.andThen($t2)"
}

final case class Property[I, FT](name: String, getter: I => FT) extends Transformation[I, FT] {

  override def transform(value: I): Some[FT] = Some(getter(value))

  override def toString: String = name

  // TODO: Fix this!
  override def equals(obj: scala.Any): Boolean = obj match {
    //TODO: Check property obj is of the same type
    case Property(`name`, _) => true
    case _                   => false
  }

  override def requirement: None.type = None
}
