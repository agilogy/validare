package com.agilogy.validare

import cats.implicits._

import com.agilogy.validare.validation.Predicate
import com.agilogy.validare.validation.Validity.{ Invalid, Valid }

final case class ValidationError[A](failsPredicate: Predicate[A]) extends Exception

trait ValidatedCompanionLike[A, B] {

  def predicate: Predicate[A]
  def unsafe(value: A): B

  def apply(value: A): Either[ValidationError[A], B] = predicate(value) match {
    case Valid                    => unsafe(value).asRight
    case Invalid(failedPredicate) => ValidationError(failedPredicate).asLeft
  }

}

abstract class ValidatedCompanion[A, B](validation: Predicate[A])(build: A => B) extends ValidatedCompanionLike[A, B] {
  override def predicate: Predicate[A] = validation
  override def unsafe(value: A): B     = build(value)
}
