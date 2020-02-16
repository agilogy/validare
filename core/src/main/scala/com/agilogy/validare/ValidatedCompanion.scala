package com.agilogy.validare

import scala.reflect.ClassTag

import cats.implicits._
import com.agilogy.validare.validation.Predicate
import com.agilogy.validare.validation.Validity.{ Invalid, Valid }

final case class ValidationError[A, B](typeName: String, failsPredicate: Predicate[B]) extends Exception {
  override def getMessage: String = s"Error validating $typeName. The value fails to satisfy $failsPredicate."
}

trait ValidatedCompanionLike[A, B] {

  def predicate: Predicate[A]
  def unsafe(value: A): B
  def typeName: String

  def apply(value: A): Either[ValidationError[B, A], B] = predicate(value) match {
    case Valid                    => unsafe(value).asRight
    case Invalid(failedPredicate) => ValidationError[B, A](typeName, failedPredicate).asLeft
  }

}

abstract class ValidatedCompanion[A: ClassTag, B](validation: Predicate[A])(build: A => B)
    extends ValidatedCompanionLike[A, B] {
  override def predicate: Predicate[A] = validation
  override def unsafe(value: A): B     = build(value)
  override def typeName: String        = implicitly[ClassTag[A]].toString()
}
