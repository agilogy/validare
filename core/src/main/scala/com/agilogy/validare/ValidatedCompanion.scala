package com.agilogy.validare

import scala.reflect.ClassTag

import com.agilogy.validare.validation.{ NonTransformedPredicate, Predicate, Property, TransformedPredicate }
import com.agilogy.validare.validation.Validity.{ Invalid, Valid }
import cats.implicits._

final case class ValidationError[A, B](typeName: String, failsPredicate: Predicate[B]) extends Exception {
  override def getMessage: String = s"Error validating $typeName. The value fails to satisfy $failsPredicate."
}

trait ValidatedCompanionLike[A, B] {

  def predicate: NonTransformedPredicate[A]
  def unsafe(value: A): B
  def typeName: String

  private val idTransformation = new Property[A, A]("", identity)
  private val transformation   = new Property[A, B](typeName, unsafe)

  def transformedPredicate: TransformedPredicate[A] { type Result = B } =
    idTransformation.satisfies(predicate).andThen(transformation)

  def apply(value: A): Either[ValidationError[B, A], B] = predicate(value) match {
    case Valid                    => unsafe(value).asRight
    case Invalid(failedPredicate) => ValidationError[B, A](typeName, failedPredicate).asLeft
  }

}

abstract class ValidatedCompanion[A, B: ClassTag](validation: NonTransformedPredicate[A])(build: A => B)
    extends ValidatedCompanionLike[A, B] {
  override def predicate: NonTransformedPredicate[A] = validation
  override def unsafe(value: A): B                   = build(value)
  override def typeName: String                      = implicitly[ClassTag[B]].toString()
}
