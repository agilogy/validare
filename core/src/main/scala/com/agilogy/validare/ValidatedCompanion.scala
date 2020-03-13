package com.agilogy.validare

import scala.reflect.ClassTag

import cats.implicits._

import com.agilogy.validare.ValidatedCompanion.ValidatedCompanionLike
import com.agilogy.validare.validation.{ Parser, Predicate, Property }

final case class ValidationError[A, B](typeName: String, failsPredicate: Predicate[B]) extends Exception {
  override def getMessage: String = s"Error validating $typeName. The value fails to satisfy $failsPredicate."
}

abstract class ValidatedCompanion[A, B: ClassTag](validation: Predicate[A])(build: A => B)
    extends ValidatedCompanionLike[A, B] {
  def predicate: Predicate[A] = validation
  def unsafe(value: A): B     = build(value)
  def typeName: String        = implicitly[ClassTag[B]].toString()

  final def parser: Parser[A, B] = Parser.of(predicate, Property(typeName, unsafe))

  override def apply(value: A): Either[ValidationError[B, A], B] =
    parser.parse(value).leftMap(i => ValidationError(typeName, i.failing))
}

object ValidatedCompanion {
  type ValidatedCompanionLike[A, B] = A => Either[ValidationError[B, A], B]
}
