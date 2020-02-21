package com.agilogy.validare

import scala.reflect.ClassTag

import com.agilogy.validare.validation.{ NonMappedPredicate, Parser, Predicate, Property }
import cats.implicits._

final case class ValidationError[A, B](typeName: String, failsPredicate: Predicate[B]) extends Exception {
  override def getMessage: String = s"Error validating $typeName. The value fails to satisfy $failsPredicate."
}

trait ValidatedCompanionLike[A, B] {

  def predicate: Predicate[A]
  def unsafe(value: A): B
  def typeName: String

  final def parser: Parser[A, B] = Parser.of(predicate, Property(typeName, unsafe))

  def apply(value: A): Either[ValidationError[B, A], B] =
    parser.parse(value).leftMap(i => ValidationError(typeName, i.failing))

}

abstract class ValidatedCompanion[A, B: ClassTag](validation: NonMappedPredicate[A])(build: A => B)
    extends ValidatedCompanionLike[A, B] {
  override def predicate: NonMappedPredicate[A] = validation
  override def unsafe(value: A): B              = build(value)
  override def typeName: String                 = implicitly[ClassTag[B]].toString()
}
