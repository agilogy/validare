package com.agilogy.validare

import scala.reflect.ClassTag

import cats.implicits._

import com.github.ghik.silencer.silent

import com.agilogy.validare.validation.{ Conversion, Parser, Predicate, Property }

final case class ValidationError[A, B](typeName: String, failsPredicate: Predicate[B]) extends Exception {
  override def getMessage: String = s"Error validating $typeName. The value fails to satisfy $failsPredicate."
}

trait ValidatedCompanion[A, B] extends (A => Either[ValidationError[B, A], B]) {
  def unsafe(value: A): B
  def parser: Parser[A, B]
  def typeName: String
  final override def apply(value: A): Either[ValidationError[B, A], B] =
    parser.parse(value).leftMap(i => ValidationError(typeName, i.failing))
}

abstract class ValidatedCompanionOf[A, B: ClassTag](validation: Predicate[A])(build: A => B)
    extends ValidatedCompanion[A, B] {
  override def typeName: String           = implicitly[ClassTag[B]].toString()
  final override def parser: Parser[A, B] = Parser.of(validation, Property(typeName, unsafe))
  final override def unsafe(value: A): B  = build(value)
}

final case class isEnum[A](override val id: String, all: List[A])(getValue: A => String) extends Conversion[String, A] {
  override def transform(value: String): Option[A] = all.find(getValue(_) === value)
}

abstract class EnumCompanion[A: ClassTag](getValue: A => String) extends ValidatedCompanion[String, A] {
  override def typeName: String = implicitly[ClassTag[A]].toString()
  def all: List[A]
  final override def parser: Parser[String, A] = Parser.of(isEnum(typeName, all)(getValue), Property(typeName, unsafe))
  @silent("OptionPartial")
  final override def unsafe(value: String): A = all.find(getValue(_) === value).get

}
