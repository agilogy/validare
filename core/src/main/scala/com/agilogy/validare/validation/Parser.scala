package com.agilogy.validare.validation

import com.agilogy.validare.validation.Validity.{ Invalid, Valid }
import cats.implicits._

trait Parser[A, B] {
  def parse(input: A): Either[Invalid[A], B]
  def asPredicate: Predicate[A]
  def andThen[C](conversion: Conversion[B, C]): Parser[A, C] = flatMap(Parser.of(conversion))
  def flatMap[C](parser: Parser[B, C]): Parser[A, C]         = Parser.FlatMap(this, parser)
  def &&(predicate: Predicate[B]): Parser[A, B]              = Parser.TailVerification(this, predicate)
  private[validation] def mapPredicate(predicate: Predicate[B]): Predicate[A]
}

object Parser {
  def of[A, B](predicate: Predicate[A], property: Property[A, B]): Parser[A, B] = Base(predicate, property)
  def of[A, B](conversion: Conversion[A, B]): Parser[A, B]                      = Simple(None, conversion)
  def of[A, B](predicate: Predicate[A], conversion: Conversion[A, B]): Parser[A, B] =
    Simple(Some(predicate), conversion)

  private final case class Base[A, B](predicate: Predicate[A], property: Property[A, B]) extends Parser[A, B] {
    override def parse(input: A): Either[Invalid[A], B] = predicate(input) match {
      case Invalid(p) => Invalid(p).asLeft[B]
      case Valid      => property.getter(input).asRight[Invalid[A]]
    }
    override private[validation] def mapPredicate(predicate: Predicate[B]): Predicate[A] = property.satisfies(predicate)

    override def asPredicate: Predicate[A] = predicate
  }
  private final case class Simple[A, B](predicate: Option[Predicate[A]], conversion: Conversion[A, B])
      extends Parser[A, B] {
    override def parse(input: A): Either[Invalid[A], B] = {
      val predicateResult = predicate.map(_.apply(input)).getOrElse(Valid)
      (predicateResult, conversion.convert(input)) match {
        case (Invalid(p1), Left(Invalid(p2))) => Invalid(p1 && p2).asLeft[B]
        case (Invalid(p1), Right(_))          => Invalid(p1).asLeft[B]
        case (_, res)                         => res
      }
    }
    override def asPredicate: Predicate[A] = predicate.map(_ && conversion).getOrElse(conversion)
    override private[validation] def mapPredicate(predicate: Predicate[B]): Predicate[A] =
      conversion.satisfies(predicate)
  }
  private final case class FlatMap[A, B, C](left: Parser[A, B], right: Parser[B, C]) extends Parser[A, C] {
    override def parse(input: A): Either[Invalid[A], C] =
      left
        .parse(input)
        .leftMap(i => Invalid(i.failing && left.mapPredicate(right.asPredicate)))
        .flatMap(right.parse(_).leftMap(i => Invalid(left.mapPredicate(i.failing))))

    override def asPredicate: Predicate[A] = left.asPredicate && left.mapPredicate(right.asPredicate)
    override private[validation] def mapPredicate(predicate: Predicate[C]): Predicate[A] =
      left.mapPredicate(right.mapPredicate(predicate))
  }
  private final case class TailVerification[A, B](parser: Parser[A, B], predicate: Predicate[B]) extends Parser[A, B] {
    override def parse(input: A): Either[Invalid[A], B] =
      parser.parse(input).leftMap(i => Invalid(i.failing && parser.mapPredicate(predicate))).flatMap { res =>
        predicate(res) match {
          case Valid      => res.asRight[Invalid[A]]
          case Invalid(p) => Invalid(parser.mapPredicate(p)).asLeft[B]
        }
      }
    override def asPredicate: Predicate[A] = parser.asPredicate && parser.mapPredicate(predicate)
    override private[validation] def mapPredicate(predicate: Predicate[B]): Predicate[A] =
      parser.mapPredicate(predicate)
  }
}
