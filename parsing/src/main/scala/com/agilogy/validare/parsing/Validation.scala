package com.agilogy.validare.parsing

import com.agilogy.validare.parsing.Validated.{FailedPredicate, FailedTransformation, Parsed, Valid}
import com.agilogy.validare.validation.{Predicate, Validity}

sealed trait Validation[I, O] {

  def apply(input: I): Validated[O]

}

object Validation {
  def empty[T]: EmptyParser[T] = EmptyParser[T]()
}

final case class And[I, O](transformation: Parser[I, O], verification: Predicate[O]) extends Validation[I, O] {
  def apply(input: I): Validated[O] = {
    transformation(input) match {
      case f: Validated.Invalid[O] => f
      case Validated.Valid(value) => verification.apply(value) match {
        case Validity.Valid => Valid(value)
        case i: Validity.Invalid => FailedPredicate(i) //FailedPredicateAfter(i,transformation)
      }
    }
  }

  def andThen(other: Predicate[O]): And[I, O] = And[I, O](transformation, verification && other)

  //def andThen[OO<:O](verification:Predicate[OO]):And[I,OO] = And[I,OO](this,verification)

}


sealed trait Parser[I, O] extends Validation[I, O] {
  def andThen(verification: Predicate[O]): And[I, O] = And(this, verification)

  def andThen[O2](other: Parser[O, O2]): Parser[I, O2]
}

//object ParserTest{
//  val p:Parser[String,String] = ???
//  val p2: And[String, String] = p.andThen(Predicate.True)
//  val pred2:Predicate[Iterable[Char]] = ???
//  val p3 = p2.andThen(pred2)
//}


final case class AndThen[A, B, C](v1: AtomicParser[A, B], v2: Parser[B, C]) extends Parser[A, C] {

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def apply(input: A): Validated[C] = v1(input) match {
    case f@FailedTransformation(_) => FailedTransformation(this)
    case Valid(b) => v2(b) match {
      case f2@FailedTransformation(v) => f2 //FailedTransformationAfter(v2,v1)
      case Valid(c) => Valid(c)
      case _ => throw new RuntimeException("The compiler is not correctly understanding this will not ever happen")
    }
    case _ => throw new RuntimeException("The compiler is not correctly understanding this will not ever happen")
  }

  override def andThen[O2](other: Parser[C, O2]): Parser[A, O2] = AndThen(v1, v2.andThen(other))
}

trait AtomicParser[I, O] extends Parser[I, O] {

  val id: String = this.getClass.getSimpleName

  def andThen[O2](other: Parser[O, O2]): Parser[I, O2] = AndThen(this, other)

  override def apply(input: I): Parsed[O]
}

final case class EmptyParser[T]() extends AtomicParser[T, T] {
  override def apply(input: T): Valid[T] = Valid(input)
}

trait Transformation[T] extends AtomicParser[T, T] {

  def transform(value: T): T

  final def apply(input: T): Valid[T] = Valid(transform(input))
}

