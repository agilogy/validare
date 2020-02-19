package com.agilogy.validare.validation

import com.agilogy.validare.validation.PredicatesBooleanAlgebra._
import com.agilogy.validare.validation.Validity.{ Invalid, Valid }
import cats.data.NonEmptyList
import cats.implicits._
import cats.Order

sealed trait Predicate[-I] extends Product with Serializable {

  def &&[II <: I](other: Predicate[II]): AndPredicate[II] = and(this, other)

  def ||[II <: I](other: Predicate[II]): OrPredicate[II] = or(this, other)

  def opposite: Predicate[I]

  def implies[II <: I](other: Predicate[II]): Predicate[II] = !this || other

  def apply(input: I): Validity[I]

}

object Predicate {

  implicit class PredicateSyntax[A](p: Predicate[A]) {
    def unary_! : Predicate[A] = p.opposite
  }

  implicit def predicateOrder[A]: Order[Predicate[A]] = new Order[Predicate[A]] {
    override def compare(x: Predicate[A], y: Predicate[A]): Int = if (x == y) 0 else x.toString.compareTo(y.toString)
  }

//  case object True extends AtomicPredicate[Any] {
//    override def satisfiedBy(value: Any): Boolean = true
//
//    override def opposite: NonTransformedPredicate[Any] = False
//  }
//
//  case object False extends AtomicPredicate[Any] {
//    override def satisfiedBy(value: Any): Boolean = false
//
//    override def opposite: NonTransformedPredicate[Any] = True
//  }

}

trait NonTransformedPredicate[-A] extends Predicate[A] {

  override def opposite: NonTransformedPredicate[A]
}

object NonTransformedPredicate {
  implicit class NonTransformedPredicateSyntax[A](p: NonTransformedPredicate[A]) {
    def unary_! : NonTransformedPredicate[A] = p.opposite
  }
}

final case class OrPredicate[I](verifications: NonEmptyList[Predicate[I]]) extends NonTransformedPredicate[I] {

  require(verifications.size >= 2)

  override def apply(input: I): Validity[I] = verifications.map(_.apply(input)).reduceLeft(_ || _)

  override def opposite: AndPredicate[I] = AndPredicate(verifications.map(_.opposite))

  override def toString: String = verifications.map(_.toString).toList.mkString(" || ")
}

object OrPredicate {
  def apply[I](verifications: NonEmptyList[Predicate[I]]) = new OrPredicate[I](verifications.distinct)
}

final case class AndPredicate[I](verifications: NonEmptyList[Predicate[I]]) extends NonTransformedPredicate[I] {

  require(verifications.size >= 2)

  override def apply(input: I): Validity[I] = verifications.foldLeft[Validity[I]](Validity.Valid) {
    case (acc, v) => acc && v.apply(input)
  }

  override def opposite: OrPredicate[I] = OrPredicate(verifications.map(_.opposite))

  override def toString: String = verifications.map(_.toString).toList.mkString(" && ")
}

object AndPredicate {
  def apply[I](verifications: NonEmptyList[Predicate[I]]) = new AndPredicate[I](verifications.distinct)
}

final case class NotPredicate[I](v: AtomicPredicate[I]) extends NonTransformedPredicate[I] {
  override def apply(input: I): Validity[I] = v.apply(input) match {
    case Validity.Valid      => Validity.Invalid(this)
    case Validity.Invalid(_) => Validity.Valid
  }

  override def opposite: NonTransformedPredicate[I] = v

  override def toString: String = s"!$v"
}

trait CollectionPredicate[I] extends NonTransformedPredicate[I] {}

trait AtomicPredicate[-I] extends NonTransformedPredicate[I] {

  val id: String = this.getClass.getSimpleName

  def satisfiedBy(value: I): Boolean

  //TODO: It was final. Make somehow difficult to override by accident (no idea how to do that, yet)
  override def apply(input: I): Validity[I] =
    if (satisfiedBy(input))
      Validity.Valid
    else
      Validity.Invalid(this)

//  override def toString: String = id
}

final case class is[A, B](transformation: Transformation[A, B]) extends AtomicPredicate[A] {
  override def opposite: NotPredicate[A]      = NotPredicate(this)
  override def satisfiedBy(value: A): Boolean = transformation(value).isRight
}

//sealed trait TransformedPredicate[A, B] extends Predicate[A] {
//  def andThen[C](predicate: SimpleTransformedPredicate[B, C]): ChainedTransformedPredicate[A, B, C] =
//    ChainedTransformedPredicate(this, predicate)
//  def parse(input: A): Either[Invalid[A], B]
//  def transformation: Transformation[A, B]
//  override final def apply(input: A): Validity[A] = parse(input) match {
//    case Left(Invalid(p)) => Invalid(p)
//    case _                => Valid
//  }
//}

/**
 * A predicate that validates a value after some transformation that may itself fail
 * e.g. isDefined transforms an Option[T] into a T and then continues validating T,
 * but the transformation itself may fail (i.e. when the value is None)
 * Note that PropertyPredicate is similar but the transformation is assumed to never fail
 * @tparam A The type of the value that will be validated
 */
trait TransformedPredicate[A] extends Predicate[A] {
  type Result
  def transformation: Transformation[A, Result]
  def verification: NonTransformedPredicate[Result]

  def andThen(predicate: TransformedPredicate[Result]): Predicate[A] =
    this && transformation.andThen(predicate.transformation).satisfies(predicate.verification)

  override final def apply(input: A): Validity[A] = parse(input) match {
    case Left(Invalid(p)) => Invalid(p)
    case _                => Valid
  }

  override def toString: String = s"$transformation.satisfies($verification)"

  override def opposite: Predicate[A] = {
    val satisfaction = transformation.satisfies(verification.opposite)
    transformation.requirement.fold[Predicate[A]](satisfaction)(r => !r || satisfaction)
  }

  def parse(input: A): Either[Invalid[A], Result] = transformation(input) match {
    case Right(i2) =>
      verification(i2) match {
        case Valid => i2.asRight[Invalid[A]]
        //        case Invalid(t:TransformedPredicate[A]) => Invalid(transformation.satisfies(s)).asLeft[B]
        case Invalid(p: TransformedPredicate[Result])    => Invalid(transformation.andThen(p)).asLeft[Result]
        case Invalid(p: NonTransformedPredicate[Result]) => Invalid(transformation.satisfies(p)).asLeft[Result]
      }
    case Left(Invalid(failedPredicate)) =>
      Invalid(failedPredicate && this).asLeft[Result]

  }

}

final case class SimpleTransformedPredicate[A, B](
  transformation: Transformation[A, B],
  verification: NonTransformedPredicate[B]
) extends TransformedPredicate[A] {
  type Result = B
}

//final case class ChainedTransformedPredicate[A, B, C](
//  init: TransformedPredicate[A, B],
//  last: SimpleTransformedPredicate[B, C]
//) extends TransformedPredicate[A, C] {
//
//  override def toString: String = s"$init.andThen($last)"
//
//  override def transformation: Transformation[A, C] = AndThen(init.transformation, last.transformation)
//
//  def parse(input: A): Either[Invalid[A], C] =
//    for {
//      res1 <- init.parse(input).leftMap {
//               case Invalid(predicate) => Invalid(predicate && transformation.andThen(last.verification))
//             }
//      res2 <- last.parse(res1).leftMap {
////               case Invalid(s @ SimpleTransformedPredicate(_, _)) =>
////                 Invalid(init.transformation.validate(s))
//               case Invalid(predicate) => Invalid(init.transformation.andThen(predicate))
//             }
//    } yield res2
//
//  override def opposite: Predicate[A] = !init || !transformation.andThen(last.verification)
//
//}
