package com.agilogy.validare.validation

import cats.Order
import cats.data.NonEmptyList
import cats.implicits._
import com.agilogy.validare.validation.PredicatesBooleanAlgebra._
import com.agilogy.validare.validation.Validity.{ Invalid, Valid }
import com.github.ghik.silencer.silent

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

}

final case class is[A, B](transformation: Transformation[A, B]) extends AtomicPredicate[A] {
  override def opposite: NotPredicate[A]      = NotPredicate(this)
  override def satisfiedBy(value: A): Boolean = transformation(value).isRight
}

/**
 * A predicate that validates a value after some transformation that may itself fail
 * e.g. isDefined transforms an Option[T] into a T and then continues validating T,
 * but the transformation itself may fail (i.e. when the value is None)
 * Note that PropertyPredicate is similar but the transformation is assumed to never fail
 * @tparam A The type of the value that will be validated
 */
sealed trait TransformedPredicate[A] extends Predicate[A] { self =>
  type Result

  def andThen[C](predicate: TransformedPredicate[Result] { type Result = C })
    : TransformedPredicate[A] { type Result = C }

  def parse(input: A): Either[Invalid[A], Result]

  override final def apply(input: A): Validity[A] = parse(input) match {
    case Left(Invalid(p)) => Invalid(p)
    case _                => Valid
  }

}

final case class SimpleTransformedPredicate[A, B](
  transformation: Transformation[A, B],
  verification: NonTransformedPredicate[B]
) extends TransformedPredicate[A] { self =>
  type Result = B

  override def andThen[C](predicate: TransformedPredicate[B] { type Result = C })
    : CompoundTransformedPredicate[A, B, C] =
    CompoundTransformedPredicate(this, predicate)

  override def toString: String = s"$transformation.satisfies($verification)"

  override def opposite: Predicate[A] = {
    val satisfaction = transformation.satisfies(verification.opposite)
    transformation.requirement.fold[Predicate[A]](satisfaction)(r => !r || satisfaction)
  }

  override def parse(input: A): Either[Invalid[A], Result] = transformation(input) match {
    case Right(i2) =>
      verification(i2) match {
        case Valid                                       => i2.asRight[Invalid[A]]
        case Invalid(p: TransformedPredicate[Result])    => Invalid(transformation.andThen(p)).asLeft[Result]
        case Invalid(p: NonTransformedPredicate[Result]) => Invalid(transformation.satisfies(p)).asLeft[Result]
      }
    case Left(Invalid(failedPredicate)) =>
      val res = Invalid(failedPredicate && this).asLeft[Result]
      println(
        s"Failed to validate simple $this when validating $transformation with $failedPredicate, so failing $this with $res"
      )
      res

  }

}

final case class CompoundTransformedPredicate[A, B, C](
  head: SimpleTransformedPredicate[A, B],
  verification: TransformedPredicate[B] { type Result = C }
) extends TransformedPredicate[A] {
  type Result = C

  override def andThen[D](predicate: TransformedPredicate[C] { type Result = D })
    : CompoundTransformedPredicate[A, B, D] =
    CompoundTransformedPredicate(head, verification.andThen(predicate))

  override def toString: String = s"$head.andThen($verification)"

  override def opposite: Predicate[A] = head.opposite || head.transformation.andThen(verification).opposite

  override def parse(input: A): Either[Invalid[A], Result] =
    head.transformation.apply(input) match {
      case Left(Invalid(p)) =>
        val res = Left(Invalid(p && this))
        println(s"Validating: $this\nFailed: ${head.transformation}\nFor value: $input\nWith:$p\nReturning: $res\n")
        res
      case Right(value) =>
        val headResult: Either[Invalid[A], B] = head.parse(input).leftMap {
          case Invalid(p) =>
            val res = Invalid(p)
            println(s"Validating: $this\nFailed: $head\nFor value: $input\nWith:$p\nReturning: $res\n")
            res
        }
        val tailResult: Either[Invalid[A], C] = verification.parse(value).leftMap {
          case Invalid(p: NonTransformedPredicate[B]) =>
            val res = Invalid(head.transformation.satisfies(p))
            println(s"Validating: $this\nFailed: $verification\nFor value: $value\nWith:$p\nReturning: $res\n")
            res
          case Invalid(p: TransformedPredicate[B]) =>
            val res = Invalid(head.transformation.andThen(p))
            println(s"Validating: $this\nFailed: $verification\nFor value: $value\nWith:$p\nReturning: $res\n")
            res
        }
        (headResult, tailResult) match {
          case (Left(Invalid(p1)), Left(Invalid(p2))) => Left(Invalid(p1 && p2))
          case (Left(Invalid(p1)), _)                 => Left(Invalid(p1))
          case (_, Left(Invalid(p2)))                 => Left(Invalid(p2))
          case (Right(_), Right(v))                   => Right(v)
        }
    }

}

object TransformedPredicate {
  @silent("unchecked")
  @silent("Recursion")
  def transformationAndThen[A, B, C](
    transformation: Transformation[A, B],
    predicate: TransformedPredicate[B] { type Result = C }
  ): TransformedPredicate[A] { type Result = C } = predicate match {
    case s: SimpleTransformedPredicate[B, C] =>
      (transformation andThen s.transformation).satisfies(s.verification)
    case s: CompoundTransformedPredicate[B, x, C] =>
      transformationAndThen(transformation, s.head).andThen(s.verification)
  }

  def transformationSatisfies[A, B](
    transformation: Transformation[A, B],
    satisfies: NonTransformedPredicate[B]
  ): TransformedPredicate[A] { type Result = B } = SimpleTransformedPredicate(transformation, satisfies)

}
