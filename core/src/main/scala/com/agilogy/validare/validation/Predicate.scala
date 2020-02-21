package com.agilogy.validare.validation

import com.agilogy.validare.validation.PredicatesBooleanAlgebra._
import com.agilogy.validare.validation.Validity.{ Invalid, Valid }
import com.github.ghik.silencer.silent
import cats.Order
import cats.data.NonEmptyList
import cats.implicits._

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

trait NonMappedPredicate[-A] extends Predicate[A] {

  override def opposite: NonMappedPredicate[A]

//  def andThen(predicate: MappedPredicate[A]): MappedPredicate[A] { type Result = predicate.Result } =
//    PreverifiedMappedPredicate(this, predicate)
}

object NonMappedPredicate {
  implicit class NonMappedPredicateSyntax[A](p: NonMappedPredicate[A]) {
    def unary_! : NonMappedPredicate[A] = p.opposite
  }
}

final case class OrPredicate[I](verifications: NonEmptyList[Predicate[I]]) extends NonMappedPredicate[I] {

  require(verifications.size >= 2)

  override def apply(input: I): Validity[I] = verifications.map(_.apply(input)).reduceLeft(_ || _)

  override def opposite: AndPredicate[I] = AndPredicate(verifications.map(_.opposite))

  override def toString: String = verifications.map(_.toString).toList.mkString(" || ")
}

object OrPredicate {
  def apply[I](verifications: NonEmptyList[Predicate[I]]) = new OrPredicate[I](verifications.distinct)
}

final case class AndPredicate[I](verifications: NonEmptyList[Predicate[I]]) extends NonMappedPredicate[I] {

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

final case class NotPredicate[I](v: AtomicPredicate[I]) extends NonMappedPredicate[I] {
  override def apply(input: I): Validity[I] = v.apply(input) match {
    case Validity.Valid      => Validity.Invalid(this)
    case Validity.Invalid(_) => Validity.Valid
  }

  override def opposite: NonMappedPredicate[I] = v

  override def toString: String = s"!$v"
}

trait CollectionPredicate[I] extends NonMappedPredicate[I] {}

trait AtomicPredicate[-I] extends NonMappedPredicate[I] {

  val id: String = this.getClass.getSimpleName

  def satisfiedBy(value: I): Boolean

  //TODO: It was final. Make somehow difficult to override by accident (no idea how to do that, yet)
  override def apply(input: I): Validity[I] =
    if (satisfiedBy(input))
      Validity.Valid
    else
      Validity.Invalid(this)

}

final case class is[A, B](mapFunction: Conversion[A, B]) extends AtomicPredicate[A] {
  override def opposite: NotPredicate[A]      = NotPredicate(this)
  override def satisfiedBy(value: A): Boolean = mapFunction.parse(value).isRight
}

/**
 * A predicate that validates a value after some mapping that may itself fail
 * e.g. isDefined maps an Option[T] into a T and then continues validating T,
 * but the mapping itself may fail (i.e. when the value is None)
 * Note that PropertyPredicate is similar but the mapping is assumed to never fail
 * @tparam A The type of the value that will be validated
 */
sealed trait MappedPredicate[A] extends Predicate[A] { self =>
  type Result

  def andThen(predicate: MappedPredicate[Result]): MappedPredicate[A] { type Result = predicate.Result }

  def parse(input: A): Either[Invalid[A], Result]

  override final def apply(input: A): Validity[A] = parse(input) match {
    case Left(Invalid(p)) => Invalid(p)
    case _                => Valid
  }

}

trait SimpleMappedPredicate[A, B] extends MappedPredicate[A] { self =>
  type Result = B

  def transformation: Conversion[A, B]
  def verification: Option[NonMappedPredicate[B]]

}

final case class Satisfies[A, B](c: Conversion[A, B], predicate: NonMappedPredicate[B])
    extends SimpleMappedPredicate[A, B] {
  override def transformation: Conversion[A, B]            = c
  override def verification: Option[NonMappedPredicate[B]] = Some(predicate)

  override def andThen(
    predicate: MappedPredicate[Result]
  ): MappedPredicate[A] { type Result = predicate.Result } = CompoundMappedPredicate(this, predicate)

  override def toString: String = s"$c.satisfies($predicate)"

  override def opposite: Predicate[A] = c match {
    case _ => !c || c.satisfies(predicate.opposite)
  }

  override def parse(input: A): Either[Invalid[A], Result] = c.parse(input) match {
    case Right(i2) =>
      predicate.apply(i2) match {
        case Valid                               => i2.asRight[Invalid[A]]
        case Invalid(p: MappedPredicate[Result]) => Invalid(c.andThen(p)).asLeft[Result]
        case Invalid(p: NonMappedPredicate[Result]) =>
          Invalid(c.satisfies(p)).asLeft[Result]
      }
    case Left(Invalid(failedPredicate)) =>
      val res = Invalid(failedPredicate && this).asLeft[Result]
      println(
        s"Failed to validate simple $this when validating $c with $failedPredicate, so failing $this with $res"
      )
      res

  }

}

trait Conversion[A, B] extends AtomicPredicate[A] with SimpleMappedPredicate[A, B] { self =>

  override def transformation: Conversion[A, B]            = this
  override def verification: Option[NonMappedPredicate[B]] = None

  override def satisfiedBy(value: A): Boolean = parse(value).isRight

  override def parse(value: A): Either[Invalid[A], B] = transform(value).toRight(Invalid(this))

  def transform(value: A): Option[B]

  def apply(other: NonMappedPredicate[B]): MappedPredicate[A] = this.satisfies(other)

  @silent("unchecked")
  @silent("Recursion")
  override def andThen(predicate: MappedPredicate[B]): MappedPredicate[A] { type Result = predicate.Result } =
    predicate match {
      case s: SimpleMappedPredicate[B, predicate.Result] =>
        self.compose(s.transformation).satisfies(s.verification)
      case s: CompoundMappedPredicate[B, x, predicate.Result] =>
        transformation.andThen(s.head).andThen(s.verification)
    }

  def satisfies(predicate: Option[NonMappedPredicate[B]]): MappedPredicate[A] { type Result = B } =
    predicate.map(this.satisfies).getOrElse(this)

  def satisfies(predicate: NonMappedPredicate[B]): MappedPredicate[A] { type Result = B } =
    Satisfies(this, predicate)

  def compose[C](b: Conversion[B, C]): Conversion[A, C] = b match {
    case AndThen(t1, t2) => AndThen(AndThen(this, t1), t2)
    case _               => AndThen(this, b)
  }

  override def opposite: NonMappedPredicate[A] = NotPredicate(this)
}

final case class AndThen[A, B, C](t1: Conversion[A, B], t2: Conversion[B, C]) extends Conversion[A, C] {

  override def opposite: NonMappedPredicate[A] = t1.opposite || t1.satisfies(t2.opposite)

  override def parse(value: A): Either[Invalid[A], C] =
    t1.parse(value)
      .flatMap(
        b =>
          t2.parse(b).leftMap {
            case Invalid(p: MappedPredicate[B])    => Invalid(t1.andThen(p))
            case Invalid(p: NonMappedPredicate[B]) => Invalid(t1.satisfies(p))
          }
      )

  override def transform(value: A): Option[C] = parse(value).toOption

  override def toString: String = s"$t1.andThen($t2)"
}

final case class Property[I, FT](name: String, getter: I => FT) {

  def transform(value: I): Some[FT] = Some(getter(value))

  def apply(p: Predicate[FT]): PropertySatisfies[I, FT]     = satisfies(p)
  def satisfies(p: Predicate[FT]): PropertySatisfies[I, FT] = PropertySatisfies(this, p)

  override def toString: String = name

  // TODO: Fix this!
  override def equals(obj: scala.Any): Boolean = obj match {
    //TODO: Check property obj is of the same type
    case Property(`name`, _) => true
    case _                   => false
  }

}

final case class PropertySatisfies[A, B](p: Property[A, B], predicate: Predicate[B]) extends NonMappedPredicate[A] {

  override def toString: String = s"$p.satisfies($predicate)"

  override def opposite: NonMappedPredicate[A] = PropertySatisfies(p, predicate.opposite)

  override def apply(input: A): Validity[A] = predicate(p.getter(input)) match {
    case Invalid(failingPredicate) => Invalid(PropertySatisfies(p, failingPredicate))
    case Valid                     => Valid
  }
}

final case class CompoundMappedPredicate[A, B, C](
  head: SimpleMappedPredicate[A, B],
  verification: MappedPredicate[B] { type Result = C }
) extends MappedPredicate[A] {
  type Result = C

  override def andThen(predicate: MappedPredicate[C]): CompoundMappedPredicate[A, B, predicate.Result] =
    CompoundMappedPredicate(head, verification.andThen(predicate))

  override def toString: String = s"$head.andThen($verification)"

  override def opposite: Predicate[A] = head.opposite || head.transformation.andThen(verification).opposite

  override def parse(input: A): Either[Invalid[A], Result] =
    head.transformation.parse(input) match {
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
          case Invalid(p: NonMappedPredicate[B]) =>
            val res = Invalid(head.transformation.satisfies(p))
            println(s"Validating: $this\nFailed: $verification\nFor value: $value\nWith:$p\nReturning: $res\n")
            res
          case Invalid(p: MappedPredicate[B]) =>
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
