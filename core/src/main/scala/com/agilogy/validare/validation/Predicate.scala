package com.agilogy.validare.validation

import cats.Order
import cats.data.NonEmptyList

import com.agilogy.validare.validation.PredicatesBooleanAlgebra._
import com.agilogy.validare.validation.Validity.{ Invalid, Valid }

sealed trait Predicate[-I] extends Product with Serializable {

  def &&[II <: I](other: Predicate[II]): Predicate[II] = and(this, other)

  def ||[II <: I](other: Predicate[II]): Predicate[II] = or(this, other)

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

  case object True extends AtomicPredicate[Any] {
    override def satisfiedBy(value: Any): Boolean = true

    override def opposite: False.type = False
  }

  case object False extends AtomicPredicate[Any] {
    override def satisfiedBy(value: Any): Boolean = false

    override def opposite: True.type = True
  }

}

final case class OrPredicate[I](verifications: NonEmptyList[Predicate[I]]) extends Predicate[I] {

  require(verifications.size >= 2)

  override def apply(input: I): Validity[I] = verifications.map(_.apply(input)).reduceLeft(_ || _)

  override def opposite: AndPredicate[I] = AndPredicate(verifications.map(_.opposite))

  override def toString: String = verifications.map(_.toString).toList.mkString(" || ")
}

object OrPredicate {
  def apply[I](verifications: NonEmptyList[Predicate[I]]) = new OrPredicate[I](verifications.distinct)
}

final case class AndPredicate[I](verifications: NonEmptyList[Predicate[I]]) extends Predicate[I] {

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

final case class NotPredicate[I](v: AtomicPredicate[I]) extends Predicate[I] {
  override def apply(input: I): Validity[I] = v.apply(input) match {
    case Validity.Valid      => Validity.Invalid(this)
    case Validity.Invalid(_) => Validity.Valid
  }

  override def opposite: AtomicPredicate[I] = v

  override def toString: String = s"!$v"
}

trait CollectionPredicate[I] extends Predicate[I] {}

trait AtomicPredicate[-I] extends Predicate[I] {

  val id: String = this.getClass.getSimpleName

  def satisfiedBy(value: I): Boolean

  final override def apply(input: I): Validity[I] =
    if (satisfiedBy(input))
      Validity.Valid
    else
      Validity.Invalid(this)

}

/**
 * A predicate that validates a value after some conversion that may itself fail
 * e.g. isDefined converts an Option[T] into a T and then continues validating T,
 * but the conversion itself may fail (i.e. when the value is None)
 * @tparam A The type of the value that will be validated
 */
final case class ConversionAndSatisfies[A, B](c: Conversion[A, B], predicate: Predicate[B]) extends Predicate[A] {

  override def toString: String = s"$c.satisfies($predicate)"

  override def opposite: Predicate[A] = c match {
    case _ => !c || c.satisfies(predicate.opposite)
  }

  override def apply(input: A): Validity[A] = c.convert(input) match {
    case Right(i2) =>
      predicate.apply(i2) match {
        case Valid      => Valid
        case Invalid(p) => Invalid(c.satisfies(p))
      }
    case Left(Invalid(failedPredicate)) =>
      val res = Invalid(failedPredicate && this)
      println(
        s"Failed to validate simple $this when validating $c with $failedPredicate, so failing $this with $res"
      )
      res

  }

}

trait Conversion[A, B] extends AtomicPredicate[A] { self =>

  type Result = B

  override def satisfiedBy(value: A): Boolean = convert(value).isRight

  def convert(value: A): Either[Invalid[A], B] = transform(value).toRight(Invalid(this))

  def transform(value: A): Option[B]

  def apply(other: Predicate[B]): Predicate[A] = this.satisfies(other)

  def satisfies(predicate: Predicate[B]): Predicate[A] = ConversionAndSatisfies(this, predicate)

  override def opposite: Predicate[A] = NotPredicate(this)
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

final case class PropertySatisfies[A, B](p: Property[A, B], predicate: Predicate[B]) extends Predicate[A] {

  override def toString: String = s"$p.satisfies($predicate)"

  override def opposite: Predicate[A] = PropertySatisfies(p, predicate.opposite)

  override def apply(input: A): Validity[A] = predicate(p.getter(input)) match {
    case Invalid(failingPredicate) => Invalid(PropertySatisfies(p, failingPredicate))
    case Valid                     => Valid
  }
}
