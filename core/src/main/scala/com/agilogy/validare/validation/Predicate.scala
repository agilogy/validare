package com.agilogy.validare.validation

import com.agilogy.validare.validation.Predicate.False
import com.agilogy.validare.validation.PredicatesBooleanAlgebra._
import com.agilogy.validare.validation.Validity._
import cats.data.NonEmptyList

sealed trait Predicate[-I] extends Product with Serializable {

  def &&[II <: I](other: Predicate[II]): Predicate[II] = and(this, other)

  def ||[II <: I](other: Predicate[II]): Predicate[II] = or(this, other)

  def opposite: Predicate[I]

  def unary_! : Predicate[I] = opposite

  def implies[II <: I](other: Predicate[II]): Predicate[II] = !this || other

  def apply(input: I): Validity[I]

}

object Predicate {

  case object True extends AtomicPredicate[Any] {
    override def satisfiedBy(value: Any): Boolean = true

    override def opposite: Predicate[Any] = False
  }

  case object False extends AtomicPredicate[Any] {
    override def satisfiedBy(value: Any): Boolean = false

    override def opposite: Predicate[Any] = True
  }

}

final case class OrPredicate[I](verifications: NonEmptyList[Predicate[I]]) extends Predicate[I] {

  require(verifications.size >= 2)

  override def apply(input: I): Validity[I] =
    verifications.map {
      _.apply(input)
    }.reduceLeft(_ || _)

  override def opposite: Predicate[I] =
    verifications.tail.foldLeft[Predicate[I]](!verifications.head)((acc, v) => and(acc, !v))

  override def toString: String = verifications.map(_.toString).toList.mkString(" || ")
}

final case class AndPredicate[I](verifications: NonEmptyList[Predicate[I]]) extends Predicate[I] {

  require(verifications.size >= 2)

  override def apply(input: I): Validity[I] = verifications.foldLeft[Validity[I]](Validity.Valid) {
    case (acc, v) => acc && v.apply(input)
  }

  override def opposite: Predicate[I] =
    verifications.tail.foldLeft[Predicate[I]](!verifications.head)((acc, v) => or(acc, !v))

  override def toString: String = verifications.map(_.toString).toList.mkString(" && ")
}

final case class NotPredicate[I](v: AtomicPredicate[I]) extends Predicate[I] {
  override def apply(input: I): Validity[I] = v.apply(input) match {
    case Validity.Valid      => Validity.Invalid(this)
    case Validity.Invalid(_) => Validity.Valid
  }

  override def opposite: Predicate[I] = v

  override def toString: String = s"!$v"
}

//TODO: This transformation is a tautology (it is always true). Can this be represented in the type level somehow?
final case class Property[I, FT](name: String, getter: I => FT) extends Transformation[I, FT] {

  override val id: String = name

  override val f: I => Option[FT] = i => Some(getter(i))

  override def opposite: Predicate[I] = False

  override def toString: String = name

  override def equals(obj: scala.Any): Boolean = obj match {
    //TODO: Check property obj is of the same type
    case Property(`name`, _) => true
    case _                   => false
  }
}

trait Transformation[T, T2] extends AtomicPredicate[T] {
  val f: T => Option[T2]
  override def satisfiedBy(value: T): Boolean = f(value).isDefined

  def apply(other: Predicate[T2]): TransformedPredicate[T, T2]    = this.validate(other)
  def validate(other: Predicate[T2]): TransformedPredicate[T, T2] = TransformedPredicate(this, other)
}

/**
 * A predicate that validates a value after some transformation that may itself fail
 * e.g. isDefined transforms an Option[T] into a T and then continues validating T,
 * but the transformation itself may fail (i.e. when the value is None)
 * Note that PropertyPredicate is similar but the transformation is assumed to never fail
 * @tparam T The type of the value that will be validated
 * @tparam T2 The type of the result of the transformation, that will be further validated to validate the input value
 */
final case class TransformedPredicate[T, T2](transformation: Transformation[T, T2], verification: Predicate[T2])
    extends Predicate[T] {

  override def apply(input: T): Validity[T] = transformation.f(input) match {
    case Some(i2) =>
      verification(i2) match {
        case Valid      => Valid
        case Invalid(p) => Invalid(TransformedPredicate(transformation, p))
      }
    case None =>
      //Validity.Invalid(transformation)
      transformation(input) && Invalid(this)
  }

  override def opposite: Predicate[T] = !transformation || TransformedPredicate(transformation, !verification)

}

trait CollectionPredicate[I] extends Predicate[I] {}

trait AtomicPredicate[-I] extends Predicate[I] {

  val id: String = this.getClass.getSimpleName

  def satisfiedBy(value: I): Boolean

  //TODO: It was final. Make somehow difficult to override by accident (no idea how to do that, yet)
  override def apply(input: I): Validity[I] =
    if (satisfiedBy(input))
      Validity.Valid
    else
      Validity.Invalid(this)

}
