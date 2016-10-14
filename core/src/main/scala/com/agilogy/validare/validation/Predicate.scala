package com.agilogy.validare.validation

import Validity._
import PredicatesBooleanAlgebra._
import com.agilogy.validare.utils.Indexable
import com.agilogy.validare.validation.Predicate.False

import scala.language.higherKinds

sealed trait Predicate[-I] extends Product with Serializable {

  def &&[II <: I](other: Predicate[II]): Predicate[II] = and(this, other)
//  def and2(other: Predicate[I]): Predicate[I] = and(this, other)

  def ||[II <: I](other: Predicate[II]): Predicate[II] = or(this, other)

  def opposite: Predicate[I]

  def unary_! : Predicate[I] = opposite

  def implies[II <: I](other: Predicate[II]): Predicate[II] = !this || other

  def apply(input: I): Validity

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

final case class OrPredicate[I](verifications: Seq[Predicate[I]]) extends Predicate[I] {

  require(verifications.size >= 2)

  override def apply(input: I): Validity = {
    verifications.map {
      _.apply(input)
    }.reduceLeft(_ || _)
  }

  override def opposite: Predicate[I] = verifications.tail.foldLeft[Predicate[I]](!verifications.head)((acc, v) => and(acc, !v))
}

final case class AndPredicate[I](verifications: Seq[Predicate[I]]) extends Predicate[I] {

  require(verifications.size >= 2)

  override def apply(input: I): Validity = verifications.foldLeft[Validity](Validity.Valid) {
    case (acc, v) => acc && v.apply(input)
  }

  override def opposite: Predicate[I] = verifications.tail.foldLeft[Predicate[I]](!verifications.head)((acc, v) => or(acc, !v))
}

final case class NotPredicate[I](v: AtomicPredicate[I]) extends Predicate[I] {
  override def apply(input: I): Validity = v.apply(input) match {
    case Validity.Valid => Validity.Invalid(this)
    case Validity.Invalid(_) => Validity.Valid
  }

  override def opposite: Predicate[I] = v
}

@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
final case class FieldPredicate[I, FT](name: String, field: I => FT, verification: Predicate[FT]) extends Predicate[I] {
  override def apply(input: I): Validity = verification.apply(field(input)) match {
    case Validity.Valid => Validity.Valid
    case i: Validity.Invalid => Validity.Invalid(FieldPredicate[I, FT](name, field, i.failing.asInstanceOf[Predicate[FT]]))
  }

  override def opposite: Predicate[I] = this.copy(verification = !verification)
}

final case class PositionPredicate[S[_]:Indexable,E](position:Int, verification:Predicate[E]) extends Predicate[S[E]] {

  val indexable = Indexable[S]

  //TODO: Fix Invalid(False)
  override def apply(input: S[E]): Validity = indexable.at(input,position).map(verification.apply).getOrElse(Invalid(False))

  override def opposite: Predicate[S[E]] = this.copy(verification = !verification)
}

trait CollectionPredicate[I] extends Predicate[I] {
}

trait AtomicPredicate[I] extends Predicate[I] {

  val id: String = this.getClass.getSimpleName

  def satisfiedBy(value: I): Boolean

  override final def apply(input: I): Validity = {
    if (satisfiedBy(input))
      Validity.Valid
    else
      Validity.Invalid(this)
  }

}

