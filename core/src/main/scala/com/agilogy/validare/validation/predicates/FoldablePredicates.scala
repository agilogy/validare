package com.agilogy.validare.validation.predicates

import com.agilogy.validare.utils.Indexable
import com.agilogy.validare.validation.Validity.{ Invalid, Valid }
import com.agilogy.validare.validation._

trait FoldablePredicates {
  self: TransformedPredicates =>

  case class forAll[E, S[_]: Indexable](elementValidation: Predicate[E]) extends CollectionPredicate[S[E]] {

    private val indexable = implicitly[Indexable[S]]

    override def apply(input: S[E]): Validity[S[E]] =
      indexable.zipWithIndex(input).foldLeft[Validity[S[E]]](Validity.Valid) {
        case (v, (e, idx)) =>
          val positionValidation = elementValidation(e) match {
            case Valid                                  => Valid
            case Invalid(p: TransformedPredicate[E])    => Invalid(atPos[S, E](idx).andThen(p))
            case Invalid(p: NonTransformedPredicate[E]) => Invalid(atPos[S, E](idx).satisfies(p))
          }
          v && positionValidation
      }

    override def opposite: NonTransformedPredicate[S[E]] = exists(!elementValidation)
  }

  case class exists[I, T[_]: Indexable](elementValidation: Predicate[I]) extends CollectionPredicate[T[I]] {

    private val indexable = implicitly[Indexable[T]]

    override def apply(input: T[I]): Validity[T[I]] = {
      val res = indexable
        .zipWithIndex(input)
        .map { case (e, pos) => validateElement(e, pos) }
        .reduceLeftOption(_ || _)
      res.getOrElse(Invalid(this))
    }

    private def validateElement(e: I, idx: Int): Validity[T[I]] = elementValidation(e) match {
      case Valid                                  => Valid
      case Invalid(p: TransformedPredicate[I])    => Invalid(atPos[T, I](idx).andThen(p))
      case Invalid(p: NonTransformedPredicate[I]) => Invalid(atPos[T, I](idx).satisfies(p))
    }

    override def opposite: NonTransformedPredicate[T[I]] = forAll(!elementValidation)
  }

//  def atPosition[S[_] : Indexable,E](i:Int,p:Predicate[E]):Predicate[S[E]] = PositionPredicate(i,p)

}
