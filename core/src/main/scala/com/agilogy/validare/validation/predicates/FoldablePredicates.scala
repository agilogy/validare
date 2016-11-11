package com.agilogy.validare.validation.predicates

import com.agilogy.validare.utils.Indexable
import com.agilogy.validare.validation.Predicate.False
import com.agilogy.validare.validation.Validity.{Invalid, Valid}
import com.agilogy.validare.validation._

import scala.language.{existentials, higherKinds}

trait FoldablePredicates {
  self:TransformedPredicates =>

  case class forAll[E, S[_]: Indexable](elementValidation: Predicate[E]) extends CollectionPredicate[S[E]] {

    val indexable = implicitly[Indexable[S]]

    @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
    override def apply(input: S[E]): Validity = {
      indexable.zipWithIndex(input).foldLeft[Validity](Validity.Valid) {
        case (v, (e, idx)) =>
          val positionValidation = elementValidation(e) match {
            case Valid => Valid
            case Invalid(p) => Invalid(atPosition[S,E](idx, p.asInstanceOf[Predicate[E]]))
          }
          v && positionValidation
      }
    }

    override def opposite: Predicate[S[E]] = exists(!elementValidation)
  }

  case class exists[I, T[_] : Indexable](elementValidation: Predicate[I]) extends CollectionPredicate[T[I]] {

    val indexable = implicitly[Indexable[T]]

    override def apply(input: T[I]): Validity = {
      val res = indexable.zipWithIndex(input).foldLeft[Validity](Invalid(False)) {
        case (v, (e,idx)) =>
          v || validateElement(e, idx)
      }
      if (res == Invalid(False)) {
        Invalid(this)
      } else {
        res
      }
    }

    @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
    private def validateElement(e: I, idx: Int): Validity = elementValidation(e) match {
      case Valid => Valid
      case Invalid(p) => Invalid(atPosition[T,I](idx, p.asInstanceOf[Predicate[I]]))
    }

    override def opposite: Predicate[T[I]] = forAll(!elementValidation)
  }

//  def atPosition[S[_] : Indexable,E](i:Int,p:Predicate[E]):Predicate[S[E]] = PositionPredicate(i,p)

}
