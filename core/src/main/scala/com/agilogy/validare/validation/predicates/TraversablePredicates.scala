package com.agilogy.validare.validation.predicates

import com.agilogy.validare.validation.Validity.{Invalid, Valid}
import com.agilogy.validare.validation.{CollectionPredicate, PositionPredicate, Predicate, Validity}

import scala.language.existentials

trait TraversablePredicates {

  case class forAll[I, T <: Traversable[I]](elementValidation: Predicate[I]) extends CollectionPredicate[T] {

    @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
    override def apply(input: T): Validity = {
      val (res, _) = input.foldLeft[(Validity, Int)]((Validity.Valid, 0)) {
        case ((v, idx), e) =>
          val positionValidation = elementValidation(e) match {
            case Valid => Valid
            case Invalid(p) => Invalid(PositionPredicate(idx, p.asInstanceOf[Predicate[I]]))
          }
          (v && positionValidation, idx + 1)
      }
      res

    }

    override def opposite: Predicate[T] = exists(!elementValidation)
  }

  case class exists[I, T <: Traversable[I]](elementValidation: Predicate[I]) extends CollectionPredicate[T] {

    @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
    override def apply(input: T): Validity = {
      val (res, _) = input.foldLeft[(Validity, Int)]((Validity.Valid, 0)) {
        case ((v, idx), e) =>
          val positionValidation = elementValidation(e) match {
            case Valid => Valid
            case Invalid(p) => Invalid(PositionPredicate(idx, p.asInstanceOf[Predicate[I]]))
          }
          (v || positionValidation, idx + 1)
      }
      res
    }

    override def opposite: Predicate[T] = forAll(!elementValidation)
  }

}
