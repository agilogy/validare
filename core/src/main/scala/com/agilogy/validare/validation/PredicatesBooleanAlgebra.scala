package com.agilogy.validare.validation

import com.agilogy.validare.validation.Predicate.{False, True}

trait PredicatesBooleanAlgebra {

  def or[I](v1: Predicate[I], v2: Predicate[I]): Predicate[I] = (v1, v2) match {
    case (True, _) => True
    case (_, True) => True
    case (False, o) => o
    case (o, False) => o
    case (o1: OrPredicate[I], o2: OrPredicate[I]) => OrPredicate(o1.verifications ++ o2.verifications)
    case (o1: OrPredicate[I], o2) => OrPredicate(o1.verifications :+ o2)
    case (o1, o2: OrPredicate[I]) => OrPredicate(o1 +: o2.verifications)
    case (o1, o2) => OrPredicate(Seq(o1, o2))
  }

  def and[I](v1: Predicate[I], v2: Predicate[I]): Predicate[I] = (v1, v2) match {
    case (False, _) => False
    case (_, False) => False
    case (True, o) => o
    case (o, True) => o
    case (o1: AndPredicate[I], o2: AndPredicate[I]) => AndPredicate(o1.verifications ++ o2.verifications)
    case (o1: AndPredicate[I], o2) => AndPredicate(o1.verifications :+ o2)
    case (o1, o2: AndPredicate[I]) => AndPredicate(o1 +: o2.verifications)
    case (o1, o2) => AndPredicate(Seq(o1, o2))
  }

}

object PredicatesBooleanAlgebra extends PredicatesBooleanAlgebra