package com.agilogy.validare.validation.predicates

import com.agilogy.validare.validation.AtomicPredicate

trait OrderingPredicates {

  case class gt[T: Ordering](minimum: T) extends AtomicPredicate[T] {
    override def satisfiedBy(value: T): Boolean = Ordering[T].gt(value, minimum)

    override def opposite: lteq[T] = lteq(minimum)
  }

  case class gteq[T: Ordering](minimum: T) extends AtomicPredicate[T] {
    override def satisfiedBy(value: T): Boolean = Ordering[T].gteq(value, minimum)

    override def opposite: lt[T] = lt(minimum)
  }

  case class lt[T: Ordering](maximum: T) extends AtomicPredicate[T] {
    override def satisfiedBy(value: T): Boolean = Ordering[T].lt(value, maximum)

    override def opposite: gteq[T] = gteq(maximum)
  }

  case class lteq[T: Ordering](maximum: T) extends AtomicPredicate[T] {
    override def satisfiedBy(value: T): Boolean = Ordering[T].lteq(value, maximum)

    override def opposite: gt[T] = gt(maximum)
  }

}
