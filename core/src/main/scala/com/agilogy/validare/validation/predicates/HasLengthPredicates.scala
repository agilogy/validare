package com.agilogy.validare.validation.predicates

import com.agilogy.validare.utils.HasLength
import com.agilogy.validare.validation.AtomicPredicate

trait HasLengthPredicates {

  case class nonEmpty[T: HasLength]() extends AtomicPredicate[T] {
    override def satisfiedBy(value: T): Boolean = HasLength[T].length(value) > 0

    override def opposite: isEmpty[T] = isEmpty[T]
  }

  def nonEmptyT[T <: Traversable[_]]: nonEmpty[T] = nonEmpty[T]

  def nonEmptyS: nonEmpty[String] = nonEmpty[String]

  case class isEmpty[T: HasLength]() extends AtomicPredicate[T] {
    override def satisfiedBy(value: T): Boolean = HasLength[T].length(value) == 0

    override def opposite: nonEmpty[T] = nonEmpty[T]
  }

  def isEmptyT[T <: Traversable[_]]: isEmpty[T] = isEmpty[T]

  def isEmptyS: isEmpty[String] = isEmpty[String]

}
