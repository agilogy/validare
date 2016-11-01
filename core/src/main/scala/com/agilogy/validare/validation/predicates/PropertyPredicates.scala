package com.agilogy.validare.validation.predicates

import com.agilogy.validare.utils.HasLength
import com.agilogy.validare.validation.{PropertyPredicate, Predicate}

trait PropertyPredicates {

  case class Property[T,FT] private[predicates](name: String, f: T => FT){
    def apply(verification: Predicate[FT]): PropertyPredicate[T, FT] = PropertyPredicate(name, f, verification)
  }

  class AtBuilder[T] private[predicates] {
    def apply[FT](name: String, f: T => FT): Property[T, FT] = Property(name,f)
  }

  def at[T]: AtBuilder[T] = new AtBuilder[T]

  def length[T:HasLength]: Property[T, Int] = at[T]("length",implicitly[HasLength[T]].length)


}
