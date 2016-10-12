package com.agilogy.validare.validation.predicates

import com.agilogy.validare.validation.{FieldPredicate, Predicate}

trait FieldPredicates {

  class AtBuilder[T] {
    def apply[FT](name: String, f: T => FT)(verification: Predicate[FT]): FieldPredicate[T, FT] = FieldPredicate(name, f, verification)
  }

  def at[T]: AtBuilder[T] = new AtBuilder[T]


}
