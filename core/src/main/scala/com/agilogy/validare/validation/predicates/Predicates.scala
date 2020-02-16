package com.agilogy.validare.validation.predicates

trait Predicates
    extends TransformedPredicates
    with HasLengthPredicates
    with OrderingPredicates
    with StringPredicates
    with FoldablePredicates

object Predicates extends Predicates
