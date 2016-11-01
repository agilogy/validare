package com.agilogy.validare.validation.predicates

trait Predicates extends PropertyPredicates with HasLengthPredicates with OrderingPredicates with StringPredicates with FoldablePredicates

object Predicates extends Predicates
