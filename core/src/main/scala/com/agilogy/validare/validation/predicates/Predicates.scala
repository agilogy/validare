package com.agilogy.validare.validation.predicates

trait Predicates extends FieldPredicates with HasLengthPredicates with OrderingPredicates with StringPredicates with FoldablePredicates

object Predicates extends Predicates
