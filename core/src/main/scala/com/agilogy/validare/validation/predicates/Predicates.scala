package com.agilogy.validare.validation.predicates

trait Predicates extends FieldPredicates with HasLengthPredicates with OrderingPredicates with StringPredicates with TraversablePredicates

object Predicates extends Predicates
