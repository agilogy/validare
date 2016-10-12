package com.agilogy.validare.parsing.parsers

import com.agilogy.validare.validation.predicates.Predicates

trait StringParsers extends SimpleStringParsers with StringTransformations with Predicates

object StringParsers extends StringParsers