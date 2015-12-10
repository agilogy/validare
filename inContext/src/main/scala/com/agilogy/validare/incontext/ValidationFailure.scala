package com.agilogy.validare.incontext

case class ValidationFailure(message:String) {
  override def toString: String = message
}