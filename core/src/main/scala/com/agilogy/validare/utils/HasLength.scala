package com.agilogy.validare.utils

trait HasLength[T] {
  def length(value: T): Int
}

object HasLength {
  def apply[T: HasLength]: HasLength[T] = implicitly[HasLength[T]]

  implicit def traversableHasLength[T <: Traversable[_]]: HasLength[T] = new HasLength[T] {
    override def length(value: T): Int = value.size
  }

  implicit val stringHasLength: HasLength[String] = new HasLength[String] {
    override def length(value: String): Int = value.length
  }

}

