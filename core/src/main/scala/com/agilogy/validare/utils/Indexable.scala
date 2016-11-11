package com.agilogy.validare.utils

import scala.language.higherKinds

trait Indexable[-S[_]] extends HasLength[S[_]]{
  def zipWithIndex[A](values: S[A]): Traversable[(A, Int)]
  def at[A](s:S[A], index:Int):Option[A]
}

object Indexable {
  def apply[S[_] : Indexable]: Indexable[S] = implicitly[Indexable[S]]

  implicit val seqIndexable: Indexable[Seq] =
    new Indexable[Seq] {
      def zipWithIndex[A](values: Seq[A]): Seq[(A, Int)] =
        values.zipWithIndex

      override def at[A](s: Seq[A], index: Int): Option[A] = s.lift(index)

      override def length(value: Seq[_]): Int = value.size
    }
}