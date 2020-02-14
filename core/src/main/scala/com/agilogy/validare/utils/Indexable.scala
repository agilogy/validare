package com.agilogy.validare.utils

import scala.language.higherKinds

import cats.Foldable
import cats.implicits._

trait Indexable[-S[_]] extends HasLength[S[_]] {
  def zipWithIndex[A](values: S[A]): Traversable[(A, Int)]
  def at[A](s: S[A], index: Int): Option[A]
}

trait IndexableLowLevel {

  implicit def foldableIndexable[F[_]: Foldable] = new Indexable[F] {
    override def zipWithIndex[A](values: F[A]): List[(A, Int)] =
      values
        .foldLeft((0, List.empty[(A, Int)])) {
          case ((idx, result), a) => (idx + 1, (a, idx) :: result)
        }
        ._2

    override def at[A](s: F[A], index: Int): Option[A] = Foldable[F].get(s)(index.toLong)

    override def length(value: F[_]): Int = Foldable[F].size(value).toInt
  }

}

object Indexable extends IndexableLowLevel {

  def apply[S[_]: Indexable]: Indexable[S] = implicitly[Indexable[S]]

  implicit val seqIndexable: Indexable[Seq] =
    new Indexable[Seq] {
      def zipWithIndex[A](values: Seq[A]): Seq[(A, Int)] =
        values.zipWithIndex

      override def at[A](s: Seq[A], index: Int): Option[A] = s.lift(index)

      override def length(value: Seq[_]): Int = value.size
    }

}
