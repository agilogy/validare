package com.agilogy.validare.validation.predicates

import scala.language.higherKinds
import scala.util.Try

import com.agilogy.validare.utils.{ HasLength, Indexable }
import com.agilogy.validare.validation._

trait TransformedPredicates {
  self: OrderingPredicates =>

  class AtBuilder[T] private[predicates] {
    def apply[FT](name: String, f: T => FT): Property[T, FT] = Property(name, f)
  }

  def at[T]: AtBuilder[T] = new AtBuilder[T]

  case class AtPosition[S[_]: Indexable, E](index: Int) extends Conversion[S[E], E] {
    override def transform(value: S[E]): Option[E] = implicitly[Indexable[S]].at(value, index)
  }

  def atPos[S[_]: Indexable, E](index: Int): AtPosition[S, E] = AtPosition[S, E](index)

  def length[T: HasLength]: Property[T, Int] = at[T]("length", HasLength[T].length)

  trait Product0 extends Product {

    @SuppressWarnings(Array("org.wartremover.warts.Throw"))
    override def productElement(n: Int): Any = throw new IndexOutOfBoundsException(n.toString)

    override def productArity: Int = 0

    override def canEqual(that: Any): Boolean = this.getClass == that.getClass
  }

  class defined[T] extends Conversion[Option[T], T] with Product0 {

    override def transform(value: Option[T]): Option[T] = value

    override def toString: String = "defined"

    override def equals(obj: scala.Any): Boolean = obj match {
      case o: defined[T] if o.canEqual(this) => true
      case _                                 => false
    }

  }

  object defined {
    def apply[T]: defined[T] = new defined[T]
  }

  def ifDefined[T](p: Predicate[T]): Predicate[Option[T]] = !defined[T] || defined[T].satisfies(p)

  case object intString extends Conversion[String, Int] with Product0 {

    override def transform(value: String): Option[Int] = Try(value.toInt).toOption
  }

}
