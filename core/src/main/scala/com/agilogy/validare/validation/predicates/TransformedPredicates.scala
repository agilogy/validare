package com.agilogy.validare.validation.predicates

import com.agilogy.validare.utils.{ HasLength, Indexable }
import com.agilogy.validare.validation.Validity.{ Invalid, Valid }
import com.agilogy.validare.validation._

import scala.language.higherKinds

trait TransformedPredicates {
  self: OrderingPredicates =>

  class AtBuilder[T] private[predicates] {
    def apply[FT](name: String, f: T => FT): Property[T, FT] = Property(name, f)
  }

  def at[T]: AtBuilder[T] = new AtBuilder[T]

  case class AtPosition[S[_]: Indexable, E](index: Int) extends Transformation[S[E], E] {
    override val f: S[E] => Option[E] = implicitly[Indexable[S]].at(_, index)

    override def opposite: Predicate[S[E]] = length[S[E]].validate(lt(index))

    override final def apply(input: S[E]): Validity[S[E]] = super.apply(input) match {
      case Valid      => Valid
      case Invalid(_) => Invalid(length[S[_]].validate(gteq(index)))
    }

  }

  //TODO: Remove this one or atPosition
  def atPos[S[_]: Indexable, E](index: Int): AtPosition[S, E] = AtPosition[S, E](index)

  //TODO: Remove this one or atPos
  def atPosition[S[_]: Indexable, E](i: Int, p: Predicate[E]): Predicate[S[E]] = atPos[S, E](i).validate(p)

  def length[T: HasLength]: Property[T, Int] = at[T]("length", implicitly[HasLength[T]].length)

  trait Product0 extends Product {

    @SuppressWarnings(Array("org.wartremover.warts.Throw"))
    override def productElement(n: Int): Any = throw new IndexOutOfBoundsException(n.toString)

    override def productArity: Int = 0

    override def canEqual(that: Any): Boolean = this.getClass == that.getClass
  }

  class notDefined[T] extends Transformation[Option[T], None.type] with Product0 {

    override val f: Option[T] => Option[None.type] = o => if (o.isDefined) None else Some(None)

    override def opposite: Predicate[Option[T]] = isDefined[T]

    override def equals(obj: scala.Any): Boolean = obj match {
      case o: notDefined[T] if o.canEqual(this) => true
      case _                                    => false
    }

    override def toString: String = "notDefined"
  }

  object notDefined {
    def apply[T]: notDefined[T] = new notDefined[T]
  }

  class isDefined[T] extends Transformation[Option[T], T] with Product0 {

    override val f: Option[T] => Option[T] = identity

    override def opposite: Predicate[Option[T]] = notDefined[T]

    override def equals(obj: scala.Any): Boolean = obj match {
      case o: isDefined[T] if o.canEqual(this) => true
      case _                                   => false
    }

    override def toString: String = "isDefined"
  }

  object isDefined {
    def apply[T]: isDefined[T] = new isDefined[T]
  }

  def ifDefined[T](p: Predicate[T]): Predicate[Option[T]] = notDefined[T] || isDefined[T](p)

}
