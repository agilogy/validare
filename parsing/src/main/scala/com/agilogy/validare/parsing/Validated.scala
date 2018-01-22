package com.agilogy.validare.parsing

import com.agilogy.validare.validation.{Predicate, Validity}

sealed trait Validated[T] extends Product with Serializable

object Validated {
  def apply[T](value: T, validity: Validity[T]): Validated[T] = validity match {
    case Validity.Valid => Valid(value)
    case Validity.Invalid(p) => Invalid(Validity.Invalid(p))
  }

  sealed trait Parsed[T] extends Validated[T]

  final case class Valid[T](value: T) extends Validated[T] with Parsed[T]

  //  abstract case class Invalid[A, B, C] private
  //  (succeededTransformation: TransformingValidation[A, B], failedTransformation: TransformingValidation[B, C], validity: Validity[B])
  //  extends Validated[C]

  sealed trait Invalid[T] extends Validated[T]

  final case class FailedPredicate[A](validity: Validity.Invalid[A]) extends Invalid[A]

  //    case class FailedPredicateAfter[A,B](validity:Validity.Invalid[B], after:TransformingValidation[A,B]) extends Invalid[B]
  final case class FailedTransformation[A, B](parser: Parser[A, B]) extends Invalid[B] with Parsed[B]

  //    case class FailedTransformationAfter[A,B,C](transformation:TransformingValidation[B,C], after:TransformingValidation[A,B]) extends Invalid[C]

  object Invalid {

    def apply[A, B](transformation: AtomicParser[A, B]): FailedTransformation[A, B] = FailedTransformation(transformation)

    def apply[T](validity: Validity.Invalid[T]): FailedPredicate[T] = FailedPredicate(validity)

    def apply[T](p: Predicate[T]): FailedPredicate[T] = FailedPredicate(Validity.Invalid(p))

    //    def apply[T](validity: Validity.Invalid[T]): Invalid[T, T, T] = new Invalid[T, T, T](TransformingValidation.empty[T], TransformingValidation.empty[T], validity) {}

    //    def apply[A, B](succeededTransformation: TransformingValidation[A, B], validity: Validity.Invalid[B]) = new Invalid[A, B, B](succeededTransformation, TransformingValidation.empty[B], validity) {}

    //    def apply[A, B, C](succeededTransformation: TransformingValidation[A, B], failedTransformation: TransformingValidation[B, C]) = new Invalid[A, B, C](succeededTransformation, failedTransformation, Validity.Valid) {}
  }

}


//final case class Invalid[T](validity: Validity.Invalid[T]) extends Validated[T]


//sealed trait Validated[R] extends Product with Serializable {
//
//  val validity: Validity
//
//  def and(v2: Validity): Validated[R] = {
//    v2.fold({
//      i2 =>
//        this match {
//          case Valid(v) => Invalid(v, i2)
//          case Invalid(v, i1) => Invalid(v, i1 && i2)
//          case Illegal(i1) => Illegal(i1 && i2)
//        }
//    }, this)
//  }
//
//  def fold[B](fError: Validity.Invalid => B, fOk: R => B): B = this match {
//    case Valid(r) => fOk(r)
//    case Invalid(_, errors) => fError(errors)
//    case Illegal(errors) => fError(errors)
//  }
//
//  def isSuccess: Boolean = validity.isSuccess
//
//  def isError: Boolean = !isSuccess
//
//  def foreach(f: R => Unit): Unit = fold(_ => (), f)
//
//  def orElse[R2 >: R](default: => Validated[R2]): Validated[R2] = fold(_ => default, Valid.apply)
//
//  def getOrElse[R2 >: R](default: => R2): R2 = fold(_ => default, identity)
//
//  def exists(p: R => Boolean): Boolean = fold(_ => false, p)
//
//  def toEither: Either[Validity.Invalid, R] = fold[Either[Validity.Invalid,R]](Left.apply[Validity.Invalid,R], Right.apply[Validity.Invalid,R])
//
//  def toOption: Option[R] = fold(_ => None, Some.apply)
//
//  def combine[R2, RR](that: Validated[R2])(f: (R, R2) => RR): Validated[RR] = Validare.map2(this, that)(f)
//
//  def apply[R2](f: Validated[R => R2]): Validated[R2] = Validare.applyTo(this, f)
//
//  def map[R2](f: R => R2): Validated[R2] = Validare.map(this, f)
//
//  def flatMap[R2](f: R => Validated[R2]): Validated[R2] = Validare.flatMap(this, f)
//
//  def at(path: String): Validated[R] = at(Path.Self / path)
//
//  def at(ctx:Path):Validated[R]
//}
//
//final case class Valid[R](value: R) extends Validated[R] {
//  val validity = Validity.Valid
//
//  override def at(ctx: Path): Valid[R] = this
//}
//
//final case class Invalid[R](value: R, validity: Validity.Invalid) extends Validated[R] {
//  override def at(ctx: Path): Invalid[R] = Invalid(value,validity.at(ctx))
//}
//
//final case class Illegal(validity: Validity.Invalid) extends Validated[Nothing] {
//  override def at(ctx: Path): Illegal = Illegal(validity.at(ctx))
//}


