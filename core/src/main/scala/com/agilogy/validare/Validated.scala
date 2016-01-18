package com.agilogy.validare

sealed trait Validated[+R] extends Product with Serializable {

  val validity: Validity

  def and(v2: Validity): Validated[R] = {
    v2.fold({
      i2 =>
        this match {
          case (Valid(v)) => Invalid(v, i2)
          case (Invalid(v, i1)) => Invalid(v, i1 && i2)
          case (Illegal(i1)) => Illegal(i1 && i2)
        }
    }, this)
  }

  def fold[B](fError: Validity.Invalid => B, fOk: R => B): B = this match {
    case Valid(r) => fOk(r)
    case Invalid(_, errors) => fError(errors)
    case Illegal(errors) => fError(errors)
  }

  def isSuccess: Boolean = validity.isSuccess

  def isError: Boolean = !isSuccess

  def foreach(f: R => Unit): Unit = fold(_ => (), f)

  def orElse[R2 >: R](default: => Validated[R2]): Validated[R2] = fold(_ => default, Valid.apply)

  def getOrElse[R2 >: R](default: => R2): R2 = fold(_ => default, identity)

  def exists(p: R => Boolean): Boolean = fold(_ => false, p)

  def toEither: Either[Validity.Invalid, R] = fold(Left.apply, Right.apply)

  def toOption: Option[R] = fold(_ => None, Some.apply)

  def combine[R2, RR](that: Validated[R2])(f: (R, R2) => RR): Validated[RR] = Validare.map2(this, that)(f)

  def apply[R2](f: Validated[R => R2]): Validated[R2] = Validare.applyTo(this, f)

  def map[R2](f: R => R2): Validated[R2] = Validare.map(this, f)

  def flatMap[R2](f: R => Validated[R2]): Validated[R2] = Validare.flatMap(this, f)

  def at(path: String): Validated[R] = at(Context.Self / path)

  def at(ctx:Context):Validated[R]
}

case class Valid[+R](value: R) extends Validated[R] {
  val validity = Validity.Valid

  override def at(ctx: Context): Valid[R] = this
}

case class Invalid[+R](value: R, validity: Validity.Invalid) extends Validated[R] {
  override def at(ctx: Context): Invalid[R] = Invalid(value,validity.at(ctx))
}

case class Illegal(validity: Validity.Invalid) extends Validated[Nothing] {
  override def at(ctx: Context): Illegal = Illegal(validity.at(ctx))
}


