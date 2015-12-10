package com.agilogy.validare

import com.agilogy.classis.Semigroup

trait Validation[F] {

  module =>

  type Failures = F

  def valid[R](value: R): Valid[R] = Valid(value)

  sealed trait Validated[+R] extends Product with Serializable {

    def fold[B](fError: Failures => B, fOk: R => B): B = this match {
      case Valid(r) => fOk(r)
      case Invalid(_, errors) => fError(errors)
      case Illegal(errors) => fError(errors)
    }

    def isSuccess: Boolean = fold(_ => false, _ => true)

    def isError: Boolean = !isSuccess

    def foreach(f: R => Unit): Unit = fold(_ => (), f)

    def orElse[R2 >: R](default: => Validated[R2]): Validated[R2] = fold(_ => default, Valid.apply)

    def getOrElse[R2 >: R](default: => R2): R2 = fold(_ => default, identity)

    def exists(p: R => Boolean): Boolean = fold(_ => false, p)

    def toEither: Either[Failures, R] = fold(Left.apply, Right.apply)

    def toOption: Option[R] = fold(_ => None, Some.apply)

    def combine[R2, RR](that: Validated[R2])(f: (R, R2) => RR)(implicit ev: Semigroup[Failures]): Validated[RR] = module.map2(this, that)(f)

    def apply[R2](f: Validated[R => R2])(implicit ev: Semigroup[Failures]): Validated[R2] = module.applyTo(this, f)

    def map[R2](f: R => R2)(implicit ev: Semigroup[Failures]): Validated[R2] = module.map(this, f)

    def flatMap[R2](f: R => Validated[R2])(implicit ev: Semigroup[Failures]): Validated[R2] = module.flatMap(this, f)

  }

  case class Valid[+R](value: R) extends Validated[R]

  case class Invalid[+R](value: R, errors: Failures) extends Validated[R]

  case class Illegal(errors: Failures) extends Validated[Nothing]

  def map2[V1, V2, VM](validated1: Validated[V1], validated2: Validated[V2])(f: (V1, V2) => VM)(implicit ev: Semigroup[Failures]): Validated[VM] = {
    (validated1, validated2) match {
      case (Valid(v1), Valid(v2)) => Valid(f(v1, v2))
      case (Valid(v1), Invalid(v2, errs2)) => Invalid(f(v1, v2), errs2)
      case (Valid(v1), i2@Illegal(errs2)) => i2
      case (Invalid(v1, errs1), Valid(v2)) => Invalid(f(v1, v2), errs1)
      case (Invalid(v1, errs1), Invalid(v2, errs2)) => Invalid(f(v1, v2), ev.append(errs1, errs2))
      case (Invalid(v1, errs1), Illegal(errs2)) => Illegal(ev.append(errs1, errs2))
      case (i1@Illegal(errs1), Valid(v2)) => i1
      case (Illegal(errs1), Invalid(v2, errs2)) => Illegal(ev.append(errs1, errs2))
      case (Illegal(errs1), Illegal(errs2)) => Illegal(ev.append(errs1, errs2))
    }
  }

  def applyTo[R, R2](validated: Validated[R], vf: Validated[R => R2])(implicit ev: Semigroup[Failures]): Validated[R2] = map2(validated, vf)((v, f) => f(v))

  def apply[R, R2](vf: Validated[R => R2], validated: Validated[R])(implicit ev: Semigroup[Failures]): Validated[R2] = map2(vf, validated)((f, v) => f(v))

  def map[R, R2](validated: Validated[R], f: R => R2)(implicit ev: Semigroup[Failures]): Validated[R2] = apply(Valid(f), validated)

  def flatMap[R, R2](validated: Validated[R], f: R => Validated[R2])(implicit ev: Semigroup[Failures]): Validated[R2] = {
    validated match {
      case Valid(v) => f(v)
      case Invalid(v, errs) => f(v) match {
        case Valid(v2) => Invalid(v2, errs)
        case Invalid(v2, errs2) => Invalid(v2, ev.append(errs,errs2))
        case Illegal(errs2) => Illegal(ev.append(errs,errs2))
      }
      case i@Illegal(errs) => i
    }
  }


}
