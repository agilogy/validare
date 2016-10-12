package com.agilogy.validare

//import com.agilogy.path.Path
//import com.agilogy.validare.Validity.ValidationFailure
//
//trait Validare{
//
//  private def toValidity(vf:(Path,ValidationFailure)):Validity.Invalid = Validity.Invalid(Map(vf._1 -> Seq(vf._2)))
//  def valid[R](value: R): Valid[R] = Valid(value)
//  def invalid[R](value:R, v0:ValidationFailure, vs:ValidationFailure *): Invalid[R] = {
//    invalid(value, Path.Self -> v0, vs.map(Path.Self -> _) :_*)
//  }
//  def invalid[R](value:R, v0:(Path,ValidationFailure), vs:(Path,ValidationFailure) *): Invalid[R] = {
//    Invalid(value, vs.map(toValidity).fold(toValidity(v0))(_ && _))
//  }
//  def illegal(v0:ValidationFailure, vs:ValidationFailure *): Illegal = {
//    illegal(Path.Self -> v0, vs.map(Path.Self -> _) :_*)
//  }
//  def illegal(v0:(Path,ValidationFailure), vs:(Path,ValidationFailure) *): Illegal = {
//    Illegal(vs.map(toValidity).fold(toValidity(v0))(_ && _))
//  }
//  def itShould(msg:String, args:(String,String)*):ValidationFailure = Validity.itShould(msg,args :_*)
//
//  def map2[V1, V2, VM](validated1: Validated[V1], validated2: Validated[V2])(f: (V1, V2) => VM): Validated[VM] = {
//    (validated1, validated2) match {
//      case (Valid(v1), Valid(v2)) => Valid(f(v1, v2))
//      case (Valid(v1), Invalid(v2, errs2)) => Invalid(f(v1, v2), errs2)
//      case (Valid(v1), i2@Illegal(_)) => i2
//      case (Invalid(v1, errs1), Valid(v2)) => Invalid(f(v1, v2), errs1)
//      case (Invalid(v1, errs1), Invalid(v2, errs2)) => Invalid(f(v1, v2), errs1 && errs2)
//      case (Invalid(v1, errs1), Illegal(errs2)) => Illegal(errs1 && errs2)
//      case (i1@Illegal(_), Valid(_)) => i1
//      case (Illegal(errs1), Invalid(v2, errs2)) => Illegal(errs1 && errs2)
//      case (Illegal(errs1), Illegal(errs2)) => Illegal(errs1 && errs2)
//    }
//  }
//
//  def applyTo[R, R2](validated: Validated[R], vf: Validated[R => R2]): Validated[R2] = map2(validated, vf)((v, f) => f(v))
//
//  def apply[R, R2](vf: Validated[R => R2], validated: Validated[R]): Validated[R2] = map2(vf, validated)((f, v) => f(v))
//
//  def map[R, R2](validated: Validated[R], f: R => R2): Validated[R2] = apply(Valid(f), validated)
//
//  def flatMap[R, R2](validated: Validated[R], f: R => Validated[R2]): Validated[R2] = {
//    validated match {
//      case Valid(v) => f(v)
//      case Invalid(v, errs) => f(v) match {
//        case Valid(v2) => Invalid(v2, errs)
//        case Invalid(v2, errs2) => Invalid(v2, errs && errs2)
//        case Illegal(errs2) => Illegal(errs && errs2)
//      }
//      case i@Illegal(errs) => i
//    }
//  }
//}
//
//object Validare extends Validare
