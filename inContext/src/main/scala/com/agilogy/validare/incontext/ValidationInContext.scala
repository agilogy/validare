package com.agilogy.validare.incontext

import com.agilogy.classis.Semigroup
import com.agilogy.validare.Validation
import java.util.List

trait ValidationInContext extends Validation[Map[Context, Seq[ValidationFailure]]] {

  implicit val failuresSemigroup:Semigroup[Failures] = new Semigroup[Failures] {
    override def append(x: Failures, y: Failures): Failures = {
      (x.keySet ++ y.keySet).map {
        ctx =>
          ctx -> (x.getOrElse(ctx, Seq.empty[ValidationFailure]) ++ y.getOrElse(ctx, Seq.empty[ValidationFailure]))
      }.toMap
    }
  }


  def invalid[R](value: R, errors: String*): Invalid[R] = Invalid(value, Map(Context.Self -> errors.map(ValidationFailure.apply)))

  def illegal[R](errors: String*): Illegal = Illegal(Map(Context.Self -> errors.map(ValidationFailure.apply)))

}
