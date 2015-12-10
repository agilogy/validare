package com.agilogy.validare

import com.agilogy.classis.Applicative
import com.agilogy.validare.incontext.ValidationInContext

package object standalone extends ValidationInContext{

  module =>
  
  implicit val validatedApplicative:Applicative[Validated] = new Applicative[Validated] {
    
    override def apply[A, B](fab: Validated[(A) => B])(fa: Validated[A]): Validated[B] = module.apply(fab,fa)

    override def unit[A](a: => A): Validated[A] = Valid(a)
  }

}
