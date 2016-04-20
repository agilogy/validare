package com.agilogy.validare

import com.agilogy.validare.Context.{ContextImpl, Part}


sealed trait Context {

  val parts:Seq[Part]

  def /(index: Int): Context = ContextImpl(parts.init :+ parts.last / index)

  def /(ctx: Context): Context

  def /(path: String): Context = this / Context(path)

  override def toString: String = parts.mkString("","/","")
}

object Context {

  object Self extends Context {
    override def /(ctx: Context): Context = ctx

    override val parts: Seq[Part] = Seq(Context.Part("."))
  }

  val __ = Self

  private final case class ContextImpl(parts: Seq[Part]) extends Context {

    def /(ctx: Context): Context = ctx match {
      case Self => this
      case ContextImpl(ctxParts) => ContextImpl(parts ++ ctxParts)
    }

  }

  final case class Part(name:String,index:Int *){
    def /(i: Int):Part = Part(name, index :+ i :_*)
    override def toString: String = name + (if(index.isEmpty) "" else index.mkString("[","][","]"))
  }

  def apply(part: String): Context = {
    if(part.isEmpty) Context.Self
    else ContextImpl(Seq(Part(part)))
  }

}
