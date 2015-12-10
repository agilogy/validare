package com.agilogy.validare.incontext

import com.agilogy.validare.incontext.Context.{ContextImpl, Part}

trait Context {

  val parts:Seq[Part]

  def /(index: Int): Context = ContextImpl(parts.init :+ parts.last / index)

  def /(ctx: Context): Context

  def /(path: String): Context = this / Context(path)
}

object Context {

  object Self extends Context {
    override def /(ctx: Context): Context = ctx

    override def toString: String = "."

    override val parts: Seq[Part] = Seq(Part(""))
  }

  private case class ContextImpl(parts: Seq[Part]) extends Context {
    def /(ctx: Context): Context = ctx match {
      case Self => this
      case ContextImpl(ctxParts) => ContextImpl(parts ++ ctxParts)
    }

    override def toString: String = parts.mkString("","/","")
  }

  final case class Part(name:String,index:Int *){
    def /(i: Int):Part = Part(name, index :+ i :_*)
    override def toString: String = name + (if(index.isEmpty) "" else index.mkString("[","][","]"))
  }

  def apply(part: String): Context = {
    require(!part.isEmpty)
    ContextImpl(Seq(Part(part)))
  }

}
