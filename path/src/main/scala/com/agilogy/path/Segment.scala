package com.agilogy.path


sealed trait Segment

object Segment{

  def apply(index:Int):IndexSegment = IndexSegment(index)
  def apply(field:String):Segment = {
    if(field.isEmpty) EmptySegment
    else FieldSegment(field)
  }

  case object EmptySegment extends Segment
  sealed trait NonEmptySegment extends Segment
  final case class IndexSegment(index:Int) extends NonEmptySegment
  final case class FieldSegment(field:String) extends NonEmptySegment{
    require(field.nonEmpty)
  }
}

