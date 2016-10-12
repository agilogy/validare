package com.agilogy.path

import com.agilogy.path.Segment.{EmptySegment, FieldSegment, IndexSegment, NonEmptySegment}

final case class Path private (segments:Seq[NonEmptySegment]) {

  def /(segment: Segment): Path = segment match {
    case EmptySegment => this
    case s:NonEmptySegment => Path(segments :+ s)
  }

  def /(index: Int): Path = this / Segment(index)

  def /(field: String): Path = this / Segment(field)

  def ::(segment: Segment):Path = segment match {
    case EmptySegment => this
    case s:NonEmptySegment => new Path(s +: segments)
  }

  def ::(field: String):Path = ::(Segment(field))

  def ::(index:Int):Path = ::(Segment(index))



  override lazy val toString: String = segments.foldLeft(""){
    case ("",FieldSegment(f)) => f
    case (s,FieldSegment(f)) => s"$s/$f"
    case (s,IndexSegment(i)) => s"$s[$i]"
  }
}

object Path {

  def apply(field: String): Path = Path.Self / field

  def apply(index:Int):Path = Path.Self / index

  val Self = Path(Seq.empty)

  val __ = Self
}
