package com.agilogy.validare

sealed trait Validity {

  def &&(v2: Validity): Validity

  def ||(v2: => Validity):Validity

  def fold[B](fError:Validity.Invalid => B, fOk: => B): B = this match {
    case Validity.Valid => fOk
    case i@Validity.Invalid(errors) => fError(i)
  }

  def isSuccess: Boolean = fold(_ => false, true)

  def isError: Boolean = !isSuccess

  def at(path: String): Validity = at(Context.Self / path)
  def at(ctx:Context):Validity

}

object Validity {

  final case class ValidationFailure(message: String, args: Map[String, String])

  case object Valid extends Validity {
    override def &&(v2: Validity): Validity = v2
    override def ||(v2: => Validity): Validity = this

    override def at(ctx: Context): Valid.type = this
  }

  final case class Invalid(issues: Map[Context, Seq[ValidationFailure]]) extends Validity {
    override def &&(v2: Validity): Invalid = v2 match {
      case Valid => this
      case Invalid(issues2) =>
        val resultIssues =
          (issues.keySet ++ issues2.keySet).map {
            ctx =>
              ctx -> (issues.getOrElse(ctx, Seq.empty[ValidationFailure]) ++ issues2.getOrElse(ctx, Seq.empty[ValidationFailure]))
          }.toMap
        Invalid(resultIssues)
    }

    override def ||(v2: => Validity): Validity = v2
    def apply(ctx:Context): Seq[ValidationFailure] = issues.getOrElse(ctx,Seq.empty[ValidationFailure])

    override def at(ctx: Context): Invalid = Invalid(issues.map{ case (k,v) => (ctx / k) -> v})
  }

  object Invalid{
    def apply(vf:ValidationFailure *): Invalid = new Invalid(Map(Context.Self -> vf))
  }


  def itShould(msg:String, args:(String,String)*):ValidationFailure = ValidationFailure(msg, args.toMap)
}
