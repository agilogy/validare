package examples

import java.text.{ParsePosition, SimpleDateFormat}
import java.util
import java.util.{GregorianCalendar, Date}

import com.agilogy.validare._
import com.agilogy.validare.Validity.ValidationFailure
import org.scalatest.FunSpec

import com.agilogy.validare.standalone._

import scala.language.implicitConversions

object Options extends Enumeration{
  val string = Value
  val number = Value
  val date = Value
}

class ExampleTest extends FunSpec {

  module =>

  def parseEnum[T<:Enumeration](e:T): String => Validated[T#Value] = {
    s =>
      try {
        Valid(e.withName(s))
      }catch {
        case n:NoSuchElementException => illegal(itShould(s"be a valid value", "validValues" -> e.values))
      }
  }

  def foreach[I,O](f:I=>Validated[O]):Seq[I] => Validated[Seq[O]] = {
    is =>
      is.zipWithIndex.foldLeft[Validated[Seq[O]]](Valid(Seq.empty)){
        case (acc, (i,idx)) => acc.combine(f(i).at(Context.Self / idx))(_ :+ _)
      }
  }

  def nonEmpty[T <: Iterable[_]]: T => Validated[T] = {
    is => if(is.isEmpty) invalid(is,itShould("be non empty")) else Valid(is)
  }

  def andThen[T1, T2, T3](f1: T1 => Validated[T2], f2: T2 => Validated[T3]): T1 => Validated[T3] = {
    i =>
      f1(i).flatMap(i2 => f2(i2))
  }

  def and[I](f1:I => Validated[Unit], f2:I => Validated[Unit]): I => Validated[Unit] = {
    i =>
      f1(i).flatMap(_ => f2(i))
  }

  def flatMap2[I1, O1, I2, O2](f1: I1 => Validated[O1], f2: O1 => I2 => Validated[O2]): (I1, I2) => Validated[(O1, O2)] = {
    (i1, i2) =>
      val vo1 = f1(i1)
      vo1.flatMap{
        o1 =>
          f2(o1)(i2).map(o2 => (o1,o2))
      }
  }

  def check[I](f: I => Validity): I => Validated[I] = i => f(i).fold(inv => Invalid(i,inv),Valid(i))

  implicit class Reader[I, O](f: I => Validated[O]) {
    def &&(f2: O => Validity):I => Validated[O] = i => f(i).flatMap(check(f2))
    def combine[O2](f2: O => Validated[O2]): I => Validated[O2] = module.andThen(f, f2)
    def flatMap[I2,O2](f2: O => I2 => Validated[O2]): (I, I2) => Validated[(O, O2)] = flatMap2(f,f2)
  }

  def greaterOrEqualThan(min: Int)(v: Int): Validity = {
    if (v >= min) Validity.Valid
    else Validity.Invalid(itShould("be greater or equal than", "value" -> v))
  }

  val parseInt: String => Validated[Int] = {
    s =>
      try {
        Valid(s.toInt)
      } catch {
        case nfe: NumberFormatException => illegal(itShould("be an int"))
      }
  }

  implicit def someReader[I,O](f:I => Validated[O]):Some[I] => Validated[Some[O]] = s => f(s.get).map(Some.apply)

  def parseDate(pattern: String)(s: String): Validated[Date] = {
    val pos = new ParsePosition(0)
    val df = new SimpleDateFormat(pattern)
    val res = df.parse(s, pos)
    if (pos.getIndex < s.length) illegal(itShould("be a date", "pattern" -> pattern))
    else Valid(res)
  }

  val pattern = "dd/MM/yyyy HH:mm:ss"
  val parseSimpleDate = parseDate(pattern) _

  type ValuesProvider[I,O] = I => O

  type ValidationInContext[I,O] = (Context, I => Validated[O])

  case class Parse2[I1,O1,I2,O2](p1: ValidationInContext[I1,O1], p2: ValidationInContext[I2,O2]){
    def providingValues: ((I1,I2)) => Validated[(O1,O2)] = {
      case (i1,i2) => p1._2(i1).at(p1._1).combine(p2._2(i2).at(p2._1))((_,_))
    }
  }

  case class Parse3[I1,O1,I2,O2,I3,O3](p1: (Context,I1 => Validated[O1]), p2: (Context, I2 => Validated[O2]), p3: (Context, I3 => Validated[O3])){
    def providingValues: ((I1,I2,I3)) => Validated[(O1,O2,O3)] = {
      case (i1,i2,i3) => p1._2(i1).at(p1._1).combine(p2._2(i2).at(p2._1))((_,_)).combine(p3._2(i3).at(p3._1)){
        case((o1,o2),o3) => (o1,o2,o3)
      }
    }
  }

  def parse2[I1,O1,I2,O2](p1: (Context,I1 => Validated[O1]), p2: (Context, I2 => Validated[O2])): Parse2[I1,O1,I2,O2] = Parse2(p1,p2)
  def parse3[I1,O1,I2,O2,I3,O3](p1: (Context,I1 => Validated[O1]), p2: (Context, I2 => Validated[O2]), p3: (Context, I3 => Validated[O3])): Parse3[I1,O1,I2,O2,I3,O3] = Parse3(p1,p2,p3)



  def sampleMethod(sNumber: String, sDate1: String, sDate2: String): Validated[(Int, Date, Date)] = {

    import Context.__
    val validateArgs = parse3(
      __ / "sNumber" -> (parseInt && greaterOrEqualThan(5)),
      __ / "sDate1" -> parseSimpleDate,
      __ / "sDate2" -> parseSimpleDate
    ).providingValues

    validateArgs((sNumber,sDate1,sDate2))
  }

  def validateIsEmpty[T]:Option[T] => Validity = {
    case Some(v) => Validity.Invalid(itShould("not be defined"))
    case None => Validity.Valid
  }

  def validateNonEmpty[T]:Option[T] => Validated[T] = {
    case Some(v) => Valid(v)
    case None => illegal(itShould("be defined"))
  }

  def at[T](ctx:String): T => Validated[T] = {
    i => Valid(i).at(ctx)
  }

//  def sampleMethod2(sOption:String, str:Option[String],sNumber:Option[String],sDate:Option[String]): Validated[(Options.Value, (Option[String], Option[Int], Option[Date]))] = {
//    val validateOption = parseEnum(Options)
//    val validateStr = at("str")(str)
//
//    val validateAll = (so:String, str:Option[String], sn:Option[String], sd:Option[String]) => {
//      validateOption(so).at("options").flatMap{
//        case Options.string => validateStr flatMap validateNonEmpty and validateIsEmpty(str) and validateIsEmpty(sd)
//        case Options.number => validateNonEmpty(sn).flatMap(parseInt && greaterOrEqualThan(5))
//      }
//    }
//  }

  def sampleMethod3(opts:Seq[String]): Validated[Seq[Options.Value]] = {
    val validateOptions = foreach(parseEnum(Options)) combine nonEmpty
    validateOptions(opts)
  }

  describe("combined validation") {

    val sDate1 =  "30/12/2015 15:42:23"
    val date1 = new GregorianCalendar(2015, 11, 30, 15, 42, 23).getTime
    val sDate2 =  "31/12/2015 23:59:59"
    val date2 = new GregorianCalendar(2015, 11, 31, 23, 59, 59).getTime

    it("should return valid values") {
      val res = sampleMethod("6",sDate1,sDate2)
      val validRes = res match {
        case Valid(v) => v
        case _ => fail(s"$res should be a success")
      }
      assert(validRes === ((6,date1,date2)))
    }

    it("should return a single error message") {
      val res = sampleMethod("6", "30/12/2015 15:42:23 hahaha", "31/12/2015 23:59:59")
      assert(res.isError)
      val validity = res.validity
      val failures = validity.fold(identity, fail("unexepcted success")).issues
      assert(failures.size === 1)
      val sDate1Failures = failures(Context.Self / "sDate1")
      assert(sDate1Failures.size === 1)
      val sDate1Failure = sDate1Failures.head
      assert(sDate1Failure === ValidationFailure("be a date", Map("pattern" -> pattern)))

    }

  }
}
