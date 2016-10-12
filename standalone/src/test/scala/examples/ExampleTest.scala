package examples

import java.text.{ParsePosition, SimpleDateFormat}
import java.util
import java.util.{Date, GregorianCalendar}

import com.agilogy.classis.{Applicative, ApplicativeSyntax}
import com.agilogy.path.Path$
import com.agilogy.validare._
import com.agilogy.validare.Validity.ValidationFailure
import org.scalatest.FunSpec
import com.agilogy.validare.standalone._

import scala.language.implicitConversions

trait Named[T]{
  def name(v:T):String
}

object Named{
  import scala.language.reflectiveCalls
//  implicit def structurallyNamed[T<:{val name:String}](v:T):Named[T] = new Named[T] {
//    override def name(v: T): String = v.name
//  }
  def apply[T](nameFunction: T => String):Named[T] = new Named[T] {
    override def name(v: T): String = nameFunction(v)
  }
  implicit class NamedOps[T:Named](v:T){
    def name:String = implicitly[Named[T]].name(v)
  }
}
import Named._

sealed trait Options{
  val name:String
  override def toString: String = name
}
object Options{
  abstract class BaseOptions(val name:String) extends Options
  case object string extends BaseOptions("string")
  case object number extends BaseOptions("number")
  case object date extends BaseOptions("date")
  val values = Seq[Options](string,number,date)
  implicit val namedOptions: Named[Options] = Named[Options](_.name)
}

//object Options extends Enumeration{
//  val string = Value
//  val number = Value
//  val date = Value
//}

//TODO: This is not really a test but an example
class ExampleTest extends FunSpec {

  module =>

  type Reader[I,O] = I => Validated[O]
  type Validator[I] = I => Validity

  implicit class ReaderOps[I, O](f: I => Validated[O]) {
    private def fr:Reader[I,O] = f
    def &&(f2: O => Validity):Reader[I,O] = i => f(i).flatMap(check(f2))
    def combine[O2](f2: Reader[O,O2]): Reader[I,O2] = module.andThen(f, f2)
    def >>[O2](f2: Reader[O,O2]): Reader[I,O2] = module.andThen(f, f2)
    def flatMap[I2,O2](f2: O => Reader[I2,O2]): (I, I2) => Validated[(O, O2)] = flatMap2(f,f2)
    //TODO: Better name and tests. Is it useful at all?
    def tupledFlatMap[I2,O2](f2: O => Reader[I2,O2]): Reader[(I, I2),(O, O2)] = flatMap2(fr,f2).tupled
    def at(ctx:Path):Reader[I,O] = {
      v => f(v).at(ctx)
    }
    def at(ctx:String):Reader[I,O] = at(Path.Self / ctx)
  }


  def parseAdt[T:Named](values:Seq[T]): Reader[String,T] = {
    v =>
      val value = values.find(_.name == v)
      value.fold[Validated[T]](illegal(itShould(s"be a valid value", "validValues" -> values.map(_.name).mkString(","))))(Valid.apply)
  }

  def foreach[I,O](f:Reader[I,O]): Reader[Seq[I],Seq[O]] = {
    is =>
      is.zipWithIndex.foldLeft[Validated[Seq[O]]](Valid(Seq.empty)){
        case (acc, (i,idx)) => acc.combine(f(i).at(Path.Self / idx))(_ :+ _)
      }
  }

  def nonEmpty[T <: Iterable[_]]: Reader[T,T] = {
    is => if(is.isEmpty) invalid(is,itShould("be non empty")) else Valid(is)
  }

  def andThen[T1, T2, T3](f1: Reader[T1,T2], f2: Reader[T2,T3]): Reader[T1,T3] = {
    i =>
      f1(i).flatMap(i2 => f2(i2))
  }

  def and[I](f1:I => Validated[Unit], f2:I => Validated[Unit]): I => Validated[Unit] = {
    i =>
      f1(i).flatMap(_ => f2(i))
  }

  def flatMap2[I1, O1, I2, O2](f1: Reader[I1,O1], f2: O1 => Reader[I2,O2]): (I1, I2) => Validated[(O1, O2)] = {
    (i1, i2) =>
      val vo1 = f1(i1)
      vo1.flatMap{
        o1 =>
          f2(o1)(i2).map(o2 => (o1,o2))
      }
  }

  def check[I](f: I => Validity): Reader[I,I] = i => f(i).fold[Validated[I]](inv => Invalid(i,inv),Valid(i))

  def greaterOrEqualThan(min: Int)(v: Int): Validity = {
    if (v >= min) Validity.Valid
    else Validity.Invalid(itShould("be greater or equal than", "value" -> v.toString))
  }

  val parseInt: Reader[String,Int] = {
    s =>
      try {
        Valid(s.toInt)
      } catch {
        case nfe: NumberFormatException => illegal(itShould("be an int"))
      }
  }

  implicit def someReader[I,O](f:Reader[I,O]):Some[I] => Validated[Some[O]] = {
    case Some(v) =>
      f(v).map(Some.apply)
  }

  def parseDate(pattern: String)(s: String): Validated[Date] = {
    val pos = new ParsePosition(0)
    val df = new SimpleDateFormat(pattern)
    val res = df.parse(s, pos)
    if (pos.getIndex < s.length) illegal(itShould("be a date", "pattern" -> pattern))
    else Valid(res)
  }

  val pattern = "dd/MM/yyyy HH:mm:ss"
  val parseSimpleDate = parseDate(pattern) _


//  type ValidationInContext[I,O] = (Context, Reader[I,O])
//
//  case class Parse2[I1,O1,I2,O2](p1: ValidationInContext[I1,O1], p2: ValidationInContext[I2,O2]){
//    def providingValues: ((I1,I2)) => Validated[(O1,O2)] = {
//      case (i1,i2) => p1._2(i1).at(p1._1).combine(p2._2(i2).at(p2._1))((_,_))
//    }
//  }
//
//  case class Parse3[I1,O1,I2,O2,I3,O3](p1: (Context,Reader[I1,O1]), p2: (Context, Reader[I2,O2]), p3: (Context, Reader[I3,O3])){
//    def providingValues: ((I1,I2,I3)) => Validated[(O1,O2,O3)] = {
//      case (i1,i2,i3) => p1._2(i1).at(p1._1).combine(p2._2(i2).at(p2._1))((_,_)).combine(p3._2(i3).at(p3._1)){
//        case((o1,o2),o3) => (o1,o2,o3)
//      }
//    }
//  }
//
//  def parse2[I1,O1,I2,O2](p1: (Context,Reader[I1,O1]), p2: (Context, Reader[I2,O2])): Parse2[I1,O1,I2,O2] = Parse2(p1,p2)
//  def parse3[I1,O1,I2,O2,I3,O3](p1: (Context,Reader[I1,O1]), p2: (Context, Reader[I2,O2]), p3: (Context, Reader[I3,O3])): Parse3[I1,O1,I2,O2,I3,O3] = Parse3(p1,p2,p3)

  def combine3[I1,O1,I2,O2,I3,O3,O](r1:Reader[I1,O1],r2:Reader[I2,O2],r3:Reader[I3,O3])(f:(O1,O2,O3) => O):Reader[(I1,I2,I3),O] = new Reader[(I1,I2,I3),O] {
    import validatedApplicative._
    override def apply(v1: (I1, I2, I3)): Validated[O] = v1 match {
      case (v1,v2,v3) => map3(r1(v1),r2(v2),r3(v3))(f)
    }
  }



  def validateIsEmpty[T]:Option[T] => Validity = {
    case Some(v) => Validity.Invalid(itShould("not be defined"))
    case None => Validity.Valid
  }

  def validateNonEmpty[T]:Option[T] => Validity = {
    case Some(v) => Validity.Valid
    case None => Validity.Invalid(itShould("be defined"))
  }

  def mandatory[T]:Reader[Option[T],T] = {
    case Some(v) => Valid(v)
    case None => illegal(itShould("be defined"))
  }

//  def sampleMethod2(sOption:Option[String], str:Option[String],sNumber:Option[String],sDate:Option[String]): Validated[(Options, (Option[String], Option[Int], Option[Date]))] = {
//    val validateOption: Reader[Option[String], Options] = mandatory[String] combine parseAdt(Options.values) at (Context.Self / "sOption")
//    val validateStr:Reader[Option[String],String] = mandatory[String] at "str"
//    val validateNumber:Reader[Option[String],Int] = mandatory[String] combine parseInt && greaterOrEqualThan(5) at "sNumber"
//    val validateDate:Reader[Option[String],Date] = mandatory[String] combine parseSimpleDate at "sDate"
//    val validateAll = validateOption.flatMap{
//      case Options.string => validateStr
//      case Options.number => ???
//      case Options.date => ???
//  }


  def sampleMethod3(opts:Seq[String]): Validated[Seq[Options]] = {
    val parseOptions = parseAdt(Options.values)
    val validateOptions = foreach(parseOptions) combine nonEmpty
    validateOptions(opts)
  }


  describe("important part") {
    // We want to read a single input (e.g. a JsValue, a String, a Map[String,Any]... and have a result...
    // composing possibly multiple readers

    trait JsValue
    case class JsInt(v:Int) extends JsValue
    case class JsString(v:String) extends JsValue
    case class JsObject(v:Map[String,JsValue]) extends JsValue

    case class Person(name:String,age:Int)

//    val readName:Reader[Option[String],String]
//    val readAge:Reader[Option[String],Int]


    implicit val isInt:Reader[JsValue,Int] = {
      case JsInt(v) => Valid(v)
      case _ => illegal(itShould("be an int"))
    }
    implicit val isString:Reader[JsValue,String] = {
      case JsString(v) => Valid(v)
      case _ => illegal(itShould("be a string"))
    }

    def get(ctx:String):Reader[JsValue,Option[JsValue]] = {
      case JsObject(v) => Valid(v.get(ctx))
      case _ => illegal(itShould("be an object"))
    }

    implicit def readerApplicative[I]: Applicative[({type λ[O] = Reader[I, O]})#λ] = new Applicative[({type λ[O] = Reader[I, O]})#λ] {
      override def apply[A, B](fab: Reader[I, (A) => B])(fa: Reader[I, A]): Reader[I, B] = new Reader[I, B] {
        override def apply(v1: I): Validated[B] = Validare.map2(fab(v1), fa(v1)) {
          case (f, v) => f(v)
        }
      }

      override def unit[A](a: => A): Reader[I, A] = new Reader[I, A] {
        override def apply(v1: I): Validated[A] = Valid(a)
      }

      override def map[A, B](fa: Reader[I, A])(f: (A) => B): Reader[I, B] = {
        i =>
          fa(i).map(f)
      }
    }

    val jsReaderApplicative: Applicative[({type λ[O] = Reader[JsValue, O]})#λ] = readerApplicative[JsValue]

    import jsReaderApplicative._

    val readPerson:Reader[JsValue,Person] =
      map2(get("name") >> mandatory >> isString, get("age") >> mandatory >> isInt && greaterOrEqualThan(18))(Person.apply)

//    def optional[I,O](r:Reader[I,O]):Reader[Option[I],Option[O]] = new Reader[Option[I],Option[O]] {
//      override def apply(v1: Option[I]): Validated[Option[O]] = v1 match {
//        case None => Valid(None)
//        case Some(av1) => r(av1).map(Some.apply)
//      }
//    }

    def getMand[T](ctx:String)(implicit r:Reader[JsValue,T]):Reader[JsValue,T] = get(ctx) >> mandatory >> r
//    def getOpt[T](ctx:String)(implicit r:Reader[JsValue,T]):Reader[JsValue,Option[T]] = get(ctx) >> optional(r)

    val readPerson2:Reader[JsValue,Person] =
      map2(getMand[String]("name"), getMand[Int]("age") && greaterOrEqualThan(18))(Person.apply)

    it("sould read a person"){
//      val js3 = JsInt(3)
      val js30 = JsInt(30)
      val jsJohn = JsString("John")
      val jsPerson = JsObject(Map[String,JsValue]("name"->jsJohn, "age"->js30))
      val res: Validated[Person] = readPerson(jsPerson)
      assert(res.isSuccess)
      val Valid(p) = res
      assert(p === Person("John",30))
      val res2 = readPerson2(jsPerson)
      assert(res2.isSuccess)
      val Valid(p2) = res2
      assert(p2 === p)
    }
  }



  describe("combined validation") {

    def sampleMethod(sNumber: String, sDate1: String, sDate2: String): Validated[(Int, Date, Date)] = {

      //    import Context.__
      //    val validateArgs = parse3(
      //      __ / "sNumber" -> (parseInt && greaterOrEqualThan(5)),
      //      __ / "sDate1" -> parseSimpleDate,
      //      __ / "sDate2" -> parseSimpleDate
      //    ).providingValues
      //    validateArgs((sNumber,sDate1,sDate2))

      val readSNumber = parseInt && greaterOrEqualThan(5) at "sNumber"
      val readSDate1 = parseSimpleDate at "sDate1"
      val readSDate2 = parseSimpleDate at "sDate2"
      //    combine3(readSNumber, readSDate1, readSDate2)((_,_,_))(sNumber,sDate1,sDate2)
      import validatedApplicative._
      map3(readSNumber(sNumber),readSDate1(sDate1),readSDate2(sDate2))(Tuple3.apply)
    }

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
      val sDate1Failures = failures(Path.Self / "sDate1")
      assert(sDate1Failures.size === 1)
      val sDate1Failure = sDate1Failures.head
      assert(sDate1Failure === ValidationFailure("be a date", Map("pattern" -> pattern)))

    }

  }
}
