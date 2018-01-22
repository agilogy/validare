package ut.validation

trait Animal{
  val name:String
}
final case class Dog(name:String) extends Animal
final case class Cat(name:String, age:Int) extends Animal

