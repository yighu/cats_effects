object MonadsForBeginners {

case class SafeValue[+T](private val internalValue: T) { //constructor = pure or unit
  def get: T = synchronized{
    internalValue
  }

  def flatMap[S](transformer: T => SafeValue[S]): SafeValue[S] = synchronized{
    transformer(internalValue)
  }
}
  def gimmeSafeValue [T](value: T): SafeValue[T] = SafeValue(value)
  val safeString: SafeValue[String]= gimmeSafeValue("Scala is awesome")
  val string = safeString.get
  val upperString = string.toUpperCase
  val upperStringSafe = SafeValue(upperString)
   //ExtractTransformWrap

  val uppserStringSafe2 = safeString.flatMap(s => SafeValue(s.toUpperCase))


//  Option, Either, Future, Try, etc

  //Monads properties
  //prop 1
  def twoConsecutive(x: Int) = List(x, x+1)
  twoConsecutive(3) //List(3,4)
  List(3).flatMap(twoConsecutive) //List(3,4)
  //Monad(x).flatMap(f) = f(x)
  //prop 2
  List(1,2,3).flatMap(x =>List(x)) //List(1,2,3)
  //Monad(v).flatMap(x => Monad(x)) useless returns Monad(v)
  //prop 3 ETW-ETW
  val increamenter  = (x: Int) => List(x,x+1)
  val doubler = (x: Int) => List(x, 2*x)
  (0 to 3).flatMap(increamenter).flatMap(doubler)
  (0 to 3).flatMap(doubler).flatMap(increamenter)
  //Monad(v).flatMap(f).flatMap(g) = Monad(v).flatMap(x=>f(x).flatMap(g))

}
