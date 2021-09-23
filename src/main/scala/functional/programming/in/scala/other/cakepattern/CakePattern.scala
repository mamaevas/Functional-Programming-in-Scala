package functional.programming.in.scala.other.cakepattern

trait Repository {
  protected def getData: List[Int]
}

trait Repository1 extends Repository {
  protected def getData: List[Int] = List(1, 2, 3)
}

trait Repository2 extends Repository {
  protected def getData: List[Int] = List(-1, -2, -3)
}

trait Service {
  this: Repository =>
  protected def doSomething(): List[Int] = getData
}

trait Service1 extends Service {
  this: Repository =>
  override protected def doSomething(): List[Int] = getData.map(_ * 2)
}

trait Service2 extends Service {
  this: Repository =>

  override protected def doSomething(): List[Int] = getData.map(_ * 10)
}

class Service3(v: Int) extends Service with Repository2 {
  override protected def doSomething(): List[Int] = getData.map(_ + v)
}

trait Consumer {
  this: Service =>
  def printData(): Unit = println(s"${this.getClass.getSimpleName}: ${doSomething()}")
  // Encapsulation problem is absent because this is not compile
  //    def getDataDirectlyFromRepository: List[Int] = getData
}

trait CheatConsumer {
  this: Service with Repository =>
  // Encapsulation problem
  def getDataDirectlyFromRepository: List[Int] = getData
}

// Inject concrete Service implementation
class ConsumerImpl extends Consumer with Service with Repository1

class ConsumerImpl1 extends Consumer with Service1 with Repository1

class ConsumerImpl2 extends Consumer with Service2 with Repository2

case class ConsumerImpl3(private val v: Int) extends Service3(v) with Consumer

object CakeApp extends App {
  (new ConsumerImpl).printData()
  (new ConsumerImpl1).printData()
  (new ConsumerImpl2).printData()
  ConsumerImpl3(5).printData()

}