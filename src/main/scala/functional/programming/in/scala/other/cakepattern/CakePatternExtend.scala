package functional.programming.in.scala.other.cakepattern

trait Dao {
    protected def getData: List[Int] = List(1, 2, 3)
}

trait ServiceDao extends Dao {
    protected def doSomething(): List[Int] = getData
}

trait ServiceDao1 extends ServiceDao {
    override protected def doSomething(): List[Int] = getData.map(_ * 2)
}

trait ServiceDao2 extends ServiceDao {
    override protected def doSomething(): List[Int] = getData.map(_ * 10)
}

trait ConsumerDao extends ServiceDao {
    def printData(): Unit = println(s"${this.getClass.getSimpleName}: ${doSomething()}")
    // Encapsulation problem
    def getDataDirectlyFromRepository: List[Int] = getData
}

class ConsumerDaoImpl extends ConsumerDao with ServiceDao with Dao

class ConsumerDaoImpl1 extends ConsumerDao with ServiceDao1 with Dao

class ConsumerDaoImpl2 extends ConsumerDao with ServiceDao2 with Dao

object CakeDaoApp extends App {
    (new ConsumerDaoImpl).printData()
    (new ConsumerDaoImpl1).printData()
    (new ConsumerDaoImpl2).printData()

}
