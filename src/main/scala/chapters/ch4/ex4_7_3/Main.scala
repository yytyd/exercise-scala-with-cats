package chapters.ch4.ex4_7_3

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object Main extends App {
  import FactorialCalculator._

  Await.result(Future.sequence(Vector(
    Future(factorial(3)),
    Future(factorial(3))
  )), 4.seconds)
}

object FactorialCalculator {
  def slowly[A](body: => A): A = try body finally Thread.sleep(100)
  def factorial(n: Int): Int = {
    val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
    println(ans)
    ans
  }
}
