package chapters.ch4.ex4_9_3

import cats.data.State
import cats.syntax.applicative._

object Calculator {

  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] = {
    val parsed = sym match {
      case "+" => AddIdentifier
      case "-" => MinusIdentifier
      case "*" => MultipleIdentifier
      case "/" => DivisionIdentifier
      case num => NumberToken(num.toInt)
    }
    parsed.eval()
  }

  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(0.pure[CalcState]) { (l, r) =>
      l.flatMap(_ => evalOne(r))
    }

  def evalInput(sym: String): Int = evalAll(sym.split(" ").toList).runA(Nil).value
}

sealed trait Token {
  def eval(): State[List[Int], Int]
}

case class NumberToken(num: Int) extends Token {
  def eval(): State[List[Int], Int] = State[List[Int], Int] { state =>
    (num :: state, num)
  }
}

sealed trait OpsIdentifier extends Token {
  override def eval(): State[List[Int], Int] =
    State[List[Int], Int] {
      case s :: f :: tail =>
        val evaluation: Int = operate(f, s)
        (evaluation :: tail, evaluation)
      case _ => throw new RuntimeException("Invalid Syntax??")
    }

  protected def operate(left: Int, right: Int): Int
}

case object AddIdentifier extends OpsIdentifier {
  override def operate(left: Int, right: Int): Int = left + right
}

case object MinusIdentifier extends OpsIdentifier {
  override def operate(left: Int, right: Int): Int = left - right
}

case object MultipleIdentifier extends OpsIdentifier {
  override def operate(left: Int, right: Int): Int = left * right
}

case object DivisionIdentifier extends OpsIdentifier {
  override def operate(left: Int, right: Int): Int = left / right
}
