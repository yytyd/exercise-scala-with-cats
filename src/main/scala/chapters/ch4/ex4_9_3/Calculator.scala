package chapters.ch4.ex4_9_3

import cats.data.State

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

  def evalAll(input: List[String]): CalcState[Int] = ???

  def evalInput(sym: String): CalcState[Int] = ???
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
        val evaluation: Int = operator(f, s)
        (evaluation :: tail, evaluation)
      case _ => throw new RuntimeException("Invalid Syntax??")
    }

  protected def operator(left: Int, right: Int): Int
}

case object AddIdentifier extends OpsIdentifier {
  override def operator(left: Int, right: Int): Int = left + right
}

case object MinusIdentifier extends OpsIdentifier {
  override def operator(left: Int, right: Int): Int = left - right
}

case object MultipleIdentifier extends OpsIdentifier {
  override def operator(left: Int, right: Int): Int = left * right
}

case object DivisionIdentifier extends OpsIdentifier {
  override def operator(left: Int, right: Int): Int = left / right
}
