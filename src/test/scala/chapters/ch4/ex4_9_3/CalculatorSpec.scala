package chapters.ch4.ex4_9_3

import org.scalatest._

class CalculatorSpec extends FunSpec with Matchers {

  import chapters.ch4.ex4_9_3.Calculator._

  describe("#evalOne") {
    describe("1 2 +") {
      it("should calculate and return answer 3") {
        val program = for {
          _ <- evalOne("1")
          _ <- evalOne("2")
          ans <- evalOne("+")
        } yield ans

        program.runA(Nil).value shouldBe 3
      }
    }

    describe("2 1 -") {
      it("should calculate and return answer 1") {
        val program = for {
          _ <- evalOne("2")
          _ <- evalOne("1")
          ans <- evalOne("-")
        } yield ans

        program.runA(Nil).value shouldBe 1
      }
    }

    describe("1 2 *") {
      it("should calculate and return answer 2") {
        val program = for {
          _ <- evalOne("1")
          _ <- evalOne("2")
          ans <- evalOne("*")
        } yield ans

        program.runA(Nil).value shouldBe 2
      }
    }

    describe("4 2 /") {
      it("should calculate and return answer 2") {
        val program = for {
          _ <- evalOne("4")
          _ <- evalOne("2")
          ans <- evalOne("/")
        } yield ans

        program.runA(Nil).value shouldBe 2
      }
    }
  }

  describe("evalAll") {
    describe("List(1, 2, +, 3, *)") {
      it("should calculate and return answer 9") {
        val program = evalAll(List("1", "2", "+", "3", "*"))
        program.runA(Nil).value shouldBe 9
      }
    }

    describe("List(4, 2, /, 3, +)") {
      it("should calculate and return answer 5") {
        val program = evalAll(List("4", "2", "/", "3", "+"))
        program.runA(Nil).value shouldBe 5
      }
    }
  }

  describe("evalInput") {
    describe("1 2 +") {
      it("should calculate nad return answer 3") {
        val program = evalInput("1 2 +")
        program shouldBe 3
      }
    }
  }
}
