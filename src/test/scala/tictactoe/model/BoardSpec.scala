package tictactoe.model

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class BoardSpec extends AnyWordSpec with Matchers {
  import Square._

  val board: Board = Board(Row(X, Empty, O), Row(Empty, O, Empty), Row(Empty, Empty, X))

  "Board" should {
    "Calculate next moves" in {
      board.availableMoves() shouldBe List("B1", "A2", "C2", "A3", "B3")
    }

    "Update board" in {
      board.updateBoard(X, "A3") shouldBe Board(
        Row(X, Empty, O),
        Row(Empty, O, Empty),
        Row(X, Empty, X)
      )

      board.updateBoard(O, "C2") shouldBe Board(
        Row(X, Empty, O),
        Row(Empty, O, O),
        Row(Empty, Empty, X)
      )
    }

    "Check row winner" in {
      val winningBoard = Board(Row(X, X, X), Row(Empty, O, Empty), Row(Empty, Empty, X))

      board.checkWinner() shouldBe None
      winningBoard.checkWinner() shouldBe Some(X)
    }

    "Check column winner" in {
      val winningBoard = Board(Row(X, O, O), Row(Empty, O, Empty), Row(Empty, O, X))

      board.checkWinner() shouldBe None
      winningBoard.checkWinner() shouldBe Some(O)
    }

    "Check diagonal winner" in {
      val winningBoard = Board(Row(X, O, O), Row(Empty, X, Empty), Row(Empty, O, X))

      board.checkWinner() shouldBe None
      winningBoard.checkWinner() shouldBe Some(X)
    }

    "Check board is full" in {
      val fullBoard = Board(Row(X, O, O), Row(X, X, O), Row(O, O, X))

      board.isDraw shouldBe false
      fullBoard.isDraw shouldBe true
    }
  }
}
