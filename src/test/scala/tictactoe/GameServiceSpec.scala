package tictactoe

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec._

import java.io.StringReader

class GameServiceSpec extends AnyWordSpec with Matchers {
  val board: Board = Board(Row(X, Empty, O), Row(Empty, O, Empty), Row(Empty, Empty, X))

  "GameService" should {
    "Display a board" in {
      val expected =
        """
          |   A  B  C
          |1  X  .  O
          |2  .  O  .
          |3  .  .  X
          |""".stripMargin

      GameText.displayBoard(board) shouldBe expected
    }

    "Show available moves" in {
      GameService.showNextMoves(O, board) shouldBe List("B1", "A2", "C2", "A3", "B3")
      GameService.showNextMoves(X, board) shouldBe List("B1", "A2", "C2", "A3", "B3")
    }

    "Update board" in {
      GameService.updateBoard(X, "A3", board) shouldBe Board(
        Row(X, Empty, O),
        Row(Empty, O, Empty),
        Row(X, Empty, X)
      )

      GameService.updateBoard(O, "C2", board) shouldBe Board(
        Row(X, Empty, O),
        Row(Empty, O, O),
        Row(Empty, Empty, X)
      )
    }

    "Check row winner" in {
      val winningBoard = Board(Row(X, X, X), Row(Empty, O, Empty), Row(Empty, Empty, X))

      GameService.checkRowWinner(board) shouldBe None
      GameService.checkRowWinner(winningBoard) shouldBe Some(X)
    }

    "Check column winner" in {
      val winningBoard = Board(Row(X, O, O), Row(Empty, O, Empty), Row(Empty, O, X))

      GameService.checkColumnWinner(O, board) shouldBe None
      GameService.checkColumnWinner(O, winningBoard) shouldBe Some(O)
    }

    "Check diagonal winner" in {
      val winningBoard = Board(Row(X, O, O), Row(Empty, X, Empty), Row(Empty, O, X))

      GameService.checkDiagonalWinner(O, board) shouldBe None
      GameService.checkDiagonalWinner(X, winningBoard) shouldBe Some(X)
    }

    "Check board is full" in {
      val fullBoard = Board(Row(X, O, O), Row(X, X, O), Row(O, O, X))

      GameService.checkFull(board) shouldBe false
      GameService.checkFull(fullBoard) shouldBe true
    }

    "Switch player" in {
      GameService.switch(X) shouldBe O
      GameService.switch(O) shouldBe X
    }

    "Receive coordinate input" in {
      val in             = new StringReader("B1")
      val availableMoves = GameService.showNextMoves(X, board)

      Console.withIn(in) {
        GameService.receiveCoordinateInput(availableMoves) shouldBe "B1"
      }
    }

    "Receive Square input" in {
      val in = new StringReader("x")

      Console.withIn(in) {
        GameService.receiveSquareInput() shouldBe X
      }
    }
  }
}
