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
      val gameService = new GameService(Player(X, Human), Player(O, Computer))
      gameService.nextMoves(board) shouldBe List("B1", "A2", "C2", "A3", "B3")
      gameService.nextMoves(board) shouldBe List("B1", "A2", "C2", "A3", "B3")
    }

    "Update board" in {
      val gameService = new GameService(Player(X, Human), Player(O, Computer))
      gameService.updateBoard(X, "A3", board) shouldBe Board(
        Row(X, Empty, O),
        Row(Empty, O, Empty),
        Row(X, Empty, X)
      )

      gameService.updateBoard(O, "C2", board) shouldBe Board(
        Row(X, Empty, O),
        Row(Empty, O, O),
        Row(Empty, Empty, X)
      )
    }

    "Check row winner" in {
      val gameService  = new GameService(Player(X, Human), Player(O, Computer))
      val winningBoard = Board(Row(X, X, X), Row(Empty, O, Empty), Row(Empty, Empty, X))

      gameService.checkRowWinner(board) shouldBe None
      gameService.checkRowWinner(winningBoard) shouldBe Some(X)
    }

    "Check column winner" in {
      val gameService  = new GameService(Player(X, Human), Player(O, Computer))
      val winningBoard = Board(Row(X, O, O), Row(Empty, O, Empty), Row(Empty, O, X))

      gameService.checkColumnWinner(O, board) shouldBe None
      gameService.checkColumnWinner(O, winningBoard) shouldBe Some(O)
    }

    "Check diagonal winner" in {
      val gameService  = new GameService(Player(X, Human), Player(O, Computer))
      val winningBoard = Board(Row(X, O, O), Row(Empty, X, Empty), Row(Empty, O, X))

      gameService.checkDiagonalWinner(O, board) shouldBe None
      gameService.checkDiagonalWinner(X, winningBoard) shouldBe Some(X)
    }

    "Check board is full" in {
      val gameService = new GameService(Player(X, Human), Player(O, Computer))
      val fullBoard   = Board(Row(X, O, O), Row(X, X, O), Row(O, O, X))

      gameService.checkDraw(board) shouldBe false
      gameService.checkDraw(fullBoard) shouldBe true
    }

    "Receive coordinate input" in {
      val gameService    = new GameService(Player(X, Human), Player(O, Computer))
      val in             = new StringReader("B1")
      val availableMoves = gameService.nextMoves(board)

      Console.withIn(in) {
        gameService.receiveCoordinateInput(availableMoves) shouldBe "B1"
      }
    }

    "Receive Square input" in {
      val gameService = new GameService(Player(X, Human), Player(O, Computer))
      val in          = new StringReader("x")

      Console.withIn(in) {
        gameService.receiveSquareInput() shouldBe X
      }
    }
  }
}
