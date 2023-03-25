package tictactoe

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec._

import java.io.{ ByteArrayInputStream, ByteArrayOutputStream, InputStream, StringReader }
import java.nio.charset.StandardCharsets

class GameServiceSpec extends AnyWordSpec with Matchers {
  val board: Board     = Board(Row(X, Empty, O), Row(Empty, O, Empty), Row(Empty, Empty, X))
  val players: Players = Players(Player(X, Human), Player(O, Computer))

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
      val gameService = new GameService(players)
      gameService.nextMoves(board) shouldBe List("B1", "A2", "C2", "A3", "B3")
      gameService.nextMoves(board) shouldBe List("B1", "A2", "C2", "A3", "B3")
    }

    "Update board" in {
      val gameService = new GameService(players)
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
      val gameService  = new GameService(players)
      val winningBoard = Board(Row(X, X, X), Row(Empty, O, Empty), Row(Empty, Empty, X))

      gameService.checkRowWinner(board) shouldBe None
      gameService.checkRowWinner(winningBoard) shouldBe Some(X)
    }

    "Check column winner" in {
      val gameService  = new GameService(players)
      val winningBoard = Board(Row(X, O, O), Row(Empty, O, Empty), Row(Empty, O, X))

      gameService.checkColumnWinner(board) shouldBe None
      gameService.checkColumnWinner(winningBoard) shouldBe Some(O)
    }

    "Check diagonal winner" in {
      val gameService  = new GameService(players)
      val winningBoard = Board(Row(X, O, O), Row(Empty, X, Empty), Row(Empty, O, X))

      gameService.checkDiagonalWinner(board) shouldBe None
      gameService.checkDiagonalWinner(winningBoard) shouldBe Some(X)
    }

    "Check board is full" in {
      val gameService = new GameService(players)
      val fullBoard   = Board(Row(X, O, O), Row(X, X, O), Row(O, O, X))

      gameService.checkDraw(board) shouldBe false
      gameService.checkDraw(fullBoard) shouldBe true
    }

    "Receive coordinate input" in {
      val gameService    = new GameService(players)
      val in             = new StringReader("B1")
      val availableMoves = gameService.nextMoves(board)

      Console.withIn(in) {
        gameService.receiveCoordinateInput(availableMoves) shouldBe "B1"
      }
    }

    "Receive Square input" in {
      val in = new StringReader("x")

      Console.withIn(in) {
        GameService.receiveSquareInput() shouldBe X
      }
    }

    "Choose player" in {
      val lines =
        raw"""x
             |human
             |o
             |h
             |c
             |""".stripMargin
      val in: InputStream = new ByteArrayInputStream(lines.getBytes(StandardCharsets.UTF_8))

      Console.withIn(in) {
        GameService.choosePlayer() shouldBe (Player(X, Human))
        GameService.choosePlayer() shouldBe (Player(O, Human))
        GameService.choosePlayer(Some(X)) shouldBe (Player(X, Computer))
      }
    }

    "Win rigged game Computer vs Computer" in {
      val out          = new ByteArrayOutputStream()
      val board: Board = Board(Row(O, Empty, X), Row(Empty, X, Empty), Row(Empty, Empty, O))
      val gameService  = new GameService(Players(Player(X, Computer), Player(O, Computer)))

      Console.withOut(out) {
        gameService.startGame(board)
      }

      out.toString should include("Player X wins the game!!")
    }

    "Draw game with empty board Computer vs Computer" in {
      val out = new ByteArrayOutputStream()
      val board: Board =
        Board(Row(Empty, Empty, Empty), Row(Empty, Empty, Empty), Row(Empty, Empty, Empty))
      val gameService = new GameService(Players(Player(X, Computer), Player(O, Computer)))

      Console.withOut(out) {
        gameService.startGame(board)
      }

      out.toString should include("It is a draw!!")
    }
  }
}
