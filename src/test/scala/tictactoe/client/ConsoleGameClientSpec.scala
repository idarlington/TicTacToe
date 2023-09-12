package tictactoe.client

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec._
import tictactoe.engine.InMemoryGameEngine
import tictactoe.model._

import java.io.{ ByteArrayInputStream, ByteArrayOutputStream, InputStream, StringReader }
import java.nio.charset.StandardCharsets

class ConsoleGameClientSpec extends AnyWordSpec with Matchers {
  import Square._
  val board: Board     = Board(Row(X, Empty, O), Row(Empty, O, Empty), Row(Empty, Empty, X))
  val players: Players = Players(HumanPlayer(X), ComputerPlayer(O))
  val gameEngine       = new InMemoryGameEngine()

  "ConsoleGameClient" should {
    "Display a board" in {
      val expected =
        """
          |   A  B  C
          |1  X  .  O
          |2  .  O  .
          |3  .  .  X
          |""".stripMargin

      ConsoleUtils.displayBoard(board) shouldBe expected
    }

    "Receive coordinate input" in {
      val in             = new StringReader("B1")
      val availableMoves = board.availableMoves()

      Console.withIn(in) {
        ConsoleGameClient.receiveCoordinateInput(availableMoves) shouldBe "B1"
      }
    }

    "Receive Square input" in {
      val in = new StringReader("x")

      Console.withIn(in) {
        ConsoleGameClient.receiveSquareInput() shouldBe X
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
        ConsoleGameClient.choosePlayer() shouldBe (HumanPlayer(X))
        ConsoleGameClient.choosePlayer() shouldBe (HumanPlayer(O))
        ConsoleGameClient.choosePlayer(Some(X)) shouldBe (ComputerPlayer(X))
      }
    }

    "Win rigged game Computer vs Computer" in {
      val out          = new ByteArrayOutputStream()
      val board: Board = Board(Row(O, Empty, X), Row(Empty, X, Empty), Row(Empty, Empty, O))
      val consoleGame =
        new ConsoleGameClient(Players(ComputerPlayer(X), ComputerPlayer(O)), gameEngine)

      Console.withOut(out) {
        consoleGame.startGame(board)
      }

      out.toString should include("Player X wins the game!!")
    }

    "Draw game with empty board Computer vs Computer" in {
      val out = new ByteArrayOutputStream()
      val board: Board =
        Board(Row(Empty, Empty, Empty), Row(Empty, Empty, Empty), Row(Empty, Empty, Empty))
      val gameService =
        new ConsoleGameClient(Players(ComputerPlayer(X), ComputerPlayer(O)), gameEngine)

      Console.withOut(out) {
        gameService.startGame(board)
      }

      out.toString should include("It is a draw!!")
    }
  }
}
