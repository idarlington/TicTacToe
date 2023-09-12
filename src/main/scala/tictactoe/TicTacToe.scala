package tictactoe

import tictactoe.client.console.Client
import tictactoe.engine.InMemoryGameEngine
import tictactoe.model.Square.Empty
import tictactoe.model.{Board, Row}

object TicTacToe extends App {

  val board: Board = Board(
    Row(Empty, Empty, Empty),
    Row(Empty, Empty, Empty),
    Row(Empty, Empty, Empty)
  )

  private val gameEngine    = new InMemoryGameEngine()
  private val consoleClient = new Client(gameEngine)
  consoleClient.start(board = board)
}
