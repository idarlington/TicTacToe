package tictactoe

import tictactoe.client.ConsoleGameClient
import tictactoe.client.ConsoleGameClient.{choosePlayer, switch}
import tictactoe.engine.InMemoryGameEngine
import tictactoe.model.Square.Empty
import tictactoe.model.{Board, Players, Row}

object TicTacToe extends App {

  val board: Board = Board(
    Row(Empty, Empty, Empty),
    Row(Empty, Empty, Empty),
    Row(Empty, Empty, Empty)
  )

  private val firstPlayer  = choosePlayer()
  private val secondPlayer = choosePlayer(Some(switch(firstPlayer.square)))
  private val players      = Players(firstPlayer, secondPlayer)

  val gameEngine  = new InMemoryGameEngine()
  val gameService = new ConsoleGameClient(players, gameEngine)
  gameService.startGame(board)
}
