package tictactoe

import tictactoe.GameService.{ choosePlayer, switch }
import tictactoe.model.{ Board, Empty, Players, Row }

object TicTacToe extends App {

  val board: Board = Board(
    Row(Empty, Empty, Empty),
    Row(Empty, Empty, Empty),
    Row(Empty, Empty, Empty)
  )

  private val firstPlayer  = choosePlayer()
  private val secondPlayer = choosePlayer(Some(switch(firstPlayer.square)))
  private val players      = Players(firstPlayer, secondPlayer)

  val gameService = new GameService(players)
  gameService.startGame(board)
}
