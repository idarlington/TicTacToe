package tictactoe

import tictactoe.GameService.{ choosePlayer, switch }

object TicTacToe extends App {

  val board: Board = Board(
    Row(Empty, Empty, Empty),
    Row(Empty, Empty, Empty),
    Row(Empty, Empty, Empty)
  )

  private val firstPlayer  = choosePlayer(None)
  private val secondPlayer = choosePlayer(Some(switch(firstPlayer.square)))
  private val players      = Players(firstPlayer, secondPlayer)

  val gameService = new GameService(players)
  gameService.startGame(board)
}
