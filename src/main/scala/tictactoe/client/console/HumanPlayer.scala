package tictactoe.client.console

import tictactoe.model._

case class HumanPlayer(square: Square.NonEmpty) extends Player {
  override def `type`: PlayerType = Human

  def play(board: Board): Coordinate = {
    val availableMoves = board.availableMoves()
    Client.showNextMoves(square, availableMoves)
    Client.receiveCoordinateInput(availableMoves)
  }
}
