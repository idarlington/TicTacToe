package tictactoe.model

trait Player {
  def square: Square.NonEmpty

  def `type`: PlayerType

  def play(board: Board): Coordinate
}
