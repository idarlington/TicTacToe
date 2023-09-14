package tictactoe.model

import scala.util.Random

case class MinMaxOutcome(score: Int, move: Option[Coordinate])

case class ComputerPlayer(square: Square.NonEmpty) extends Player {
  private val random = new Random()

  override def `type`: PlayerType = Computer

  def play(board: Board): Coordinate = {
    val minMaxMove = minimaxMove(
      board    = board,
      player   = square,
      opponent = square.switch(),
      depth    = 0,
      maximize = true
    )

    minMaxMove.move.getOrElse(randomMove(board))
  }

  private def randomMove(board: Board): Coordinate = {
    val availableMoves: Iterable[Coordinate] = board.availableMoves()
    val randomPosition: Int                  = random.nextInt(availableMoves.size)
    availableMoves.toSeq(randomPosition)
  }

  private final def minimaxMove(
    player: Square,
    opponent: Square,
    board: Board,
    depth: Int,
    maximize: Boolean
  ): MinMaxOutcome = {

    if (board.isGameOver) {
      board match {
        case _ if (board.isDraw) => MinMaxOutcome(0, None)
        case _ if (board.checkWinner().contains(player)) => MinMaxOutcome(10 - depth, None)
        case _ => MinMaxOutcome(depth - 10, None)
      }
    } else {
      val availableMoves = board.availableMoves()
      if (maximize) {
        availableMoves.foldLeft(MinMaxOutcome(-10, None)) {
          case (acc, move) =>
            val updatedBoard = board.updateBoard(player, move)
            val gameValue = minimaxMove(
              player   = player,
              opponent = opponent,
              board    = updatedBoard,
              depth    = depth + 1,
              maximize = false
            ).score
            if (gameValue > acc.score) MinMaxOutcome(gameValue, Some(move))
            else acc
        }
      } else {
        availableMoves.foldLeft(MinMaxOutcome(10, None)) {
          case (acc, move) =>
            val updatedBoard = board.updateBoard(opponent, move)
            val gameValue =
              minimaxMove(
                player   = player,
                opponent = opponent,
                board    = updatedBoard,
                depth    = depth + 1,
                maximize = true
              ).score
            if (gameValue < acc.score) MinMaxOutcome(gameValue, Some(move))
            else acc
        }
      }
    }
  }

}
