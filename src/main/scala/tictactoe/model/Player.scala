package tictactoe.model

import tictactoe.GameService

import scala.util.Random

sealed trait Player {
  def square: NonEmptySquare

  def `type`: PlayerType

  def play(board: Board): String
}

case class HumanPlayer(square: NonEmptySquare) extends Player {
  override def `type`: PlayerType = Human

  def play(board: Board): String = {
    val availableMoves: Iterable[String] = board.nextMoves()
    //TODO move this display away
    GameService.showNextMoves(square, availableMoves)
    GameService.receiveCoordinateInput(availableMoves)
  }
}

case class ComputerPlayer(square: NonEmptySquare) extends Player {
  private val random = new Random()

  private def randomMove(board: Board): String = {
    val availableMoves: Iterable[String] = board.nextMoves()
    val randomPosition: Int              = random.nextInt(availableMoves.size)
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
      val availableMoves = (board.nextMoves())
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

  def play(board: Board): String = {
    val minMaxMove = minimaxMove(
      board    = board,
      player   = square,
      opponent = GameService.switch(square),
      depth    = 0,
      maximize = true
    )

    minMaxMove.move.getOrElse(randomMove(board))
  }

  override def `type`: PlayerType = Computer
}
