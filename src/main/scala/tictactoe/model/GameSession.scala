package tictactoe.model

import java.util.UUID

case class GameSession(
  id: UUID,
  players: Players,
  board: Board,
) {

  def state: GameState = {
    if (board.isGameOver) {
      board.checkWinner() match {
        case Some(winningSquare) =>
          if (players.currentPlayer.square == winningSquare) {
            GameState.Ended(GameEndedCondition.Win(players.currentPlayer))
          } else {
            GameState.Ended(GameEndedCondition.Win(players.opponent))
          }
        case None =>
          GameState.Ended(GameEndedCondition.Draw)
      }
    } else {
      GameState.InPlay
    }
  }

  def playerMove(coordinate: String, player: Player): Either[Exception, GameSession] = {
    state match {
      case GameState.InPlay =>
        if (player.square == players.currentPlayer.square) {
          if (board.availableMoves().exists(_ == coordinate)) {
            val updatedBoard = board.updateBoard(player.square, coordinate)
            Right(this.copy(board = updatedBoard))
          } else {
            Left(BoardCoordinateNotEmptyError(coordinate))
          }
        } else {
          Left(NotPlayersTurnError(player))
        }
      case ended: GameState.Ended =>
        Left(GameIsOverError(ended))
    }
  }
}

sealed trait GameState

object GameState {

  case object InPlay extends GameState

  case class Ended(condition: GameEndedCondition) extends GameState
}

sealed trait GameEndedCondition

object GameEndedCondition {
  case object Draw extends GameEndedCondition

  case class Win(winner: Player) extends GameEndedCondition
}
