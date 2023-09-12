package tictactoe.model

import java.util.UUID

sealed abstract class GameEngineError(message: String) extends Exception(message)

case class GameSessionDoesNotExistError() extends GameEngineError("Game session does not exist")
case class GameSessionExistsError(id: UUID)
    extends GameEngineError(s"Game session already exists with ID: $id")
case class GameSessionUpdateFailedError() extends GameEngineError("Failed to update game session")

case class GameIsOverError(ended: GameState.Ended) extends GameEngineError("Game is already over")
case class BoardCoordinateNotEmptyError(coordinate: String)
    extends GameEngineError(s"Board coordinate $coordinate is not empty")
case class NotPlayersTurnError(player: Player) extends GameEngineError(s"It is not $player's turn")
