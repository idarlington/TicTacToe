package tictactoe.engine

import tictactoe.model.{Board, GameSession, Player, Players}

import java.util.UUID

trait GameEngine {

  def playerMove(coordinate: String, player: Player, sessionId: UUID): Either[Exception, GameSession]

  def initGameSession(players: Players, board: Board): Either[Exception, GameSession]

  def getGameSession(id: UUID): Option[GameSession]

}
