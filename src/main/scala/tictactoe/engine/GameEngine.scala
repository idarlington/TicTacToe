package tictactoe.engine

import tictactoe.model.{Board, Coordinate, GameSession, Player, Players}

import java.util.UUID

trait GameEngine {

  def playerMove(coordinate: Coordinate, player: Player, sessionId: UUID): Either[Exception, GameSession]

  def initGameSession(players: Players, board: Board): Either[Exception, GameSession]

  def getGameSession(id: UUID): Option[GameSession]

}
