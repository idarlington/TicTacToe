package tictactoe.engine

import tictactoe.model._

import java.util.UUID
import java.util.concurrent.ConcurrentHashMap
import scala.collection.concurrent
import scala.jdk.CollectionConverters._

class InMemoryGameEngine() extends GameEngine {

  private val engineStorage: concurrent.Map[UUID, GameSession] =
    new ConcurrentHashMap[UUID, GameSession]().asScala

  override def playerMove(
    coordinate: String,
    player: Player,
    sessionId: UUID
  ): Either[Exception, GameSession] = {
    for {
      gameSession <- engineStorage.get(sessionId).toRight(GameSessionDoesNotExistError())

      updatedGameSession <- gameSession
        .playerMove(coordinate, player)
        .map(_.copy(players = gameSession.players.switch()))

      replaced = engineStorage.replace(gameSession.id, gameSession, updatedGameSession)
      update <- if (replaced) {
        Right(updatedGameSession)
      } else {
        Left(GameSessionUpdateFailedError())
      }

    } yield update
  }

  override def initGameSession(
    players: Players,
    board: Board
  ): Either[GameEngineError, GameSession] = {
    val sessionId: UUID      = UUID.randomUUID()
    val session: GameSession = GameSession(id = sessionId, players = players, board = board)
    val action               = engineStorage.putIfAbsent(session.id, session)

    action.toLeft(session).left.map(game => GameSessionExistsError(game.id))
  }

  override def getGameSession(id: UUID): Option[GameSession] =
    engineStorage.get(id)
}
