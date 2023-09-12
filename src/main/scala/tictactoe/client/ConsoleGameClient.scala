package tictactoe.client

import ConsoleUtils.displayText
import tictactoe.engine.GameEngine
import tictactoe.model._

import java.util.UUID
import scala.annotation.tailrec

class ConsoleGameClient(players: Players, gameEngine: GameEngine) {

  def startGame(board: Board): Unit = {
    val session = gameEngine.initGameSession(players, board)
    session match {
      case Left(error) =>
        throw error
      case Right(session) => gameLoopEngine(players.currentPlayer, session.id)
    }
  }

  private final def gameLoopEngine(currentPlayer: Player, sessionId: UUID): Unit = {

    for {
      session <- gameEngine.getGameSession(sessionId).toRight(GameSessionDoesNotExistError())

      _          = ConsoleUtils.displayBoard(session.board)
      playerMove = currentPlayer.play(session.board)

      updatedSession <- gameEngine.playerMove(playerMove, currentPlayer, sessionId)
      _ = displayText(ConsoleUtils.move(playerMove, currentPlayer))

      _ = updatedSession.state match {
        case GameState.InPlay =>
          gameLoopEngine(players.switchPlayer(currentPlayer), updatedSession.id)
        case GameState.Ended(condition) =>
          condition match {
            case GameEndedCondition.Draw =>
              println(ConsoleUtils.draw)
              ConsoleUtils.displayBoard(updatedSession.board)
            case GameEndedCondition.Win(winner) =>
              println(ConsoleUtils.win(winner.square))
              ConsoleUtils.displayBoard(updatedSession.board)
          }
      }
    } yield ()
  }
}

object ConsoleGameClient {
  import Square._

  def switch(square: NonEmpty): NonEmpty = {
    square match {
      case X => O
      case O => X
    }
  }

  @tailrec
  final def receiveSquareInput(): NonEmpty = {
    scala.io.StdIn.readLine().trim.toLowerCase().strip() match {
      case "x" => X
      case "o" => O
      case _ =>
        displayText(ConsoleUtils.invalidInput)
        receiveSquareInput()
    }
  }

  final def choosePlayer(optionalSquare: Option[NonEmpty] = None): Player = {
    val square     = optionalSquare.getOrElse(chooseSquare())
    val playerType = choosePlayerType(optionalSquare.isDefined)

    playerType match {
      case Computer => ComputerPlayer(square)
      case Human => HumanPlayer(square)
    }

  }

  private def chooseSquare(): NonEmpty = {
    displayText(ConsoleUtils.chooseSquare)
    receiveSquareInput()
  }

  @tailrec
  private final def receivePlayerType(): PlayerType = {
    scala.io.StdIn.readLine().trim.toLowerCase().strip() match {
      case "human" | "h" => Human
      case "computer" | "c" => Computer
      case _ =>
        displayText(ConsoleUtils.invalidInput)
        receivePlayerType()
    }
  }

  private def choosePlayerType(squareExists: Boolean): PlayerType = {
    if (squareExists) {
      displayText(ConsoleUtils.chooseOpponentPlayerType)
    } else {
      displayText(ConsoleUtils.choosePlayerType)
    }

    receivePlayerType()
  }

  @tailrec
  final def receiveCoordinateInput(availableMoves: Iterable[String]): String = {
    val coordinate = scala.io.StdIn.readLine().trim.toUpperCase().strip()
    availableMoves.find { move =>
      move == coordinate
    } match {
      case Some(value) =>
        value
      case None =>
        displayText(ConsoleUtils.invalidInput)
        receiveCoordinateInput(availableMoves)
    }
  }

  def showNextMoves(square: Square, moves: Iterable[String]): Unit = {
    val formattedAvailableMoves: String = moves.toSeq.sorted.foldLeft("") {
      case (moves, coordinate) => s"$moves $coordinate"
    }
    displayText(ConsoleUtils.showNextMove(square, formattedAvailableMoves))
  }
}
