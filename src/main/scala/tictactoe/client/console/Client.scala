package tictactoe.client.console

import tictactoe.client.console.Client.choosePlayer
import tictactoe.client.console.ConsoleUtils.displayText
import tictactoe.engine.GameEngine
import tictactoe.model._

import java.util.UUID
import scala.annotation.tailrec

class Client(gameEngine: GameEngine) {
  private def createPlayers(): Players = {
    val firstPlayer  = choosePlayer()
    val secondPlayer = choosePlayer(Some(firstPlayer.square.switch()))
    Players(firstPlayer, secondPlayer)
  }

  def start(players: Players = createPlayers(), board: Board): Unit = {
    val session = gameEngine.initGameSession(players, board)
    session match {
      case Left(error) =>
        throw error
      case Right(session) => loop(players.currentPlayer, session.id)
    }
  }

  private final def loop(currentPlayer: Player, sessionId: UUID): Unit = {
    for {
      session <- gameEngine.getGameSession(sessionId).toRight(GameSessionDoesNotExistError())

      _          = ConsoleUtils.displayBoard(session.board)
      playerMove = currentPlayer.play(session.board)

      updatedSession <- gameEngine.playerMove(playerMove, currentPlayer, sessionId)
      _ = displayText(ConsoleUtils.move(playerMove, currentPlayer))

      _ = updatedSession.state match {
        case GameState.InPlay =>
          loop(updatedSession.players.currentPlayer, updatedSession.id)
        case GameState.Ended(condition) =>
          condition match {
            case GameEndedCondition.Draw =>
              displayText(ConsoleUtils.draw)
              ConsoleUtils.displayBoard(updatedSession.board)
            case GameEndedCondition.Win(winner) =>
              displayText(ConsoleUtils.win(winner.square))
              ConsoleUtils.displayBoard(updatedSession.board)
          }
      }
    } yield ()
  }
}

object Client {
  import Square._

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
  final def receiveCoordinateInput(availableMoves: Iterable[Coordinate]): Coordinate = {
    val coordinate = scala.io.StdIn.readLine().trim.toUpperCase().strip()

    toCoordinate(coordinate) match {
      case Left(error) =>
        displayText(error)
        receiveCoordinateInput(availableMoves)
      case Right(coord) =>
        availableMoves.find { move =>
          move == coord
        } match {
          case Some(_) =>
            coord
          case None =>
            displayText(ConsoleUtils.invalidInput)
            receiveCoordinateInput(availableMoves)
        }
    }
  }

  private def toCoordinate(value: String): Either[String, Coordinate] = {
    import eu.timepit.refined._

    val split = value.split("").toSeq
    val column = split.headOption
      .toRight("Column coordinate is not specified")
    val row = split.lastOption
      .flatMap(_.toIntOption)
      .toRight("Row coordinate is not specified")

    for {
      col <- column
      row <- row
      refinedCol <- refineV(col): Either[String, Coordinate.Column]
      refinedRow <- refineV(row): Either[String, Coordinate.Row]
    } yield Coordinate(refinedCol, refinedRow)
  }

  def showNextMoves(square: Square, moves: Iterable[Coordinate]): Unit = {
    val formattedAvailableMoves: String = moves.toSeq.map(_.toString).sorted.foldLeft("") {
      case (moves, coordinate) => s"$moves $coordinate"
    }
    displayText(ConsoleUtils.showNextMove(square, formattedAvailableMoves))
  }
}
