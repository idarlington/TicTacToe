package tictactoe

import tictactoe.TextUtils.displayText
import tictactoe.model._

import scala.annotation.tailrec

class GameService(players: Players) {

  def startGame(board: Board): Unit = {
    gameLoop(players.currentPlayer, players.opponent, board)
  }

  @tailrec
  private final def gameLoop(currentPlayer: Player, opponent: Player, board: Board): Unit = {
    TextUtils.displayBoard(board)

    val playerMove: String = currentPlayer.play(board)
    displayText(TextUtils.move(playerMove, currentPlayer))
    val updatedBoard = board.updateBoard(currentPlayer.square, playerMove)

    updatedBoard.checkWinner() match {
      case Some(square) =>
        println(TextUtils.win(square))
        TextUtils.displayBoard(updatedBoard)
      case None =>
        if (updatedBoard.isDraw) {
          println(TextUtils.draw)
          TextUtils.displayBoard(updatedBoard)
        } else {
          gameLoop(
            currentPlayer = opponent,
            opponent      = currentPlayer,
            board         = updatedBoard
          )
        }
    }
  }
}

object GameService {

  def switch(square: NonEmptySquare): NonEmptySquare = {
    square match {
      case X => O
      case O => X
    }
  }

  @tailrec
  final def receiveSquareInput(): NonEmptySquare = {
    scala.io.StdIn.readLine().trim.toLowerCase().strip() match {
      case "x" => X
      case "o" => O
      case _ =>
        displayText(TextUtils.invalidInput)
        receiveSquareInput()
    }
  }

  final def choosePlayer(optionalSquare: Option[NonEmptySquare] = None): Player = {
    val square     = optionalSquare.getOrElse(chooseSquare())
    val playerType = choosePlayerType(optionalSquare.isDefined)

    playerType match {
      case Computer => ComputerPlayer(square)
      case Human => HumanPlayer(square)
    }

  }

  private def chooseSquare(): NonEmptySquare = {
    displayText(TextUtils.chooseSquare)
    receiveSquareInput()
  }

  @tailrec
  private final def receivePlayerType(): PlayerType = {
    scala.io.StdIn.readLine().trim.toLowerCase().strip() match {
      case "human" | "h" => Human
      case "computer" | "c" => Computer
      case _ =>
        displayText(TextUtils.invalidInput)
        receivePlayerType()
    }
  }

  private def choosePlayerType(squareExists: Boolean): PlayerType = {
    if (squareExists) {
      displayText(TextUtils.chooseOpponentPlayerType)
    } else {
      displayText(TextUtils.choosePlayerType)
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
        displayText(TextUtils.invalidInput)
        receiveCoordinateInput(availableMoves)
    }
  }

  def showNextMoves(square: Square, moves: Iterable[String]): Unit = {
    val formattedAvailableMoves: String = moves.toSeq.sorted.foldLeft("") {
      case (moves, coordinate) => s"$moves $coordinate"
    }
    displayText(TextUtils.showNextMove(square, formattedAvailableMoves))
  }
}
