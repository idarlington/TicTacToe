package tictactoe

import scala.annotation.tailrec
import scala.util.Random

class GameService(firstPlayer: Player, secondPlayer: Player) {
  val random = new Random()

  @tailrec
  final def receiveSquareInput(): Square = {
    scala.io.StdIn.readLine().trim.toLowerCase.strip() match {
      case "x" => X
      case "o" => O
      case _ =>
        println(GameText.invalidInput)
        receiveSquareInput()
    }
  }

  @tailrec
  final def receiveCoordinateInput(availableMoves: Iterable[String]): String = {
    val coordinate = scala.io.StdIn.readLine().trim.toUpperCase.strip()
    availableMoves.find { move =>
      move == coordinate
    } match {
      case Some(value) =>
        value
      case None =>
        println(GameText.invalidInput)
        receiveCoordinateInput(availableMoves)
    }
  }

  def showNextMoves(square: Square, moves: Iterable[String]): Unit = {
    val formattedAvailableMoves: String = moves.toSeq.sorted.foldLeft("") {
      case (moves, coordinate) => s"$moves $coordinate"
    }
    println(GameText.showNextMove(square, formattedAvailableMoves))
  }

  def switch(square: Square): Square = {
    square match {
      case X => O
      case O => X
      case Empty => Empty
    }
  }

  def choosePlayer(): Square = {
    println(GameText.choosePlayer)
    receiveSquareInput()
  }

  def updateBoard(input: Square, coordinate: String, board: Board): Board = {
    coordinate match {
      case "A1" if board.row1.col1 == Empty =>
        (board.copy(row1 = board.row1.copy(col1 = input)))
      case "A2" if board.row2.col1 == Empty =>
        (board.copy(row2 = board.row2.copy(col1 = input)))
      case "A3" if board.row3.col1 == Empty =>
        (board.copy(row3 = board.row3.copy(col1 = input)))
      case "B1" if board.row1.col2 == Empty =>
        (board.copy(row1 = board.row1.copy(col2 = input)))
      case "B2" if board.row2.col2 == Empty =>
        (board.copy(row2 = board.row2.copy(col2 = input)))
      case "B3" if board.row3.col2 == Empty =>
        (board.copy(row3 = board.row3.copy(col2 = input)))
      case "C1" if board.row1.col3 == Empty =>
        (board.copy(row1 = board.row1.copy(col3 = input)))
      case "C2" if board.row2.col3 == Empty =>
        (board.copy(row2 = board.row2.copy(col3 = input)))
      case "C3" if board.row3.col3 == Empty =>
        (board.copy(row3 = board.row3.copy(col3 = input)))
    }
  }

  def minimaxMove(
    player: Player,
    opponent: Player,
    board: Board,
    maximize: Boolean
  ): Outcome = {
    if (gameOver(board)) {
      if (checkFull(board)) {
        Outcome(board, 0)
      } else {
        if (checkWinner(player.square, board).isDefined) {
          Outcome(board, 1)
        } else {
          Outcome(board, -1)
        }
      }
    } else {
      if (maximize) {
        nextMoves(board).foldLeft((Outcome(board, -1))) {
          case (acc, current) =>
            val updatedBoard = updateBoard(player.square, current, board = board)
            val newValue = minimaxMove(
              opponent,
              player,
              updatedBoard,
              maximize = false
            ).value
            if (newValue > acc.value) Outcome(updatedBoard, newValue) else acc
        }
      } else {
        nextMoves(board).foldLeft((Outcome(board, 1))) {
          case (acc, current) =>
            val updatedBoard = updateBoard(player.square, current, board = board)
            val newValue = minimaxMove(
              opponent,
              player,
              updatedBoard,
              maximize = true
            ).value
            if (newValue < acc.value) Outcome(updatedBoard, newValue) else acc
        }
      }
    }
  }

  def nextMoves(board: Board): Iterable[String] = {
    for {
      (rowKey, row) <- board.toMap
      (colKey, square) <- row.toMap if square == Empty
    } yield s"$colKey$rowKey"
  }

  def gameOver(board: Board): Boolean = {
    if (!checkFull(board)) {
      checkWinner(firstPlayer.square, board)
        .orElse(checkWinner(secondPlayer.square, board))
        .nonEmpty
    } else {
      true
    }
  }

  def checkWinner(square: Square, board: Board): Option[Square] = {
    checkRowWinner(board)
      .orElse(checkColumnWinner(square, board))
      .orElse(checkDiagonalWinner(square, board))
  }

  def checkColumnWinner(square: Square, board: Board): Option[Square] = {
    val columnWinnerMatches: Seq[Seq[String]] =
      Seq(Seq("A1", "A2", "A3"), Seq("B1", "B2", "B3"), Seq("C1", "C2", "C3"))

    val existingSquareCoordinate = collateSquareCoordinates(square, board)
    columnWinnerMatches
      .map { win =>
        win.forall { coordinate =>
          existingSquareCoordinate.exists(_ == coordinate)
        }
      }
      .collectFirst {
        case matchAll if matchAll => square
      }
  }

  def collateSquareCoordinates(square: Square, board: Board): Iterable[String] = {
    for {
      (rowKey, row) <- board.toMap
      (colKey, existingSquare) <- row.toMap if square == existingSquare
    } yield s"$colKey$rowKey"
  }

  def checkRowWinner(board: Board): Option[Square] = {
    board.toMap
      .find {
        case (_, row) =>
          row match {
            case _ if row == Row(O, O, O) => true
            case _ if row == Row(X, X, X) => true
            case _ => false
          }
      }
      .map {
        case (_, row) =>
          row.col1
      }
  }

  def checkDiagonalWinner(square: Square, board: Board): Option[Square] = {
    val existingSquareCoordinate = collateSquareCoordinates(square, board)
    val diagonalWinnerMatches    = Seq(Seq("A1", "B2", "C3"), Seq("C1", "B2", "A3"))

    diagonalWinnerMatches
      .map { win =>
        win.forall { coordinate =>
          existingSquareCoordinate.exists(_ == coordinate)
        }
      }
      .collectFirst {
        case matchAll if matchAll => square
      }
  }

  def checkFull(board: Board): Boolean = {
    board.toMap.forall {
      case (_, row) =>
        row.toMap.forall {
          case (_, square) =>
            square != Empty
        }
    }
  }

  @tailrec
  final def gameLoop(currentPlayer: Player, opponent: Player, board: Board): Unit = {
    GameText.displayBoard(board)
    val updatedBoard: Board = currentPlayer.playerType match {
      case Human =>
        val availableMoves: Iterable[String] = nextMoves(board)
        showNextMoves(currentPlayer.square, availableMoves)
        val coordinate: String = receiveCoordinateInput(availableMoves)
        updateBoard(currentPlayer.square, coordinate, board)
      case Computer =>
        val availableMoves: Iterable[String] = nextMoves(board)
        val randomPosition: Int              = random.nextInt(availableMoves.size)
        val coordinate: String               = availableMoves.toSeq(randomPosition)

        minimaxMove(currentPlayer, opponent, board, maximize = true).board
    }

    checkWinner(currentPlayer.square, updatedBoard) match {
      case Some(square) =>
        println(GameText.win(square))
        System.exit(0)
      case None =>
        if (checkFull(updatedBoard)) {
          println(GameText.draw)
          System.exit(0)
        } else {
          gameLoop(opponent, currentPlayer, updatedBoard)
        }
    }
  }

  def startGame(board: Board): Unit = {
    gameLoop(firstPlayer, secondPlayer, board)
  }

  case class Outcome(board: Board, value: Int)
}
