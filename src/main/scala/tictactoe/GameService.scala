package tictactoe

import scala.annotation.tailrec
import scala.util.Random

class GameService(players: Players) {
  private val random = new Random()

  @tailrec
  final def receiveCoordinateInput(availableMoves: Iterable[String]): String = {
    val coordinate = scala.io.StdIn.readLine().trim.toUpperCase().strip()
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

  private def showNextMoves(square: Square, moves: Iterable[String]): Unit = {
    val formattedAvailableMoves: String = moves.toSeq.sorted.foldLeft("") {
      case (moves, coordinate) => s"$moves $coordinate"
    }
    println(GameText.showNextMove(square, formattedAvailableMoves))
  }

  def randomMove(player: Player, board: Board): Board = {
    val availableMoves: Iterable[String] = nextMoves(board)
    val randomPosition: Int              = random.nextInt(availableMoves.size)
    val coordinate: String               = availableMoves.toSeq(randomPosition)
    updateBoard(player.square, coordinate, board)
  }

  def nextMoves(board: Board): Iterable[String] = {
    for {
      (rowKey, row) <- board.toMap
      (colKey, square) <- row.toMap if square == Empty
    } yield s"$colKey$rowKey"
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

  private final def minimaxMove(
    player: Player,
    opponent: Player,
    board: Board,
    depth: Int = 0,
    maximize: Boolean
  ): Outcome = {

    if (gameOver(board)) {
      board match {
        case _ if (checkDraw(board)) => Outcome(board, 0, None)
        case _ if checkWinner(board).contains(player.square) => Outcome(board, 10 - depth, None)
        case _ => Outcome(board, depth - 10, None)
      }
    } else {
      if (maximize) {
        nextMoves(board).foldLeft((Outcome(board, -10, None))) {
          case (acc, move) =>
            val updatedBoard = updateBoard(player.square, move, board = board)
            val gameValue = minimaxMove(
              player   = player,
              opponent = opponent,
              board    = updatedBoard,
              depth    = depth + 1,
              maximize = false
            ).score
            if (gameValue > acc.score) Outcome(updatedBoard, gameValue, Some(move))
            else acc
        }
      } else {
        nextMoves(board).foldLeft((Outcome(board, 10, None))) {
          case (acc, move) =>
            val updatedBoard = updateBoard(opponent.square, move, board = board)
            val gameValue =
              minimaxMove(
                player   = player,
                opponent = opponent,
                board    = updatedBoard,
                depth    = depth + 1,
                maximize = true
              ).score
            if (gameValue < acc.score) Outcome(updatedBoard, gameValue, Some(move))
            else acc
        }
      }
    }
  }

  private def gameOver(board: Board): Boolean = {
    checkDraw(board) || checkWinner(board).isDefined
  }

  private def checkWinner(board: Board): Option[Square] = {
    checkRowWinner(board)
      .orElse(checkColumnWinner(board))
      .orElse(checkDiagonalWinner(board))
  }

  def checkColumnWinner(board: Board): Option[Square] = {
    def checkColumnWin(square: Square, coordinates: Iterable[String]): Option[Square] = {
      val columnWinnerMatches: Seq[Seq[String]] =
        Seq(Seq("A1", "A2", "A3"), Seq("B1", "B2", "B3"), Seq("C1", "C2", "C3"))

      columnWinnerMatches
        .map { win =>
          win.forall { coordinate =>
            coordinates.exists(_ == coordinate)
          }
        }
        .collectFirst {
          case true => square
        }
    }

    val xSquareCoordinates = collateSquareCoordinates(X, board)
    val oSquareCoordinates = collateSquareCoordinates(O, board)

    checkColumnWin(X, xSquareCoordinates).orElse(checkColumnWin(O, oSquareCoordinates))
  }

  private def collateSquareCoordinates(square: Square, board: Board): Iterable[String] = {
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

  def checkDiagonalWinner(board: Board): Option[Square] = {
    def checkDiagonalWin(square: Square, coordinates: Iterable[String]): Option[Square] = {
      val diagonalWinnerMatches = Seq(Seq("A1", "B2", "C3"), Seq("C1", "B2", "A3"))
      diagonalWinnerMatches
        .map { win =>
          win.forall { coordinate =>
            coordinates.exists(_ == coordinate)
          }
        }
        .collectFirst {
          case true => square
        }
    }

    val xSquareCoordinate = collateSquareCoordinates(X, board)
    val oSquareCoordinate = collateSquareCoordinates(O, board)

    checkDiagonalWin(X, xSquareCoordinate).orElse(checkDiagonalWin(O, oSquareCoordinate))

  }

  def checkDraw(board: Board): Boolean = {
    board.toMap.forall {
      case (_, row) =>
        row.toMap.forall {
          case (_, square) =>
            square != Empty
        }
    }
  }

  @tailrec
  private final def gameLoop(currentPlayer: Player, opponent: Player, board: Board): Unit = {
    GameText.displayBoard(board)
    val updatedBoard: Board = currentPlayer.playerType match {
      case Human =>
        val availableMoves: Iterable[String] = nextMoves(board)
        showNextMoves(currentPlayer.square, availableMoves)
        val coordinate: String = receiveCoordinateInput(availableMoves)
        println(GameText.move(coordinate, currentPlayer))
        updateBoard(currentPlayer.square, coordinate, board)
      case Computer =>
        val outCome: Outcome =
          minimaxMove(
            board    = board,
            player   = currentPlayer,
            opponent = opponent,
            maximize = true
          )
        outCome.move.collect { coordinate =>
          println(GameText.move(coordinate, currentPlayer))
        }
        outCome.board
    }

    checkWinner(updatedBoard) match {
      case Some(square) =>
        println(GameText.win(square))
        GameText.displayBoard(updatedBoard)
        System.exit(0)
      case None =>
        if (checkDraw(updatedBoard)) {
          println(GameText.draw)
          GameText.displayBoard(updatedBoard)
          System.exit(0)
        } else {
          gameLoop(
            currentPlayer = opponent,
            opponent      = currentPlayer,
            board         = updatedBoard
          )
        }
    }
  }

  def startGame(board: Board): Unit = {
    gameLoop(players.currentPlayer, players.opponent, board)
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
        println(GameText.invalidInput)
        receiveSquareInput()
    }
  }

  private def chooseSquare(): NonEmptySquare = {
    println(GameText.chooseSquare)
    receiveSquareInput()
  }

  @tailrec
  private final def receivePlayerType(): PlayerType = {
    scala.io.StdIn.readLine().trim.toLowerCase().strip() match {
      case "human" | "h" => Human
      case "computer" | "c" => Computer
      case _ =>
        println(GameText.invalidInput)
        receivePlayerType()
    }
  }

  private def choosePlayerType(squareExists: Boolean): PlayerType = {
    if (squareExists) {
      println(GameText.chooseOpponentPlayerType)
    } else {
      println(GameText.choosePlayerType)
    }

    receivePlayerType()
  }

  final def choosePlayer(optionalSquare: Option[NonEmptySquare]): Player = {
    val square     = optionalSquare.getOrElse(chooseSquare())
    val playerType = choosePlayerType(optionalSquare.isDefined)

    Player(square, playerType)
  }
}
