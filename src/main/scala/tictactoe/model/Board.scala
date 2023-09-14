package tictactoe.model

import eu.timepit.refined.auto._

case class Board(row1: Row, row2: Row, row3: Row) {
  import Square._
  private def toMap: Map[Coordinate.Row, Row] = Map(
    (1: Coordinate.Row) -> row1,
    (2: Coordinate.Row) -> row2,
    (3: Coordinate.Row) -> row3
  )

  def availableMoves(): Iterable[Coordinate] = {
    for {
      (rowCoord, row) <- toMap
      (colCoord, square) <- row.toMap if square == Empty
    } yield Coordinate(column = colCoord, row = rowCoord)
  }

  def updateBoard(input: Square, coordinate: Coordinate): Board = {
    coordinate.toString match {
      case "A1" if this.row1.col1 == Empty =>
        this.copy(row1 = this.row1.copy(col1 = input))
      case "A2" if this.row2.col1 == Empty =>
        this.copy(row2 = this.row2.copy(col1 = input))
      case "A3" if this.row3.col1 == Empty =>
        this.copy(row3 = this.row3.copy(col1 = input))
      case "B1" if this.row1.col2 == Empty =>
        this.copy(row1 = this.row1.copy(col2 = input))
      case "B2" if this.row2.col2 == Empty =>
        this.copy(row2 = this.row2.copy(col2 = input))
      case "B3" if this.row3.col2 == Empty =>
        this.copy(row3 = this.row3.copy(col2 = input))
      case "C1" if this.row1.col3 == Empty =>
        this.copy(row1 = this.row1.copy(col3 = input))
      case "C2" if this.row2.col3 == Empty =>
        this.copy(row2 = this.row2.copy(col3 = input))
      case "C3" if this.row3.col3 == Empty =>
        this.copy(row3 = this.row3.copy(col3 = input))
    }
  }

  def isGameOver: Boolean = {
    isDraw || checkWinner().isDefined
  }

  def isDraw: Boolean = {
    toMap.forall {
      case (_, row) =>
        row.isFilled
    }
  }

  def checkWinner(): Option[Square] = {
    checkRowWinner()
      .orElse(checkColumnWinner())
      .orElse(checkDiagonalWinner())
  }

  private def checkColumnWinner(): Option[Square] = {
    def checkColumnWin(square: Square, coordinates: Iterable[String]): Option[Square] = {
      val columnWinnerMatches: Seq[Seq[String]] = Seq(
        Seq("A1", "A2", "A3"),
        Seq("B1", "B2", "B3"),
        Seq("C1", "C2", "C3")
      )
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

    val xSquareCoordinates = collateSquareCoordinates(X)
    val oSquareCoordinates = collateSquareCoordinates(O)

    checkColumnWin(X, xSquareCoordinates).orElse(checkColumnWin(O, oSquareCoordinates))
  }

  private def checkDiagonalWinner(): Option[Square] = {
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

    val xSquareCoordinate = collateSquareCoordinates(X)
    val oSquareCoordinate = collateSquareCoordinates(O)

    checkDiagonalWin(X, xSquareCoordinate).orElse(checkDiagonalWin(O, oSquareCoordinate))
  }

  private def collateSquareCoordinates(square: Square): Iterable[String] = {
    for {
      (rowKey, row) <- toMap
      (colKey, existingSquare) <- row.toMap if square == existingSquare
    } yield s"$colKey$rowKey"
  }

  private def checkRowWinner(): Option[Square] = {
    toMap
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
}
