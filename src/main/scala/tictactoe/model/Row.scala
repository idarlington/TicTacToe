package tictactoe.model

import eu.timepit.refined.auto._

case class Row(col1: Square, col2: Square, col3: Square) {

  def toMap: Map[Coordinate.Column, Square] = Map(
    ("A": Coordinate.Column) -> col1,
    ("B": Coordinate.Column) -> col2,
    ("C": Coordinate.Column) -> col3
  )

  def isFilled: Boolean = {
    col1 != Square.Empty &&
    col2 != Square.Empty &&
    col3 != Square.Empty
  }
}
