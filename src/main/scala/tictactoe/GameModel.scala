package tictactoe

trait Square

case object X extends Square {
  override def toString: String = "X"
}

case object O extends Square {
  override def toString: String = "O"
}

case object Empty extends Square {
  override def toString: String = "."
}

case class Row(col1: Square, col2: Square, col3: Square) {
  def toMap: Map[String, Square] = Map("A" -> col1, "B" -> col2, "C" -> col3)
}

case class Board(row1: Row, row2: Row, row3: Row) {
  def toMap: Map[String, Row] = Map("1" -> row1, "2" -> row2, "3" -> row3)
}

trait PlayerType
case object Human extends PlayerType
case object Computer extends PlayerType

case class Player(square: Square, playerType: PlayerType)
