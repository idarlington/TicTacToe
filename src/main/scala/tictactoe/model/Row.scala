package tictactoe.model

case class Row(col1: Square, col2: Square, col3: Square) {
  def toMap: Map[String, Square] = Map("A" -> col1, "B" -> col2, "C" -> col3)

  def isFilled: Boolean = {
    col1 != Square.Empty &&
    col2 != Square.Empty &&
    col3 != Square.Empty
  }
}
