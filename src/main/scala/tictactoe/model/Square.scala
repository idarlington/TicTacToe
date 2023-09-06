package tictactoe.model

trait Square

case object Empty extends Square {
  override def toString: String = "."
}

trait NonEmptySquare extends Square

case object X extends NonEmptySquare {
  override def toString: String = "X"
}

case object O extends NonEmptySquare {
  override def toString: String = "O"
}
