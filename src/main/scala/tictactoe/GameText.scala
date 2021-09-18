package tictactoe

object GameText {
  val invalidInput: String = "Invalid choice, try again"
  val choosePlayer: String = "Please choose a player: X or O"
  val draw: String         = "It is a draw!!"

  def displayBoard(board: Board): String = {
    val boardDisplay =
      s"""
         |   A  B  C
         |1  ${board.row1.col1}  ${board.row1.col2}  ${board.row1.col3}
         |2  ${board.row2.col1}  ${board.row2.col2}  ${board.row2.col3}
         |3  ${board.row3.col1}  ${board.row3.col2}  ${board.row3.col3}
         |""".stripMargin

    println(boardDisplay)
    boardDisplay
  }

  def computerMove(coordinate: String): String = s"Computer played $coordinate"

  def win(square: Square): String = s"Player $square wins the game!!"

  def showNextMove(square: Square, formattedMoves: String): String =
    s"""
       |    Your next move with $square:
       |    $formattedMoves
       |""".stripMargin
}
