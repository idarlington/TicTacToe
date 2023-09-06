package tictactoe

import tictactoe.model.{Board, Player, Square}

object TextUtils {
  val invalidInput: String             = "Invalid choice, try again"
  val chooseSquare: String             = "Please choose a player symbol: X or O"
  val choosePlayerType: String         = "Please choose a player type: Human or Computer"
  val chooseOpponentPlayerType: String = "Please choose opponent player type: Human or Computer"
  val draw: String                     = "It is a draw!!"

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

  def displayText(text: String): Unit = {
    println(text)
  }

  def move(coordinate: String, player: Player): String =
    s"Player ${player.square} played $coordinate"

  def win(square: Square): String = s"Player $square wins the game!!"

  def showNextMove(square: Square, formattedMoves: String): String =
    s"""
       |    Your next move with $square:
       |    $formattedMoves
       |""".stripMargin
}
