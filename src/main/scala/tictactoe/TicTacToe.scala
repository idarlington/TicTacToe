package tictactoe

object TicTacToe extends App {

  val board: Board =
    Board(Row(Empty, Empty, Empty), Row(Empty, Empty, Empty), Row(Empty, Empty, Empty))

  GameService.startGame(board)
}
