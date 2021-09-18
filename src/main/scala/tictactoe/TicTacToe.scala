package tictactoe

object TicTacToe extends App {

  val board: Board =
    Board(Row(Empty, Empty, Empty), Row(Empty, Empty, Empty), Row(Empty, Empty, Empty))
  
  val gameService = new GameService(Player(X, Human), Player(O, Computer))
  gameService.startGame(board)
}
