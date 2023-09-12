package tictactoe.model

case class Players(currentPlayer: Player, opponent: Player) {

  def switch(): Players = Players(opponent, currentPlayer)
}
