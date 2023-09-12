package tictactoe.model

case class Players(currentPlayer: Player, opponent: Player) {

  def switch(): Players = Players(opponent, currentPlayer)

  def switchPlayer(player: Player): Player = {
    if (player == currentPlayer) {
      opponent
    } else if (player == opponent) {
      currentPlayer
    } else {
      // TODO Rather than fail silently can we let the user know of this weird case?
      player
    }
  }
}
