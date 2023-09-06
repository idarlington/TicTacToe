package tictactoe.model

trait GameEngine {

  /**
    *  - check that the correct player is playing
    *  - check that the input is valid
    *
    * @return
    */
  def players: Players

  def board: Board

  def playerMove(coordinate: String, player: Player): Either[Error, Board]

  def start(): Either[Error, Unit]

}
