package tictactoe.model

trait Square

object Square {
  case object Empty extends Square {
    override def toString: String = "."
  }

  trait NonEmpty extends Square {

    def switch(): NonEmpty = {
      this match {
        case X => O
        case O => X
      }
    }
  }

  case object X extends NonEmpty {
    override def toString: String = "X"
  }

  case object O extends NonEmpty {
    override def toString: String = "O"
  }
}
