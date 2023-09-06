package tictactoe.model

import tictactoe.GameService

sealed trait PlayerType
case object Human extends PlayerType
case object Computer extends PlayerType
