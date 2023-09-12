package tictactoe.model

sealed trait PlayerType
case object Human extends PlayerType
case object Computer extends PlayerType
