package tictactoe.model

import tictactoe.client.ConsoleGameClient

sealed trait PlayerType
case object Human extends PlayerType
case object Computer extends PlayerType
