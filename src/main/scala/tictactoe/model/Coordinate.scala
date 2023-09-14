package tictactoe.model

import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric._
import eu.timepit.refined.string.MatchesRegex

case class Coordinate(column: Coordinate.Column, row: Coordinate.Row) {
  override def toString: String = s"$column$row"
}

object Coordinate {
  type Row    = Int Refined Interval.Closed[1, 3]
  type Column = String Refined MatchesRegex["[A-C]"]
}
