package minesweeper.entity

sealed trait GameStatus extends Product with Serializable

object GameStatus {
  final case object Continual extends GameStatus
  final case object TerminalLost extends GameStatus
  final case object TerminalWon extends GameStatus
}

