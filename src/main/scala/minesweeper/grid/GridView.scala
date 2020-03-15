package minesweeper.grid

import cats.data.State
import minesweeper.entity.Cell
import minesweeper.entity.Cell.{HiddenCell, MineCell, NumberCell}
import minesweeper.grid.GridModel.Coordinate

case class GridView(model: GridModel, revealedState: Seq[Coordinate]) {

  val boardState: Seq[Cell] = {
    model.coordinates.map {
      case cell if !revealedState.contains(cell) => HiddenCell
      case cell if model.mineCoordinates.contains(cell) => MineCell
      case _ => NumberCell(0) // TODO: Compute the value
    }
  }

  override def toString: String =
    boardState.grouped(model.x)
      .toSeq
      .map(_.mkString("|"))
      .map(s => s"|$s|")
      .mkString("\n")

}

object GridView {

  sealed trait GameStatus extends Product with Serializable
  final case object Continual extends GameStatus
  final case object Terminal extends GameStatus

  def reveal(coordinate: Coordinate): State[GridView, GameStatus] =
    State(view => {
      val newRevealed: Seq[Coordinate] = view.revealedState :+ coordinate
      val newView = GridView(view.model, newRevealed)
      val status = if (newView.boardState.contains(MineCell)) Terminal else Continual
      (newView, status)
    })

}