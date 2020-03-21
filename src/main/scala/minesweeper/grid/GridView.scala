package minesweeper.grid

import cats.data.State
import minesweeper.entity.Cell
import minesweeper.entity.Cell._
import minesweeper.entity.Coordinate

case class GridView(model: GridModel, revealedState: Set[Coordinate], flaggedState: Set[Coordinate]) {

  private[grid] val boardState: Seq[Cell] = {
    model.coordinates.map {
      case cell if flaggedState.contains(cell) => FlagCell
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

  def initial(gridModel: GridModel): GridView =
    GridView(gridModel, Set.empty, Set.empty)

  def reveal(coordinate: Coordinate): State[GridView, GameStatus] =
    State(view => {
      val newView: GridView = revealCell(coordinate, view)
      val status = if (newView.boardState.contains(MineCell)) Terminal else Continual
      (newView, status)
    })

  def flag(coordinate: Coordinate): State[GridView, GameStatus] =
    State(view => {
      if (view.revealedState contains coordinate) (view, Continual)
      else (updateFlag(coordinate, view), Continual)
    })

  private def revealCell(coordinate: Coordinate, view: GridView): GridView = {
    val newRevealedState = view.revealedState incl coordinate
    val newFlaggedState = view.flaggedState excl coordinate
    val newView = view.copy(revealedState = newRevealedState, flaggedState = newFlaggedState)
    newView
  }

  private def updateFlag(coordinate: Coordinate, view: GridView): GridView = {
    val newFlaggedState =
      if (view.flaggedState.contains(coordinate)) view.flaggedState excl coordinate
      else view.flaggedState incl coordinate
    view.copy(flaggedState = newFlaggedState)
  }
}

