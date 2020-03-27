package minesweeper.grid

import cats.data.State
import minesweeper.entity.Cell._
import minesweeper.entity.GameStatus.{Continual, TerminalLost, TerminalWon}
import minesweeper.entity.{Cell, Coordinate, GameStatus}

case class GridView(model: GridModel, revealedState: Set[Coordinate], flaggedState: Set[Coordinate]) {

  import GridView._

  private[grid] val boardState: Seq[Cell] = {
    model.coordinates.map {
      case coordinate if flaggedState.contains(coordinate) => FlagCell
      case coordinate if !revealedState.contains(coordinate) => HiddenCell
      case coordinate if model.mineCoordinates.contains(coordinate) => MineCell
      case coordinate => computeNumber(coordinate)
    }
  }

  private def computeNumber(coordinate: Coordinate): NumberCell = {
    val neighbours = getNeighbourCoordinates(coordinate)
    val mineCount = neighbours.count(model.mineCoordinates.contains)
    NumberCell(mineCount)
  }

  override def toString: String =
    boardState.grouped(model.x)
      .toSeq
      .map(_.mkString("|"))
      .map(s => s"|$s|")
      .mkString("\n")

}

object GridView {

  def initial(gridModel: GridModel): GridView =
    GridView(gridModel, Set.empty, Set.empty)

  def combine(view1: GridView, view2: GridView): GridView = {
    val revealed = view1.revealedState concat view2.revealedState
    val flagged = view1.flaggedState concat view2.flaggedState
    GridView(view1.model, revealed, flagged)
  }

  def reveal(coordinate: Coordinate): State[GridView, GameStatus] =
    State(view => {
      val newView: GridView = revealCell(coordinate, view)
      (newView, getStatus(newView))
    })

  def flag(coordinate: Coordinate): State[GridView, GameStatus] =
    State(view => {
      if (view.revealedState contains coordinate) (view, Continual)
      else {
        val newView = updateFlag(coordinate, view)
        (newView, getStatus(newView))
      }
    })

  private def revealCell(coordinate: Coordinate, view: GridView): GridView = {
    val newRevealedState = view.revealedState incl coordinate
    val newFlaggedState = view.flaggedState excl coordinate
    val newView = view.copy(revealedState = newRevealedState, flaggedState = newFlaggedState)
    if (cellWasBlank(view, newView)) revealNeighbouringBlanks(newView, coordinate)
    else newView
  }

  private def cellWasBlank(view: GridView, newView: GridView): Boolean =
    view.boardState.count(_ == NumberCell(0)) != newView.boardState.count(_ == NumberCell(0))

  private def revealNeighbouringBlanks(view: GridView, coordinate: Coordinate): GridView = {
    getNeighbourCoordinates(coordinate).map { neighbouringCoordinate =>
      revealCell(neighbouringCoordinate, view)
    }.fold(initial(view.model))(combine)
  }

  private def getNeighbourCoordinates(coordinate: Coordinate): Seq[Coordinate] = {
    val neighbours = for {
      x <- (coordinate.x - 1) to (coordinate.x + 1)
      y <- (coordinate.y - 1) to (coordinate.y + 1)
    } yield Coordinate(x, y)
    neighbours.filterNot(_ == coordinate)
  }

  private def getStatus(newView: GridView): GameStatus = {
    if (newView.boardState.contains(MineCell)) TerminalLost
    else if (newView.model.mineCoordinates.forall(newView.revealedState.contains)) TerminalWon
    else Continual
  }

  private def updateFlag(coordinate: Coordinate, view: GridView): GridView = {
    val newFlaggedState =
      if (view.flaggedState.contains(coordinate)) view.flaggedState excl coordinate
      else view.flaggedState incl coordinate
    view.copy(flaggedState = newFlaggedState)
  }
}

