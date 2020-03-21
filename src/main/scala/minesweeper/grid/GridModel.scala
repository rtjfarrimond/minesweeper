package minesweeper.grid

import minesweeper.entity.Coordinate

case class GridModel(x: Int, y: Int, mineCoordinates: Seq[Coordinate]) {
  import GridModel._
  val coordinates: Seq[Coordinate] = fillCoordinates(x, y)
}

object GridModel {

  private def fillCoordinates(x: Int, y: Int): Seq[Coordinate] =
    for {
      xCoord <- 0 until x
      yCoord <- 0 until y
    } yield Coordinate(xCoord, yCoord)

  def from(x: Int, y: Int, nMines: Int): GridModel = {
    val mines: Seq[Coordinate] = scala.util.Random.shuffle(
      fillCoordinates(x, y)
    ).take(nMines)
    new GridModel(x, y, mines)
  }
}
