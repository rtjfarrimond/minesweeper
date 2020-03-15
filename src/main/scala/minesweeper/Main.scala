package minesweeper

import minesweeper.grid.GridModel.Coordinate
import minesweeper.grid.{GridModel, GridView}
import cats.effect.IO

object Main extends App {

  val gridView = GridView(GridModel.from(2, 2, 3), Seq.empty)
  val readLn = IO(scala.io.StdIn.readLine)

  for {
    _ <- IO(println(gridView))
    n <- readLn
    _ <- IO(println(n))
    _ <- IO(println(GridView.reveal(Coordinate(1,1)).run(gridView).value._1))
  } yield ()
}
