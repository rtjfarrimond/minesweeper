package minesweeper

import cats.effect.IO
import minesweeper.grid.GridView.{Continual, Terminal}
import minesweeper.grid.{GridController, GridModel, GridView}

object Main extends App {

  val gridView = GridView(GridModel.from(3, 3, 3), Seq.empty)

  val program = IO {
    println(gridView)
    step(gridView)
  }

  private def step(gridView: GridView): Unit = {
    print("> ")
    val coordinate = GridController.getMove()
    println()
    val (nextStateGridView, nextStateStatus) =
      GridView.reveal(coordinate).run(gridView).value
    println(nextStateGridView)
    nextStateStatus match {
      case Continual => step(nextStateGridView)
      case Terminal => println("BOOM!")
    }
  }

  program.unsafeRunSync()
}
