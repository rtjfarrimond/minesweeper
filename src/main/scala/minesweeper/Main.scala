package minesweeper

import cats.effect.IO
import minesweeper.entity.MoveType.RevealMove
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
    val move = GridController.getMove()
    println()
    val (nextStateGridView, nextStateStatus) = move.moveType match {
      case RevealMove => GridView.reveal(move.coordinate).run(gridView).value
      case _ => (gridView, Continual) // TODO: Implement flagging
    }
    println(nextStateGridView)
    nextStateStatus match {
      case Continual => step(nextStateGridView)
      case Terminal => println("BOOM!")
    }
  }

  program.unsafeRunSync()
}
