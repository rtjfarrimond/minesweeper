package minesweeper

import cats.effect.IO
import minesweeper.entity.MoveType._
import minesweeper.entity.GameStatus.{Continual, TerminalLost, TerminalWon}
import minesweeper.grid._

object Main extends App {

  val gridView = GridView.initial(GridModel.from(3, 3, 3))

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
      case FlagMove => GridView.flag(move.coordinate).run(gridView).value
      case _ => (gridView, Continual)
    }
    println(nextStateGridView)
    nextStateStatus match {
      case Continual => step(nextStateGridView)
      case TerminalLost => println("BOOM! 💣")
      case TerminalWon => println("Congratulations, you found all the mines! 😎")
    }
  }

  program.unsafeRunSync()
}
