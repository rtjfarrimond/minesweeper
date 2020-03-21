package minesweeper

import cats.effect.IO
import cats.implicits._
import minesweeper.entity.GameStatus.{Continual, TerminalLost, TerminalWon}
import minesweeper.entity.Move
import minesweeper.entity.MoveType._
import minesweeper.grid._
import minesweeper.validator.UserInputValidator

import scala.annotation.tailrec

object Main extends App with UserInputValidator {

  // TODO: Fix bug where flagging all of the cells will win even if they are not mine cells
  val gridView = GridView.initial(GridModel.from(1, 1, 0))

  val program = IO {
    println(gridView)
    play(gridView)
  }

  program.unsafeRunSync()

  @tailrec
  private def play(gridView: GridView): Unit = {
    print("> ")
    val move = getMove()
    println()
    val (nextStateGridView, nextStateStatus) = move.moveType match {
      case RevealMove => GridView.reveal(move.coordinate).run(gridView).value
      case FlagMove => GridView.flag(move.coordinate).run(gridView).value
      case _ => (gridView, Continual)
    }
    println(nextStateGridView)
    nextStateStatus match {
      case Continual => play(nextStateGridView)
      case TerminalLost => println("BOOM! 💣")
      case TerminalWon => println("Congratulations, you found all the mines! 😎")
    }
  }

  @tailrec
  private def getMove(): Move = {
    validate(scala.io.StdIn.readLine).toEither match {
      case Left(errors) => {
        println(errors.map(_.message).mkString_("\n"))
        print("> ")
        getMove()
      }
      case Right(move) => move
    }
  }

}
