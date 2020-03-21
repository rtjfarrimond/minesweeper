package minesweeper.grid

import cats.data.Validated._
import cats.implicits._
import minesweeper.entity.Move
import minesweeper.validator.UserInputValidator

object GridController extends UserInputValidator {

  @scala.annotation.tailrec
  final def getMove(): Move = {
    parseUserInput(scala.io.StdIn.readLine).toEither match {
      case Left(error) => {
        println(error.map(_.message).mkString_("\n")) // TODO: Move this side effect out
        getMove
      }
      case Right(coordinate) => coordinate
    }
  }

  def parseUserInput(input: String): ValidationResult[Move] = {
    (validateMoveType(input), validateCoordinate(input)).mapN(Move)
  }

}
