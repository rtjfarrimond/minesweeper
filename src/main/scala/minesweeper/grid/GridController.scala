package minesweeper.grid

import minesweeper.grid.GridModel.Coordinate

object GridController {
  final case class UserInputParseError(message: String)

  @scala.annotation.tailrec
  final def getMove(): Coordinate = {
    parseUserInput(scala.io.StdIn.readLine) match {
      case Left(error) => {
        println(error.message)
        getMove
      }
      case Right(coordinate) => coordinate
    }
  }

  private def parseUserInput(
    input: String
  ): Either[UserInputParseError, Coordinate] = {
    try {
      val split = input.split(',')
      if (split.length != 2)
        Left(UserInputParseError("Input did not match format 'x,y'"))
      else {
        val y = split(0).toInt
        val x = split(1).toInt
        Right(Coordinate(x, y))
      }
    } catch {
      case _: NumberFormatException =>
        Left(UserInputParseError("Could not parse x or y coordinate."))
      case _: Throwable =>
        Left(UserInputParseError("Unexpected parsing error occurred."))
    }
  }

}
