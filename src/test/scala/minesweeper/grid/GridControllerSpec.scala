package minesweeper.grid

import cats.data.Validated
import minesweeper.entity.{Coordinate, Move}
import minesweeper.entity.MoveType.RevealMove
import minesweeper.validator.UserInputParseError.{CommaSplitError, InvalidMoveType}
import org.scalatest.{EitherValues, FlatSpec, Matchers}

class GridControllerSpec extends FlatSpec with Matchers with EitherValues {

  "parseUserInput" should "return a move for valid input" in {
    val input = "r0,0"
    val expected = Validated.Valid(Move(RevealMove, Coordinate(0, 0)))
    GridController.parseUserInput(input) shouldBe expected
  }

  it should "aggregate errors" in {
    val input = "12,"
    val validated = GridController.parseUserInput(input).toEither.left.value
    val errorList = validated.toNonEmptyList.toList
    println(errorList.map(_.message).mkString("\n"))
    errorList.length shouldBe 2
    errorList should contain (InvalidMoveType)
    errorList should contain (CommaSplitError)
  }

}
