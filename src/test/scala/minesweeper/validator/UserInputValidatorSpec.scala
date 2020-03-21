package minesweeper.validator

import cats.data.Validated
import minesweeper.entity.MoveType.{FlagMove, RevealMove}
import minesweeper.entity.{Coordinate, Move, MoveType}
import minesweeper.validator.UserInputParseError.{CommaSplitError, InvalidMoveType}
import org.scalatest.{Assertion, EitherValues, FlatSpec, Matchers}

class UserInputValidatorSpec
  extends FlatSpec
  with Matchers
  with EitherValues
  with UserInputValidator {

  "validateMoveType" should "return a flag move from lowercase f" in {
    happyPath("f2,4", FlagMove)
  }

  it should "return a flag move from uppercase F" in {
    happyPath("F2,4", FlagMove)
  }

  it should "return a reveal move from lowercase r" in {
    happyPath("r2,4", RevealMove)
  }

  it should "return a reveal move from uppercase R" in {
    happyPath("R2,4", RevealMove)
  }

  it should "be invalid when first char does not map to a move type" in {
    val validated = validate("1234")
    validated.isValid shouldBe false
    val errorList =
      validated.toEither.left.value.toNonEmptyList.toList
    errorList should contain (InvalidMoveType)
  }

  "validate" should "return a move for valid input" in {
    val input = "r0,0"
    val expected = Validated.Valid(Move(RevealMove, Coordinate(0, 0)))
    validate(input) shouldBe expected
  }

  it should "aggregate errors" in {
    val input = "12,"
    val validated = validate(input).toEither.left.value
    val errorList = validated.toNonEmptyList.toList
    errorList.length shouldBe 2
    errorList should contain (InvalidMoveType)
    errorList should contain (CommaSplitError)
  }

  private def happyPath(input: String, expected: MoveType): Assertion = {
    val validated = validate(input)
    validated.isValid shouldBe true
    validated.toOption.get.moveType shouldBe expected
  }
}

