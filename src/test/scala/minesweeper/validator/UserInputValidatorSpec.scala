package minesweeper.validator

import minesweeper.entity.MoveType
import minesweeper.entity.MoveType.{FlagMove, RevealMove}
import minesweeper.validator.UserInputParseError.InvalidMoveType
import org.scalatest.{Assertion, EitherValues, FlatSpec, Matchers}

class UserInputValidatorSpec
  extends FlatSpec
  with Matchers
  with EitherValues
  with UserInputValidator {

  "validateMoveType" should "return a flag move from lowercase f" in {
    happyPath("f234", FlagMove)
  }

  it should "return a flag move from uppercase F" in {
    happyPath("F234", FlagMove)
  }

  it should "return a reveal move from lowercase r" in {
    happyPath("r234", RevealMove)
  }

  it should "return a reveal move from uppercase R" in {
    happyPath("R234", RevealMove)
  }

  it should "be invalid when first char does not map to a move type" in {
    val validated = validateMoveType("1234")
    validated.isValid shouldBe false
    val errorList =
      validated.toEither.left.value.toNonEmptyList.toList
    errorList should contain (InvalidMoveType)
  }

  private def happyPath(input: String, expected: MoveType): Assertion = {
    val validated = validateMoveType(input)
    validated.isValid shouldBe true
    validated.toOption.get shouldBe expected
  }

}
