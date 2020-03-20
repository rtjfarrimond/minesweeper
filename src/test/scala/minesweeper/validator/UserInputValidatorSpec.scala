package minesweeper.validator

import org.scalatest.{FlatSpec, Matchers, EitherValues}

class UserInputValidatorSpec
  extends FlatSpec
  with Matchers
  with EitherValues
  with UserInputValidator {

  "validateInputLength" should "return Right(input) given input of valid length" in {
    val input = "1234" 
    validateInputLength(input).right.value shouldBe input
  }

  it should "return Left when is input is the empty String" in {
    val input = ""
    validateInputLength(input).left.value
  }

  it should "return Left when is input too short" in {
    val input = "123"
    validateInputLength(input).left.value
  }

  it should "return Left when is input too long" in {
    val input = "12345"
    validateInputLength(input).left.value
  }

  "validateMoveType" should "return Right(input) when lowercase f" in {
    val input = "f123"
    validateMoveType(input).right.value shouldBe input
  }

  it should "return Right(input) when uppercase F" in {
    val input = "F123"
    validateMoveType(input).right.value shouldBe input
  }

  "validateMoveType" should "return Right(input) when lowercase r" in {
    val input = "r123"
    validateMoveType(input).right.value shouldBe input
  }

  it should "return Right(input) when uppercase R" in {
    val input = "R123"
    validateMoveType(input).right.value shouldBe input
  }

  it should "return Left when input is empty String" in {
    val input = ""
    validateMoveType(input).left.value
  }

  "validateThirdCharacterIsComma" should "return Left when not a comma" in {
    val input = "1234"
    validateThirdCharacterIsComma(input).left.value
  }

  it should "return Left when input is the empty String" in {
    val input = ""
    validateThirdCharacterIsComma(input).left.value
  }

}
