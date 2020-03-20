package minesweeper.validator

trait UserInputValidator {

  type ValidatedInput = Either[UserInputParseError, String]

  final case class UserInputParseError(message: String)

  private val ErrorMessage = "Input must match the format {F,R}x,y"

  def validateInputLength(input: String): ValidatedInput = {
    val validLength = 4
    if (input.length == validLength) Right(input)
    else Left(UserInputParseError(
      f"Input $input was not $validLength long.\n$ErrorMessage")
    )
  }

  def validateMoveType(input: String): ValidatedInput = {
    val validMoveTypes = "fr"
    input.headOption.map(_.toLower) match {
      case Some(c) if validMoveTypes.contains(c) => Right(input)
      case _ => Left(UserInputParseError("First character of input must be either F or R to indicate a Flag or a Reveal type move"))
    }
  }

  def validateThirdCharacterIsComma(input: String): ValidatedInput =
    if (input == "" || input(2) != ',') Left(UserInputParseError(
      "The x and y coordinates must be seperated by a comma"))
    else Right(input)

}

