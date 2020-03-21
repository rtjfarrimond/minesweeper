package minesweeper.validator

import cats.data._
import cats.implicits._
import minesweeper.entity.Coordinate
import minesweeper.entity.MoveType

trait UserInputValidator {
  import UserInputParseError._

  type ValidationResult[A] = ValidatedNec[UserInputParseError, A]

  def validateMoveType(input: String): ValidationResult[MoveType] = {
    val moveTypeOption: Option[MoveType] = input.headOption
      .map(_.toLower)
      .flatMap(c => MoveType(c))
    moveTypeOption match {
      case Some(moveType) => moveType.validNec
      case _ => InvalidMoveType.invalidNec
    }
  }

  def validateCoordinate(input: String): ValidationResult[Coordinate] = {
    try {
      val split = input.tail.split(',')
      if (split.length != 2)
        CommaSplitError.invalidNec
      else {
        val y = split(0).toInt
        val x = split(1).toInt
        Coordinate(x, y).validNec
      }
    } catch {
      case _: NumberFormatException =>
        CoordinateParseError.invalidNec
      case _: Throwable =>
        UnexpectedParsingError.invalidNec
    }
  }
}

sealed trait UserInputParseError extends Product with Serializable {
  def message: String
}

object UserInputParseError {

  final case object InvalidMoveType extends UserInputParseError {
    override val message: String =
      "First character of input must be either F or R to indicate a Flag or a Reveal type move."
  }

  final case object CommaSplitError extends UserInputParseError {
    override val message: String =
      "The x and y coordinates must be seperated by a comma."
  }

  final case object CoordinateParseError extends UserInputParseError {
    override val message: String = "Could not parse x or y coordinate."
  }

  final case object UnexpectedParsingError extends UserInputParseError {
    override val message: String =
      "An unexpected error occurred whilst parsing user input."
  }

}

