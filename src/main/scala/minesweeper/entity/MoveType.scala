 package minesweeper.entity

 sealed trait MoveType extends Product with Serializable

object MoveType {
  final case object FlagMove extends MoveType
  final case object RevealMove extends MoveType

  def apply(moveChar: Char): Option[MoveType] = moveChar.toLower match {
    case 'f' => Some(FlagMove)
    case 'r' => Some(RevealMove)
    case _ => None

  }
}

