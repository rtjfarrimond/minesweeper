package minesweeper.entity

trait Cell extends Product with Serializable

object Cell {

  final case object FlagCell extends Cell {
    override def toString: String = "F"
  }

  final case object HiddenCell extends Cell {
    override def toString: String = " "
  }

  final case object MineCell extends Cell {
    override def toString: String = "*"
  }

  final case class NumberCell(value: Int) extends Cell {
    override def toString: String = s"${if (value == 0) "." else value}"
  }

}
