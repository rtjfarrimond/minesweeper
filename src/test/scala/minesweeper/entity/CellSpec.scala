package minesweeper.entity

import minesweeper.entity.Cell.NumberCell
import org.scalatest.{FlatSpec, Matchers}

class CellSpec extends FlatSpec with Matchers {

  "NumberCell.toString" should "be period when the value is 0" in {
    val numberCell = NumberCell(value = 0)
    numberCell.toString shouldBe "."
  }

  it should "contain the value when it is > 0" in {
    val value = 1
    val numberCell = NumberCell(value = value)
    numberCell.toString shouldBe s"$value"
  }

}
