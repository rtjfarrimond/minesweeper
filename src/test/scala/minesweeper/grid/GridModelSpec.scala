package minesweeper.grid

import org.scalatest.{FlatSpec, Matchers}

class GridModelSpec extends FlatSpec with Matchers {

  "GridModel" should "have a number of coordinates equal to the product of its dimensions" in {
    val x = 2
    val y = 21
    val gridModel = new GridModel(x, y, Seq.empty)
    gridModel.coordinates.length shouldBe (x * y)
  }

}
