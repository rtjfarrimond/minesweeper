package minesweeper.grid

import minesweeper.entity.Cell._
import minesweeper.entity.Coordinate
import minesweeper.entity.GameStatus.{Continual, TerminalLost, TerminalWon}
import org.scalatest.{FlatSpec, Matchers}

class GridViewSpec extends FlatSpec with Matchers {

  "reveal" should "reveal a NumberCell(0)" in {
    val gridModel = GridModel.from(1, 1, 0)
    val gridView = GridView.initial(gridModel)

    val (updatedView, _) = GridView.reveal(Coordinate(0, 0)).run(gridView).value
    updatedView.boardState.head shouldBe NumberCell(0)
  }

  it should "produce a winning end state" in {
    val gridModel = GridModel.from(1, 1, 0)
    val gridView = GridView.initial(gridModel)

    val (_, gameStatus) = GridView.reveal(Coordinate(0, 0)).run(gridView).value
    gameStatus shouldBe TerminalWon
  }

  it should "produce a losing end state" in {
    val gridModel = GridModel.from(1, 1, 1)
    val gridView = GridView.initial(gridModel)

    val (_, gameStatus) = GridView.reveal(Coordinate(0, 0)).run(gridView).value
    gameStatus shouldBe TerminalLost
  }

  it should "reveal a NumberCell with the number of adjacent mine cells" in {
    val gridModel = GridModel.from(2, 2, 4)
    val numberCellCoordinate = Coordinate(0, 0)
    val mineCoordinates = gridModel.mineCoordinates.filterNot(_ == numberCellCoordinate)
    val gridView = GridView.initial(gridModel.copy(mineCoordinates = mineCoordinates))

    val (updatedView, gameStatus) = GridView.reveal(numberCellCoordinate).run(gridView).value
    updatedView.boardState.head shouldBe NumberCell(3)
    gameStatus shouldBe Continual
  }

  it  should "reveal a MineCell" in {
    val gridModel: GridModel = GridModel.from(1, 1, 1)
    val gridView: GridView = GridView.initial(gridModel)

    val (updatedView, gameStatus) = GridView.reveal(Coordinate(0, 0)).run(gridView).value
    updatedView.boardState.head shouldBe MineCell
    gameStatus shouldBe TerminalLost
  }

  it should "reveal a MineCell from a flagged cell" in {
    val gridModel: GridModel = GridModel.from(3, 3, 9)
    val gridView: GridView = GridView.initial(gridModel)

    val (updatedView1, gameStatus1) = GridView.flag(Coordinate(0, 0)).run(gridView).value
    updatedView1.boardState.head shouldBe FlagCell
    gameStatus1 shouldBe Continual

    val (updatedView2, gameStatus2) = GridView.reveal(Coordinate(0, 0)).run(updatedView1).value
    updatedView2.boardState.head shouldBe MineCell
    gameStatus2 shouldBe TerminalLost
  }

  it should "reveal a NumberCell(0) from a flagged cell" in {
    val gridModel: GridModel = GridModel.from(2, 2, 0)
    val gridView: GridView = GridView.initial(gridModel)

    val (updatedView1, _) = GridView.flag(Coordinate(0, 0)).run(gridView).value
    updatedView1.boardState.head shouldBe FlagCell

    val (updatedView2, _) = GridView.reveal(Coordinate(0, 0)).run(updatedView1).value
    updatedView2.boardState.head shouldBe NumberCell(0)
  }

  it should "reveal the whole board when there are no mines" in {
    val gridModel: GridModel = GridModel.from(2, 2, 0)
    val gridView: GridView = GridView.initial(gridModel)

    val (updatedView1, _) = GridView.reveal(Coordinate(0, 0)).run(gridView).value
    updatedView1.boardState.forall(_ == NumberCell(0)) shouldBe true
  }

  "flag" should "place a FlagCell at the given coordinate" in new FlagContext {
    gridView.boardState.head shouldBe HiddenCell

    val (updatedView, gameStatus) = GridView.flag(Coordinate(0, 0)).run(gridView).value
    updatedView.boardState.head shouldBe FlagCell
    gameStatus shouldBe Continual
  }

  it should "revert to a hidden cell if coordinate is already flagged" in new FlagContext {
    gridView.boardState.head shouldBe HiddenCell

    val (updatedView1, gameStatus1) = GridView.flag(Coordinate(0, 0)).run(gridView).value
    updatedView1.boardState.head shouldBe FlagCell
    gameStatus1 shouldBe Continual

    val (updatedView2, gameStatus2) = GridView.flag(Coordinate(0, 0)).run(updatedView1).value
    updatedView2.boardState.head shouldBe HiddenCell
    gameStatus2 shouldBe Continual
  }

  it should "do nothing if the cell is already revealed" in {
    val gridModel: GridModel = GridModel(3, 3, Seq(Coordinate(1,1)))
    val gridView: GridView = GridView.initial(gridModel)

    val (updatedView1, gameStatus1) = GridView.reveal(Coordinate(0, 0)).run(gridView).value
    val revealed = updatedView1.boardState.head
    gameStatus1 shouldBe Continual

    val (updatedView2, gameStatus2) = GridView.flag(Coordinate(0, 0)).run(updatedView1).value
    updatedView2.boardState.head shouldBe revealed
    gameStatus2 shouldBe Continual
  }

  it should "not complete in a win state when the whole board is flagged" in new FlagContext {
    val allCoordinates: Seq[Coordinate] = for {
      x <- 0 to 3
      y <- 0 to 3
    } yield Coordinate(x, y)
    val nearlyFullyFlaggedView: GridView = gridView.copy(flaggedState = allCoordinates.tail.toSet)

    val (updatedView, gameStatus) = GridView.flag(allCoordinates.head).run(nearlyFullyFlaggedView).value
    updatedView.boardState.forall(_ == FlagCell) shouldBe true
    gameStatus shouldBe Continual
  }

  trait FlagContext {
    val gridModel: GridModel = GridModel.from(3, 3, 3)
    val gridView: GridView = GridView.initial(gridModel)
  }

}
