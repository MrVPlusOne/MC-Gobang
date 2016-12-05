package tests

import gobang.{BoardMut, Pos, PosState}
import org.scalacheck.{Gen, Prop}


/**
  * Created by weijiayi on 05/12/2016.
  */
class BoardTests extends MyPropTest{
  "numberOfSameColorInDirection" should {
    val size = 10
    val posGen =
      for(r <- Gen.choose(0, size-1); c <- Gen.choose(0, size-1)) yield Pos(r, c)

    val dirGen = for(r <- Gen.oneOf(Seq(1,-1)); c <- Gen.oneOf(Seq(-1, 1))) yield (r, c)

    val board = BoardMut.emptyBoard(size)

    "return 0" in {
      val prop = Prop.forAll(posGen, dirGen)( (pos, dir) => {
        board.numberOfSameColorInDirection(pos, dir, PosState.Black, 5) == 0 &&
          board.numberOfSameColorInDirection(pos, dir, PosState.White, 5) == 0
      })
      checkProp(prop)
    }

    "less than maxLen" in {
      val prop = Prop.forAll(posGen, dirGen)( (pos, dir) => {
        board.numberOfSameColorInDirection(pos, dir, PosState.Empty, 5) <= 5
      })
      checkProp(prop)
    }

    "return right number" in {
      import PosState._
      val data = Array[PosState] (
        Black, Empty, Empty, White,
        Black, Black, White, Empty,
        Empty, Empty, White, Empty,
        Empty, Empty, Empty, Black
      )
      val b = new BoardMut(data, 4)
      assert(b.numberOfSameColorInDirection(Pos(1,1), (-1,-1), Black, 5) === 2)
      assert(b.numberOfSameColorInDirection(Pos(1,1), ( 1, 1), Black, 5) === 1)
      assert(b.numberOfSameColorInDirection(Pos(0,1), ( 1, 1), White, 5) === 0)
      assert(b.numberOfSameColorInDirection(Pos(0,3), (-1, 1), White, 5) === 2)
    }
  }

  "thisPieceEndedGame" should {
    "behave correctly" in {
      import PosState._
      val data = Array[PosState](
        Black, Empty, Empty, White,
        Black, Black, White, Empty,
        Empty, Empty, Black, Empty,
        Empty, Empty, Empty, Black
      )
      val b = new BoardMut(data, 4)

      assert(b.thisPieceEndedGame(Pos(1, 1), Black, 2))
      assert(b.thisPieceEndedGame(Pos(1, 1), Black, 4))
      assert(b.thisPieceEndedGame(Pos(3, 3), Black, 4))
      assert(b.thisPieceEndedGame(Pos(0, 3), Black, 1) === false)
      assert(b.thisPieceEndedGame(Pos(0, 3), White, 2))
      assert(b.thisPieceEndedGame(Pos(0, 3), White, 3) === false)
    }
  }
}
