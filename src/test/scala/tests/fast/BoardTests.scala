package tests.fast

import gobang.{BoardMut, Pos, PosColor}
import org.scalacheck.{Gen, Prop}
import tests.MyPropTest


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
        board.numberOfSameColorInDirection(pos, dir, PosColor.Black, 5) == 0 &&
          board.numberOfSameColorInDirection(pos, dir, PosColor.White, 5) == 0
      })
      checkProp(prop)
    }

    "less than maxLen" in {
      val prop = Prop.forAll(posGen, dirGen)( (pos, dir) => {
        board.numberOfSameColorInDirection(pos, dir, PosColor.Empty, 5) <= 5
      })
      checkProp(prop)
    }

    "return right number" in {
      import PosColor._
      val data = Array[PosColor] (
        Black, Empty, Empty, White,
        Black, Black, White, Empty,
        Empty, Empty, White, Empty,
        Empty, Empty, Empty, Black
      )
      val b = BoardMut.fromData(4, data)
      assert(b.numberOfSameColorInDirection(Pos(1,1), (-1,-1), Black, 5) === 1)
      assert(b.numberOfSameColorInDirection(Pos(1,1), ( 1, 1), Black, 5) === 0)
      assert(b.numberOfSameColorInDirection(Pos(0,1), ( 1, 1), White, 5) === 1)
      assert(b.numberOfSameColorInDirection(Pos(0,3), ( 1,-1), White, 5) === 1)
    }
  }

  "thisPieceEndedGame" should {
    "behave correctly" in {
      import PosColor._
      val data = Array[PosColor](
        Black, Empty, Empty, White,
        Black, Black, White, Empty,
        Empty, Empty, Black, Empty,
        Empty, Empty, Empty, Black
      )
      val b = BoardMut.fromData(4, data)

      assert(b.thisPieceEndedGame(Pos(1, 1), Black, 2))
      assert(b.thisPieceEndedGame(Pos(1, 1), Black, 4))
      assert(b.thisPieceEndedGame(Pos(3, 3), Black, 4))
      assert(b.thisPieceEndedGame(Pos(0, 3), Black, 2) === false)
      assert(b.thisPieceEndedGame(Pos(0, 3), White, 2))
      assert(b.thisPieceEndedGame(Pos(0, 3), White, 3) === false)
    }

    "pass examples" in {
      val board = BoardMut.fromString(
        """
          |.  .  .  .  .  .  .  .  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .  .  .  .  @  .  .  *
          |.  .  .  .  .  *  .  @  *  .  *  *  @  @  .
          |.  .  .  .  .  .  .  .  .  .  .  .  .  *  .
          |.  .  .  *  .  *  *  @  *  *  .  .  .  .  .
          |.  .  .  .  .  .  .  *  @  .  *  .  .  .  .
          |.  .  .  .  .  .  *  @  .  @  .  *  .  .  .
          |.  .  .  .  @  .  .  .  .  .  .  .  .  .  .
          |.  .  .  .  .  @  .  @  .  .  .  @  .  .  @
          |.  .  .  .  .  .  *  .  .  .  @  *  @  .  .
          |.  .  .  .  .  .  .  .  .  @  .  .  @  .  .
          |.  .  .  .  @  .  .  .  .  .  .  .  *  .  .
          |.  .  .  .  @  .  .  .  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .  .  .  *  .  .  .  .
        """.stripMargin
      )
      assert(board.thisPieceEndedGame(Pos(4,9), PosColor.White))
    }
  }

  "paint region" should {
    "paint right number of region" in {
      val board = BoardMut.emptyBoard(7)
      val center = Pos(3,3)
      board.placeAPiece(center, playerIsBlack = true)
      assert(board.interestRegion.size === 16)

//      board.placePiece(Pos(3,4), playerIsBlack = false)
//      assert(board.interestRegion.size === 0)
    }

    "interest region" in {
      val board = BoardMut.fromString(
        """
          |.  .  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .  .
          |.  .  .  *  .  .  .  .  .
          |.  .  .  .  @  .  .  .  .
          |.  .  .  *  .  @  .  .  .
          |.  .  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .  .
        """.stripMargin
      )
      val s1 = board.interestRegion.size
      board.placeAPiece(Pos(4,3), playerIsBlack = true)
      val s2 = board.interestRegion.size
      assert(s2 === s1 + 4 - 1)
    }

  }
}
