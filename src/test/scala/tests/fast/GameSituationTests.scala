package tests.fast

import gobang.{BoardMut, Pos, PosColor}
import tests.MyPropTest

/**
  * Created by weijiayi on 07/12/2016.
  */
class GameSituationTests extends MyPropTest{
  "Victory points statistics" should {

    // 'four' means 'victory point'
    "have zero four" in {
      val board = BoardMut.fromString(
        """
          |.  .  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .  .
          |.  .  .  *  .  .  .  .  .
          |.  .  .  .  @  .  .  .  .
          |.  .  .  *  .  @  .  .  .
          |.  .  .  .  .  .  @  .  .
          |.  .  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .  .
        """.stripMargin
      )
      assert{board.gameSituation(isBlack = true).victoryPoints.isEmpty }
      assert{board.gameSituation(isBlack = false).victoryPoints.isEmpty }
    }

    "have 1 black four" in {
      val board = BoardMut.fromString(
        """
          | .  .  .  .  .  .  .  .
          | .  .  .  .  .  .  .  .
          | .  .  *  .  .  .  .  .
          | .  .  .  @  .  .  .  .
          | .  .  *  .  @  .  .  .
          | .  .  .  .  .  @  .  .
          | .  .  .  .  .  .  @  .
          | .  .  .  .  .  .  .  .
        """.stripMargin
      )
      assert{board.gameSituation(isBlack = true).victoryPoints.size === 1 }
      assert{board.gameSituation(isBlack = false).victoryPoints.isEmpty }

      val board1 = BoardMut.fromString(
        """
          | .  .  .  .  .  .  .  .
          | .  .  .  .  .  .  .  .
          | .  .  @  .  .  .  .  .
          | .  .  .  @  .  .  .  .
          | .  .  *  .  @  .  .  .
          | .  .  .  .  .  .  .  .
          | .  .  .  .  .  .  @  .
          | .  .  .  .  .  .  .  .
        """.stripMargin
      )
      assert{board1.gameSituation(isBlack = true).victoryPoints.size === 1 }
      assert{board1.gameSituation(isBlack = true).victoryPoints contains Pos(5,5)}
    }

    "have 2 black four" in {
      val board0 = BoardMut.fromString(
        """
          | .  .  .  .  .  .  .  .
          | .  .  .  .  .  .  .  .
          | .  .  @  .  .  .  .  .
          | .  .  .  @  .  .  .  .
          | .  .  *  .  @  .  .  .
          | .  .  .  .  @  @  .  .
          | .  .  .  .  .  .  .  .
          | .  .  .  .  .  .  .  .
        """.stripMargin
      )
      assert{board0.gameSituation(isBlack = true).victoryPoints.size === 2 }

      val board1 = BoardMut.fromString(
        """
          | .  .  .  .  .  .  .  .
          | .  .  .  .  .  .  .  .
          | .  .  .  .  .  .  .  .
          | .  .  .  @  @  @  @  .
          | .  .  *  .  .  .  .  .
          | .  .  .  .  @  .  .  .
          | .  .  .  .  .  .  .  .
          | .  .  .  .  .  .  .  .
        """.stripMargin
      )
      assert{board1.gameSituation(isBlack = true).victoryPoints.size === 2 }


      val board2 = BoardMut.fromString(
        """
          | .  .  .  .  .  .  .  .
          | .  .  .  .  .  .  .  .
          | .  .  .  .  @  .  .  .
          | .  .  .  .  @  .  .  .
          | .  .  *  .  @  .  .  .
          | .  .  .  .  @  .  .  .
          | .  .  .  .  .  .  .  .
          | .  .  .  .  .  .  .  .
        """.stripMargin
      )
      assert{board2.gameSituation(isBlack = true).victoryPoints.size === 2 }
      assert{board2.gameSituation(isBlack = true).victoryPoints contains Pos(1,4)}
    }

    "have 0 black four" in {
      val board = BoardMut.fromString(
        """
          |.  .  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .  .
          |.  .  .  *  .  .  .  .  .
          |.  .  .  .  @  .  .  .  .
          |.  .  .  *  .  @  .  .  .
          |.  .  .  .  .  .  @  .  .
          |.  .  .  .  .  .  .  @  .
          |.  .  .  .  .  .  .  .  *
        """.stripMargin
      )
      assert{board.gameSituation(isBlack = true).victoryPoints.isEmpty }
      assert{board.gameSituation(isBlack = false).victoryPoints.isEmpty }
    }

    "recognize victory points" in {
      val board1 = BoardMut.fromString(
        """
          |.  .  .  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  *  .  .
          |.  .  .  .  *  .  .  .  .  .
          |.  .  .  .  *  .  .  *  .  .
          |.  @  .  @  .  .  @  .  .  .
          |.  .  .  @  *  *  @  *  .  .
          |.  .  .  @  @  .  @  .  .  .
          |.  .  .  .  .  @  @  @  .  .
          |.  .  .  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .  .  .
        """.stripMargin
      )
      assert(board1.gameSituation(isBlack = true).victoryPoints.contains(Pos(3,6)))

    }
  }

  "Leading points statistics" should {
    "have correct number of leading points" in {
      val b = BoardMut.fromString(
        """
          | . . . . . . .
          | . . . . . . .
          | . . . . . . .
          | . . @ @ @ . .
          | . . . . . . .
          | . . . . . . .
          | . . . . . . .
        """.stripMargin)
      assert { b.gameSituation(isBlack = true).leadingPoints === Set(Pos(3,1), Pos(3,5)) }
      // block left
      val b_left = b.duplicate
      b_left.placeAPiece(Pos(3,1), playerIsBlack = false)
      assert { b_left.checkLeadingPoint(Pos(3,5), isBlack = true) === false }
      assert { b_left.gameSituation(isBlack = true).leadingPoints === Set() }

      val b_ll = b.duplicate
      b_ll.placeAPiece(Pos(3,0), playerIsBlack = false)
      assert { b_ll.checkLeadingPoint(Pos(3,1), isBlack = true) === false }
      assert { b_ll.numberOfSameColorInDirectionWithSlotInfo(Pos(3,5), (0,-1), PosColor.Black) === (3, Some(Pos(3,1))) }
      assert { b_ll.numberOfSameColorInDirectionWithSlotInfo(Pos(3,5), (0, 1), PosColor.Black) === (0, Some(Pos(3,6))) }
      assert { b_ll.checkLeadingPoint(Pos(3,5), isBlack = true) === true }
      assert { b_ll.gameSituation(true).leadingPoints == Set(Pos(3,5))}

      val b1 = BoardMut.fromString(
        """
          | . . . . . . .
          | . . . . . . .
          | . . . . @ . .
          | . @ . . . . .
          | . . @ . . . .
          | . @ . . . . .
          | . . . . . . .
        """.stripMargin)
      assert { b1.gameSituation(isBlack = true).leadingPoints.size === 1 }
      // block middle
      val b1_middle = b1.duplicate
      b1_middle.placeAPiece(Pos(3,3), playerIsBlack = false)
      assert { b1_middle.gameSituation(true).leadingPoints === Set() }
      // block left
      val b1_left = b1.duplicate
      b1_left.placeAPiece(Pos(6,0), playerIsBlack = false)
      assert { b1_left.gameSituation(true).leadingPoints === Set() }
      // block right
      val b1_right = b1.duplicate
      b1_right.placeAPiece(Pos(1,5), playerIsBlack = false)
      assert { b1_right.gameSituation(true).leadingPoints === Set() }

      val b2 = BoardMut.fromString(
        """
          | . . . . . . .
          | . . . . . . .
          | . . . . . . .
          | . @ @ . @ . .
          | . . . . @ . .
          | . . . . @ . .
          | . . . . . . .
        """.stripMargin)
      assert { b2.gameSituation(isBlack = true).leadingPoints.size === 2 }

      val b3 = BoardMut.fromString(
        """
          | . . . . . . .
          | . . . . . . .
          | . . . . . . .
          | * . @ @ @ . .
          | . . . . . . .
          | . . . . @ . .
          | . . . . . . .
        """.stripMargin)
      assert { b3.numberOfSameColorInDirectionWithSlotInfo(Pos(3,1), (0,-1), PosColor.Black) === (0, None) }
      assert { b3.numberOfSameColorInDirectionWithSlotInfo(Pos(3,1), (0, 1), PosColor.Black) === (3, Some(Pos(3,5))) }
      assert { b3.gameSituation(isBlack = true).leadingPoints === Set(Pos(3,5)) }
    }

    "have non leading points" in {
      val b3 = BoardMut.fromString(
        """
          | . . . . . . .
          | . . . . @ . .
          | . . . . @ . .
          | . . . . @ @ @
          | . . . . . . .
          | . . . . @ . .
          | . . . . . . .
        """.stripMargin)
      assert { b3.gameSituation(isBlack = true).leadingPoints.size === 0 }
    }
  }
}
