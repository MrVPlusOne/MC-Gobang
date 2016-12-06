package tests.integrated

import gobang.{AIPlayer, BoardMut, Pos}
import tests.MyPropTest

import scala.util.Random

/**
  * Created by weijiayi on 06/12/2016.
  */
class AIScenarioTest extends MyPropTest{
  "A four with one living end" should {
    val board = BoardMut.fromString(
      """
        |...*...
        |...@...
        |...@...
        |...@.*.
        |...@.*.
        |.....*.
        |.......
      """.stripMargin
    )

    "makes black to attack" in {
      val ai = new AIPlayer("AI", playsBlack = true, sampleNum = 100)
      assert(ai.assessWinRates(board, Pos(5,3), currentIsBlack = true) === (1.0,0))
      assert(ai.decideWhereToMove(board) === Pos(5,3))
    }

    "drive AI to block it" in {
      val ai = new AIPlayer("AI", playsBlack = false, sampleNum = 200)
      println("target win rate: " + ai.assessWinRates(board, Pos(5,3), currentIsBlack = false))
      assert(ai.decideWhereToMove(board) === Pos(5,3))
    }
  }

  "A leading three" should {
    val board = BoardMut.fromString(
      """
        | .  .  .  .  .  .  .  .  .
        | .  .  .  .  .  .  .  .  .
        | .  .  .  .  @  .  .  .  .
        | .  .  .  .  @  .  .  .  .
        | .  .  .  .  @  .  *  .  .
        | .  .  .  .  .  *  .  .  .
        | .  .  .  .  .  .  .  .  .
        | .  .  .  .  .  .  .  .  .
        | .  .  .  .  .  .  .  .  .
      """.stripMargin
    )
    "drive AI to block" in {
      val ai = new AIPlayer("AI", playsBlack = false, sampleNum = 500)
      val move = ai.decideWhereToMove(board)
      assert(move === Pos(5,4) || move === Pos(1,4),
        s"(5,4) win rate: ${ai.assessWinRates(board, Pos(5,4), false)}")
    }
  }
}
