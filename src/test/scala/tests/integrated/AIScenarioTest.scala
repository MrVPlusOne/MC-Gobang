package tests.integrated

import gobang.GobangPlayerDecision.NextMove
import gobang.{AIAlgorithm, AIPlayer, BoardMut, Pos}
import tests.MyPropTest

import scala.util.Random

/**
  * Created by weijiayi on 06/12/2016.
  */
class AIScenarioTest extends MyPropTest{

//  val algorithm = AIAlgorithm.ConstraintSpace()
  val algorithm = AIAlgorithm.ConstraintSpace()

  def nextMove(r: Int, c: Int) = NextMove(Pos(r,c))

  s"algorithm: $algorithm" when {

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
        """.
          stripMargin
      )

      "makes black to attack" in {
        val ai = new AIPlayer("AI", playsBlack = true, algorithm, sampleNum = 100)
        assert(ai.assessWinRates(board, Pos(5, 3), currentIsBlack = true) === (1.0, 0))
        assert(ai.makeDecision(board) === NextMove(Pos(5, 3)))
      }

      "drive AI to block it" in {
        val ai = new AIPlayer("AI", playsBlack = false, algorithm, sampleNum = 100)
        println("target win rate: " + ai.assessWinRates(board, Pos(5, 3), currentIsBlack = false))
        assert(ai.makeDecision(board) === NextMove(Pos(5, 3)))
      }
    }

    "A four whith one whole" should {
      val board = BoardMut.fromString(
        """
          |...*...
          |...@...
          |...@.*.
          |...@.*.
          |.....*.
          |...@.*.
          |.......
        """.stripMargin)

      "make black attack" in {
        val ai = new AIPlayer("AI", playsBlack = true, algorithm, sampleNum = 100)
        assert{ ai.makeDecision(board) == NextMove(Pos(4,3))}
      }

      "make white defend" in {
        val ai = new AIPlayer("AI", playsBlack = false, algorithm, sampleNum = 100)
        val decision = ai.makeDecision(board)
        assert{ decision == NextMove(Pos(1,5)) || decision == nextMove(6,5)}
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
        """.
          stripMargin
      )
      "drive AI to block" in {
        val ai = new AIPlayer("AI", playsBlack = false, algorithm, sampleNum = 500)
        val move = ai.makeDecision(board)
        assert(
          move === nextMove(5, 4) || move === nextMove(1, 4),
          s"(5,4) win rate: ${ai.assessWinRates(board, Pos(5, 4), false)}")
      }
    }
  }
}
