package gobang

import gobang.AIAlgorithm.Turn

import scala.util.Random


trait AIAlgorithm {
  def randomSimulateAGame(board: BoardMut, startPos: Pos, currentIsBlack: Boolean, simulationId: Int): (GameWinner, Turn)

//  def turnWeight(turn: Int): Double
}

object AIAlgorithm{
  type Turn = Int

  def constantTurnWeight(turn: Int): Double = 1.0
  def inverseTurnWeight(turn: Int): Double = 1.0/(turn+1)
  def secondInverseTurnWeight(turn: Int): Double = 1.0/((turn+1)*(turn+1))
  def cubicWeight(turn: Int): Double = {
    val y: Double = turn+1
    1.0e9/(y*y*y)
  }
  def powerDecayTurnWeight(turn: Int): Double = math.pow(0.5, turn)

  case class SemiRandom(cutOffTurn: Int = Int.MaxValue, verbose: Boolean = false) extends AIAlgorithm {

    def turnWeight(turn: Int): Double = {
      if(turn == 0) 10000
      else 1.0 / (turn*turn)
    }

    override def randomSimulateAGame(board: BoardMut, startPos: Pos, currentIsBlack: Boolean, simulationId: Int):
    (GameWinner, Turn) = {
      {
        import gobang.PosColor._

        val random = new Random(simulationId)

        def nextPosId(nextIsBlack: Boolean): Int = {
          val leftMoves = board.interestRegion

          for(id <- leftMoves) {
            if(board.thisPieceEndedGame(id, nextIsBlack))
              return id
          }

          for(id <- leftMoves) {
            if(board.thisPieceEndedGame(id, !nextIsBlack))
              return id
          }

          // otherwise, play randomly
          val x = random.nextInt(leftMoves.size)
          leftMoves(x)
        }

        def playATurn(posId: Int, playerIsBlack: Boolean, turn: Int): (GameWinner, Int) = {
          val winner = board.placeAPiece(board.idToPos(posId), playerIsBlack)
          if (verbose) {
            println(s"turn: $turn, isBlack: $playerIsBlack, pos: ${board.idToPos(posId)}")
            println("fours: " + board.gameSituation)
            println(board.draw(Some(board.idToPos(posId))))
          }
          winner match {
            case GameWinner(Empty) =>
              if (turn >= cutOffTurn || board.interestRegion.isEmpty)
                (GameWinner(Empty), turn) // it's a draw
              else {
                playATurn(nextPosId(!playerIsBlack), !playerIsBlack, turn + 1)
              }
            case _ =>
              (winner, turn)
          }
        }

        playATurn(board.posToId(startPos), currentIsBlack, 0)
      }

    }
  }

  case class ConstraintSpace (cutOffTurn: Int = Int.MaxValue ,verbose: Boolean = false) extends AIAlgorithm {

    override def randomSimulateAGame(board: BoardMut, startPos: Pos, currentIsBlack: Boolean,
                                     simulationId: Turn): (GameWinner, Turn) = {
      import PosColor._
      val random = new Random(simulationId)

      def nextPosId(nextIsBlack: Boolean): Int = {
        val leftMoves = board.interestRegion

        val selfSituation: PlayerSituation = board.gameSituation(nextIsBlack)
        val oppSituation: PlayerSituation = board.gameSituation(!nextIsBlack)

        // take victory
        val thisFours = selfSituation.victoryPoints
        if(thisFours.nonEmpty)
          return board.posToId(thisFours.head)

        // defend opponent's victory points
        val oppFours = oppSituation.victoryPoints
        if(oppFours.nonEmpty)
          return board.posToId(oppFours.head)

        // attack using leading points
        if(selfSituation.leadingPoints.nonEmpty){
          val x = random.nextInt(selfSituation.leadingPoints.size)
          return board.posToId(selfSituation.leadingPoints.toArray.apply(x))
        } else if (oppSituation.leadingPoints.nonEmpty) {
          // defend opponent's leading points
          val x = random.nextInt(oppSituation.leadingPoints.size)
          return board.posToId(oppSituation.leadingPoints.toArray.apply(x))
        }


        // otherwise, play randomly
        val x = random.nextInt(leftMoves.size)
        leftMoves(x)
      }

      def playATurn(posId: Int, playerIsBlack: Boolean, turn: Int): (GameWinner, Int) = {
        val winner = board.placeAPiece(board.idToPos(posId), playerIsBlack)

        if (verbose) {
          println(s"turn: $turn, isBlack: $playerIsBlack, pos: ${board.idToPos(posId)}")
          println("fours: " + board.gameSituation)
          println(board.draw(Some(board.idToPos(posId))))
        }
        winner match {
          case GameWinner(Empty) =>
            if (turn >= cutOffTurn || board.interestRegion.isEmpty)
              (GameWinner(Empty), turn) // it's a draw
            else {
              playATurn(nextPosId(!playerIsBlack), !playerIsBlack, turn + 1)
            }
          case _ =>
            (winner, turn)
        }
      }

      playATurn(board.posToId(startPos), currentIsBlack, 0)
    }

  }
}