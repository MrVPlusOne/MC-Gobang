package gobang

import scala.util.Random

/**
  * Represent a game player, can be AI or human.
  */
trait GobangPlayer {
  def name: String
  def playsBlack: Boolean

  def makeDecision(boardMut: BoardMut): GobangPlayerDecision
}

sealed trait GobangPlayerDecision

object GobangPlayerDecision{
  case class NextMove(pos: Pos) extends GobangPlayerDecision
  case object Unroll extends GobangPlayerDecision
  case object GiveUp extends GobangPlayerDecision
}

/** Monte Carlo Game AI Player */
class AIPlayer(val name: String, val playsBlack: Boolean,
               aiAlgorithm: AIAlgorithm,
               turnWeight: Int => Double = AIAlgorithm.constantTurnWeight,
               winFactorF: (Double, Double) => Double = _ - _,
               sampleNum: Int, seed: Int = 0,
               simulateOpp: Boolean = true,
               reportProgress: Boolean = true) extends GobangPlayer {
  val random = new Random(seed)

  case class WinStat(pos: Pos, winRates: (Double, Double)){
    def winRateOf(playsBlack: Boolean) = if(playsBlack) winRates._1 else winRates._2

    def winFactor(playsBlack: Boolean) = winFactorF(winRateOf(playsBlack), winRateOf(!playsBlack))

    def winRatesString = "WinRates: %.1f%% vs %.1f%%".format(winRates._1*100, winRates._2*100)
  }

  def assessWinRates(board: BoardMut, pos: Pos, currentIsBlack: Boolean):
  (Double, Double) = {
    val offset = random.nextInt
    val results = (0 until sampleNum).par.map{ i =>
      val (winner, turn) = aiAlgorithm.randomSimulateAGame(board.duplicate, pos, currentIsBlack, i + offset)
      winner.winner match {
        case PosColor.Empty =>
          (0,turnWeight(turn))
        case PosColor.Black =>
          (1,turnWeight(turn))
        case PosColor.White =>
          (-1,turnWeight(turn))
      }
    }.toArray

    var blackWinScore, whiteWinScore, drawScore = 0.0
    results.foreach{
      case (tag, value) => tag match {
        case 0 => drawScore += value
        case 1 => blackWinScore += value
        case -1 => whiteWinScore += value
      }
    }
    val total = blackWinScore + whiteWinScore + drawScore
    (blackWinScore/total, whiteWinScore/total)
  }

  def chooseBestFrom(board: BoardMut, positions: Seq[Pos], playsBlack: Boolean): WinStat = {
    val leftSize = positions.size
    if(reportProgress) {
      print("." * leftSize)
      println(s"($leftSize)")
    }

    var finished = 0
    def report(): Unit = if(reportProgress) {
      finished += 1
      print("-")
      if (finished == leftSize)
        println()
    }

    positions.map(pos => {
      val winRates = assessWinRates(board, pos, playsBlack)
      report()
      WinStat(pos, winRates)
    }).maxBy(_.winFactor(playsBlack))
  }

  def makeDecision(board: BoardMut): GobangPlayerDecision = {
    def nextMove(pos: Pos) = GobangPlayerDecision.NextMove(pos)
    def nextMoveUseStat(stat: WinStat) = {
      println(stat.winRatesString)
      nextMove(stat.pos)
    }

    // if it's the beginning
    if(board.interestRegion.isEmpty)
      return nextMove(Pos(board.size/2, board.size/2))

    val selfSituation = board.gameSituation(playsBlack)
    val oppSituation = board.gameSituation(!playsBlack)

    // take the victory
    val victoryPoints: Set[Pos] = selfSituation.victoryPoints
    if(victoryPoints.nonEmpty)
      return nextMove(victoryPoints.head)

    // defend victory points
    if(oppSituation.victoryPoints.nonEmpty)
      return nextMove(oppSituation.victoryPoints.head)

    // take the victory using leading points
    if(selfSituation.leadingPoints.nonEmpty)
      return nextMove(selfSituation.leadingPoints.head)

    // defend leading points
    if(oppSituation.leadingPoints.nonEmpty){
      val stat = chooseBestFrom(board, oppSituation.leadingPoints.toSeq, playsBlack)
      return nextMoveUseStat(stat)
    }

    // random asses
    val stat1 = chooseBestFrom(board, board.interestRegion.map(board.idToPos), playsBlack)
    if(simulateOpp) {
      if (stat1.winRateOf(playsBlack) > 0.999)
        nextMoveUseStat(stat1)
      else {
        val stat2 = chooseBestFrom(board, board.interestRegion.map(board.idToPos), !playsBlack)
        if (stat1.winFactor(playsBlack) > stat2.winFactor(!playsBlack)){
          print("(attack) ")
          nextMoveUseStat(stat1)
        }
        else {
          print("(defend) ")
          nextMoveUseStat(stat2)
        }
      }
    }else {
      nextMoveUseStat(stat1)
    }
  }

}


class CommandlinePlayer(val name: String, val playsBlack: Boolean) extends GobangPlayer{

  def makeDecision(boardMut: BoardMut): GobangPlayerDecision = {
    println("What's your move? (row and column separated by space, use 'unroll' to roll back 2 turn)")

    try {
      val input = Console.in.readLine().trim.toLowerCase()
      if(input == "unroll")
        return GobangPlayerDecision.Unroll
      if(input == "give up")
        return GobangPlayerDecision.GiveUp

      input.split("\\s+") match {
        case Array(rs, cs) =>
          val p = Pos(rs.toInt, cs.toInt)
          if (boardMut.stateAt(p).contains(PosColor.Empty))
            GobangPlayerDecision.NextMove(p)
          else {
            println("!! Place already occupied.")
            makeDecision(boardMut)
          }
        case other => throw new Exception(s"Input = '${other.toSeq}'")
      }
    } catch {
      case e: Exception =>
        println("!! Input error: " + e)
        makeDecision(boardMut)
    }
  }
}


