package gobang

import scala.util.Random

/**
  * Created by weijiayi on 05/12/2016.
  */
trait GobangPlayer {
  def name: String
  def playsBlack: Boolean

  def decideWhereToMove(boardMut: BoardMut): Pos
}

class AIPlayer(val name: String, val playsBlack: Boolean, sampleNum: Int) extends GobangPlayer {
  val maxTurnCut = 1000
  case class WinStat(pos: Pos, winRates: (Double, Double)){
    val (selfWin, oppWin) = if(playsBlack) winRates else (winRates._2, winRates._1)
    val winFactor = selfWin - oppWin

    def winRatesString = "WinRates: %.1f%% vs %.1f%%".format(selfWin*100, oppWin*100)
  }

  def assessWinRates(board: BoardMut, pos: Pos, currentIsBlack: Boolean):
  (Double, Double) = {
    def turnWeight(turn: Int): Double = {
      if(turn == 0) sampleNum
      else 1.0 / (turn*turn)
    }

    val results = (0 until sampleNum).par.map{ i =>
      val (winner, turn) = RandomPlay.semiRandomPlay(board.duplicate, pos, currentIsBlack, maxTurnCut, new Random(i)) //fixme
      winner.winner match {
        case PosState.Empty =>
          (0,turnWeight(turn))
        case PosState.Black =>
          (1,turnWeight(turn))
        case PosState.White =>
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

  def decideWhereToMove(board: BoardMut): Pos = {
    val leftSize = board.interestRegion.size
    print("." * leftSize)
    println(s"($leftSize)")

    val reportLock = new Object
    var finished = 0
    def report(): Unit = reportLock.synchronized {
      finished += 1
      print("-")
    }

    val stat = board.interestRegion.map(id => {
      val pos = board.idToPos(id)
      val winRates = assessWinRates(board, pos, playsBlack)
      report()
      WinStat(pos, winRates)
    }).maxBy(_.winFactor)

    println()
    println(stat.winRatesString)
    stat.pos
  }

  def decideWhereToMoveSilent(board: BoardMut): WinStat = {
    val stat = board.interestRegion.map(id => {
      val pos = board.idToPos(id)
      val winRates = assessWinRates(board, pos, playsBlack)
      WinStat(pos, winRates)
    }).maxBy(_.winFactor)

    stat
  }
}


class CommandlinePlayer(val name: String, val playsBlack: Boolean) extends GobangPlayer{

  def decideWhereToMove(boardMut: BoardMut): Pos = {
    println("What's your move? (row and column separated by space)")

    try {
      Console.in.readLine().trim.split("\\s+") match {
        case Array(rs, cs) =>
          val p = Pos(rs.toInt, cs.toInt)
          if (boardMut.stateAt(p).contains(PosState.Empty))
            p
          else {
            println("!! Place already occupied.")
            decideWhereToMove(boardMut)
          }
        case input => throw new Exception(s"Input = '${input.toSeq}'")
      }
    } catch {
      case e: Exception =>
        println("!! Input error: " + e)
        decideWhereToMove(boardMut)
    }
  }
}


