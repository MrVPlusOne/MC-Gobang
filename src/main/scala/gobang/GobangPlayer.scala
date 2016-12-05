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

class AIPlayer(val name: String, val playsBlack: Boolean, sampleNum: Int, random: Random) extends GobangPlayer {
  case class WinStat(pos: Pos, winRates: (Double, Double)){
    val (selfWin, oppWin) = if(playsBlack) winRates else (winRates._2, winRates._1)
    val winFactor = selfWin - oppWin

    def winRatesString = "WinRates: %.1f%% vs %.1f%%".format(selfWin*100, oppWin*100)
  }

  def decideWhereToMove(board: BoardMut): Pos = {
    val leftSize = board.leftMoves.size
    println("." * leftSize)

    val reportLock = new Object
    var finished = 0
    def report(): Unit = reportLock.synchronized {
      finished += 1
      print("-")
    }

    val stat = board.leftMoves.par.map(id => {
      val pos = board.idToPos(id)
      val winRates = RandomPlay.assessWinRates(board, pos, playsBlack, sampleNum, new Random(id))
      report()
      WinStat(pos, winRates)
    }).maxBy(_.winFactor)

    println()
    println(stat.winRatesString)
    stat.pos
  }
}

class CommandlinePlayer(val name: String, val playsBlack: Boolean) extends GobangPlayer{

  def decideWhereToMove(boardMut: BoardMut): Pos = {
    println(boardMut)
    println()
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


