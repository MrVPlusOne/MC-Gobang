package gobang

import gobang.PosState.Empty

import scala.annotation.tailrec
import collection.mutable
import scala.util.Random

case class GameWinner(winner: PosState)


object RandomPlay {

  def randomPlay(board: BoardMut, startPos: Pos, currentIsBlack: Boolean, random: Random): GameWinner = {
    val leftMoves = board.leftMoves

    def nextPosId(): Int = {
      val x = random.nextInt(leftMoves.size)
      var i = 0
      leftMoves.foreach(id => {
        if(i == x) return id
        else i += 1
      })
      throw new Exception
    }

    def playATurn(posId: Int, playerIsBlack: Boolean): GameWinner = {
      leftMoves.remove (posId)
      board.placePiece(board.idToPos(posId), playerIsBlack) match {
        case GameWinner(Empty) =>
          if(leftMoves.isEmpty)
            GameWinner(Empty) // it's a draw
          else{
            playATurn(nextPosId(), !playerIsBlack)
          }
        case winner =>
          winner
      }
    }

    playATurn(board.posToId(startPos), currentIsBlack)
  }

  /**
    * Assess the winning rate of two players using repeated randomPlay, will not modify board
    * @return (blackWinRate, whiteWinRate)
    */
  def assessWinRates(board: BoardMut, pos: Pos, currentIsBlack: Boolean, sampleNum: Int, random: Random):
  (Double, Double) = {
    var blackWins, whiteWins = 0
    (0 until sampleNum).foreach{ i =>
      randomPlay(board.duplicate, pos, currentIsBlack, random).winner match {
        case PosState.Black => blackWins += 1
        case PosState.White => whiteWins += 1
        case _ => // nothing
      }
    }
    (blackWins.toDouble / sampleNum, whiteWins.toDouble / sampleNum)
  }
}
