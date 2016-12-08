package gobang

import gobang.PosColor.Empty

import scala.annotation.tailrec
import collection.mutable
import scala.util.Random

case class GameWinner(winner: PosColor)


object RandomPlay {

  /**
    * Monte-Carlo game simulation: both sides play randomly.
    * @return (Winner, Game end turn)
    */
  def randomPlay(board: BoardMut, startPos: Pos, currentIsBlack: Boolean, maxTurn: Int, random: Random):
  (GameWinner, Int) = {
    def leftMoves = board.interestRegion

    def nextPosId(): Int = {
      val x = random.nextInt(leftMoves.size)
      var i = 0
      leftMoves.foreach(id => {
        if(i == x) return id
        else i += 1
      })
      throw new Exception
    }

    def playATurn(posId: Int, playerIsBlack: Boolean, turn: Int): (GameWinner, Int) = {
      board.placeAPiece(board.idToPos(posId), playerIsBlack) match {
        case GameWinner(Empty) =>
          if(turn>= maxTurn || leftMoves.isEmpty)
            (GameWinner(Empty), turn) // it's a draw
          else{
            playATurn(nextPosId(), !playerIsBlack, turn+1)
          }
        case winner =>
          (winner, turn)
      }
    }

    playATurn(board.posToId(startPos), currentIsBlack, 0)
  }

}
