package gobang

import gobang.PosState.Empty

import scala.annotation.tailrec
import collection.mutable
import scala.util.Random

case class GameWinner(winner: PosState)


object RandomPlay {

  /**
    * Monte-Carlo game simulation: both sides play randomly.
    * @return (Winner, Game end turn)
    */
  def randomPlay(board: BoardMut, startPos: Pos, currentIsBlack: Boolean, maxTurn: Int, random: Random):
  (GameWinner, Int) = {
    def leftMoves = board.interestRegion

    def nextPosId(): Int = { // todo: use better data structure
      val x = random.nextInt(leftMoves.size)
      var i = 0
      leftMoves.foreach(id => {
        if(i == x) return id
        else i += 1
      })
      throw new Exception
    }

    def playATurn(posId: Int, playerIsBlack: Boolean, turn: Int): (GameWinner, Int) = {
      board.placePiece(board.idToPos(posId), playerIsBlack) match {
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

  def semiRandomPlay(board: BoardMut, startPos: Pos, currentIsBlack: Boolean, maxTurn: Int, random: Random,
                     verbose: Boolean = false):
  (GameWinner, Int) = {

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
      if (verbose) {
        println(s"turn: $turn, isBlack: $playerIsBlack, pos: ${board.idToPos(posId)}")
        println(board)
      }
      board.placePiece(board.idToPos(posId), playerIsBlack) match {
        case GameWinner(Empty) =>
          if (turn >= maxTurn || board.interestRegion.isEmpty)
            (GameWinner(Empty), turn) // it's a draw
          else {
            playATurn(nextPosId(!playerIsBlack), !playerIsBlack, turn + 1)
          }
        case winner =>
          (winner, turn)
      }
    }

    playATurn(board.posToId(startPos), currentIsBlack, 0)
  }

  // using counters
  def complexSimulation(board: BoardMut, startPos: Pos, currentIsBlack: Boolean, maxTurn: Int, random: Random,
                     verbose: Boolean = false):
  (GameWinner, Int) = {

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
      if (verbose) {
        println(s"turn: $turn, isBlack: $playerIsBlack, pos: ${board.idToPos(posId)}")
        println(board)
      }
      board.placePiece(board.idToPos(posId), playerIsBlack) match {
        case GameWinner(Empty) =>
          if (turn >= maxTurn || board.interestRegion.isEmpty)
            (GameWinner(Empty), turn) // it's a draw
          else {
            playATurn(nextPosId(!playerIsBlack), !playerIsBlack, turn + 1)
          }
        case winner =>
          (winner, turn)
      }
    }

    playATurn(board.posToId(startPos), currentIsBlack, 0)
  }

}
