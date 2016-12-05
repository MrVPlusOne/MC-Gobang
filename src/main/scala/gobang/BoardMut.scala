package gobang

import scala.collection.mutable


sealed trait PosState

object PosState{
  case object Empty extends PosState
  case object Black extends PosState
  case object White extends PosState

  def player(playerIsBlack: Boolean) = if(playerIsBlack) Black else White
}

case class Pos(row: Int, col: Int)


/**
  * Mutable Chess board
  */
class BoardMut(val data: Array[PosState], val size: Int) {
  require(size>0)

  def numberOfSameColorInDirection(pos: Pos, dir: (Int, Int), color: PosState, maxLen: Int): Int = {
    val (dx,dy) = dir
    (0 until maxLen).foreach(i => {
      val p = Pos(pos.row + dy*i, pos.col + dx*i)
      if(!stateAt(p).contains(color)) return i
    })
    maxLen
  }

  def thisPieceEndedGame(pos: Pos, color: PosState, winNeed: Int = 5): Boolean = {
    val dirs = Seq(
      ((0, 1), ( 0,-1)),
      ((1, 0), (-1, 0)),
      ((1, 1), (-1,-1)),
      ((1, -1),(-1, 1))
    )
    dirs.foreach{
      case (d1, d2) =>
        val total = numberOfSameColorInDirection(pos, d1, color, winNeed) +
          numberOfSameColorInDirection(pos, d2, color, winNeed) - 1
        if(total>=winNeed)
          return true
    }
    false
  }

  /** play a piece on the board, then return the current game winner **/
  def placePiece(pos: Pos, playerIsBlack: Boolean): GameWinner = {
    val color = PosState.player(playerIsBlack)
    data(posToId(pos)) = color
    if(thisPieceEndedGame(pos, color))
      GameWinner(color)
    else
      GameWinner(PosState.Empty)
  }

  def idToPos(id: Int): Pos = Pos (id / size, id % size)

  def posToId(pos: Pos) = pos.row * size + pos.col

  def stateAt(pos: Pos): Option[PosState] = {
    if (pos.row >= 0 && pos.row < size && pos.col >= 0 && pos.col < size)
      Some(data(posToId(pos)))
    else None
  }

  override def toString = {
    require(size < 100) // for printing

    val builder = new StringBuilder()
    val empty = " . "
    val black = " @ "
    val white = " * "

    val colHeader = empty + (0 until size).map(i => "%2d ".format(i)).mkString
    builder.append(colHeader)
    builder.append("\n")

    for(r <- 0 until size) {
      // draw a row
      builder.append("%2d ".format(r))
      for (c <- 0 until size) {
        val s = data(r*size+c) match {
          case PosState.Empty => empty
          case PosState.Black => black
          case PosState.White => white
        }
        builder.append(s)
      }
      builder.append("\n")
    }
    builder.toString()
  }

  def leftMoves: mutable.Set[Int] = {
    mutable.Set(data.indices.filter(i => data(i) == PosState.Empty):_*)
  }

  def duplicate = new BoardMut(data.clone(), size)
}

object BoardMut{
  def emptyBoard(size: Int) = {
    val data = Array.fill(size*size)(PosState.Empty: PosState)
    new BoardMut(data, size)
  }
}

