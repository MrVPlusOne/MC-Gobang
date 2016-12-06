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
class BoardMut(val size: Int, val data: Array[PosState],
               val dirtyRegion: Array[Boolean], var interestRegion: IndexedSeq[Int], paintSize: Int = 2) {
  require(size>0)

  def paintRegion(center: Pos): Unit = {
    val centerId = posToId(center)
    interestRegion = interestRegion.filterNot(_ == centerId)

    /*  # # #
    *    ###
    *   ## ##
    *    ###
    *   # # #
    */
    val offsets = Seq(
      (-2,-2),(-1,-1), (1,1),(2,2),
      (0,-2),(0,-1),(0,1),(0,2),
      (2,-2),(1,-1),(-1,1),(-2,2),
      (2,0),(1,0),(-1,0),(-2,0)
    )

    for{
      (r,c) <- offsets
      pos = Pos(center.row + r,center.col + c)
      id = posToId(pos)
      if stateAt(pos).contains(PosState.Empty) && !dirtyRegion(id)
    } {
      dirtyRegion(id) = true
      interestRegion :+= id
    }
  }

  @inline
  def numberOfSameColorInDirection(pos: Pos, dir: (Int, Int), color: PosState, maxLen: Int): Int = {
    val (dx,dy) = dir
    var i = 1
    while(i<maxLen){
      val p = Pos(pos.row + dy*i, pos.col + dx*i)
      if(!stateAt(p).contains(color)) return i-1

      i += 1
    }
    maxLen
  }

  @inline
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
          numberOfSameColorInDirection(pos, d2, color, winNeed) + 1
        if(total>=winNeed)
          return true
    }
    false
  }

  def thisPieceEndedGame(id: Int, playsBlack: Boolean): Boolean = {
    val color = if(playsBlack) PosState.Black else PosState.White
    thisPieceEndedGame(idToPos(id), color)
  }

  /** play a piece on the board, then return the current game winner **/
  def placePiece(pos: Pos, playerIsBlack: Boolean): GameWinner = {
    val color = PosState.player(playerIsBlack)
    data(posToId(pos)) = color
    paintRegion(pos)
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

  def draw(last: Option[Pos]): String = {
    require(size < 100) // for printing

    val builder = new StringBuilder()
    val empty = Seq(" . ", ">.<")
    val black = Seq(" @ ", ">@<")
    val white = Seq(" * ", ">*<")

    val colHeader = "   " + (0 until size).map(i => "%2d ".format(i)).mkString
    builder.append(colHeader)
    builder.append("\n")

    for(r <- 0 until size) {
      // draw a row
      builder.append("%2d ".format(r))
      for (c <- 0 until size) {
        val h = if(last.contains(Pos(r,c))) 1 else 0
        val s = data(r*size+c) match {
          case PosState.Empty => empty(h)
          case PosState.Black => black(h)
          case PosState.White => white(h)
        }
        builder.append(s)
      }
      builder.append("\n")
    }
    builder.toString()
  }

  override def toString = draw(None)

  def duplicate = {
    new BoardMut(size, data.clone(), dirtyRegion.clone(), interestRegion)
  }
}

object BoardMut{
  def fromData(size: Int, data: Array[PosState]): BoardMut = {
    require(size>0)
    val board = new BoardMut(size, data, data.map(_ => false), IndexedSeq())
    data.indices.foreach{ id =>
      if(data(id) != PosState.Empty) board.paintRegion(board.idToPos(id))
    }
    board
  }

  def emptyBoard(size: Int) = {
    val data = Array.fill(size*size)(PosState.Empty: PosState)
    fromData(size, data)
  }

  def fromString(string: String) = {
    val s = string.trim.replace(" ", "")
    val lines = s.split("\n")
    val height = lines.length

    lines.foreach(l => require(l.length == height))

    val data = s.replace("\n","").map{
      case '@' => PosState.Black
      case '*' => PosState.White
      case '.' => PosState.Empty
    }
    fromData(height, data.toArray)
  }
}

