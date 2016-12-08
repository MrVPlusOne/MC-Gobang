package gobang

import scala.collection.mutable


sealed trait PosColor

object PosColor{
  case object Empty extends PosColor

  sealed trait Occupy extends PosColor
  case object Black extends Occupy
  case object White extends Occupy

  def fromIsBlack(playerIsBlack: Boolean): Occupy = if(playerIsBlack) Black else White
}

case class Pos(row: Int, col: Int){
  def offset(dr: Int, dc: Int) = Pos(row+dr, col+dc)
}

case class PlayerPair[A](black: A, white: A){
  def apply(isBlack: Boolean) =
    if(isBlack) black else white

  def updated(isBlack: Boolean, valueUpdate: A => A) =
    if(isBlack) PlayerPair(valueUpdate(black), white) else PlayerPair(black, valueUpdate(white))

  override def toString: String = {
    s"""PlayerPair(
      |black: $black
      |------------------
      |white: $white
      |)
    """.stripMargin
  }
}


class PlayerSituation
(
  /** i.e fours */
  var victoryPoints: Set[Pos],
  /** i.e. threes */
  var leadingPoints: Set[Pos]
){

  def copy = {
    new PlayerSituation(victoryPoints, leadingPoints)
  }
}

object PlayerSituation{
  def empty = new PlayerSituation(Set(), Set())
}
/**
  * Mutable Chess board
  */
class BoardMut(val size: Int, val data: Array[PosColor], var gameSituation: PlayerPair[PlayerSituation],
               val dirtyRegion: Array[Boolean], var interestRegion: IndexedSeq[Int], paintSize: Int = 2) {
  require(size>0)

  def updateSituation(pos: Pos): Unit = {
    val posIsBlack = stateAt(pos) match {
      case Some(PosColor.Black) =>
        true
      case Some(PosColor.White) =>
        false
      case _ => return
    }

    // additions
    val color = PosColor.fromIsBlack(posIsBlack)
    BoardMut.dirs.foreach {
      case (d1, d2) =>
        val (dr, dc) = d1

        for {
          i <- -4 to 4
          p = pos.offset(dr * i, dc * i)
          if stateAt(p).contains(PosColor.Empty)
        } {
          val (n1, slot1) = numberOfSameColorInDirectionWithSlotInfo(p, d1, color, 5)
          val (n2, slot2) = numberOfSameColorInDirectionWithSlotInfo(p, d2, color, 5)

          // add victory points
          val total = n1 + n2 + 1
          if (total >= 5)
            gameSituation(posIsBlack).victoryPoints += p

          // add leading points
          if (total == 4 && slot1.nonEmpty && slot2.nonEmpty) {
            gameSituation(posIsBlack).leadingPoints += p
//            var leadingPoints = Set(p)
//            if (n1 > 0) leadingPoints += slot1.get
//            if (n2 > 0) leadingPoints += slot2.get
//
//            gameSituation(posIsBlack).leadingPoints ++= leadingPoints
          }
        }
    }

    // deletions
    gameSituation(!posIsBlack).victoryPoints -= pos
    gameSituation(!posIsBlack).leadingPoints -= pos

    def oppLeadingPoints = gameSituation(!posIsBlack).leadingPoints
    BoardMut.dirs.foreach {
      case (d1, d2) =>
        val (dr, dc) = d1
        for {
          i <- -5 to 5 // leading point affected range can be up to 5. for example:  1@@.@2
          p = pos.offset(dr * i, dc * i)
          if stateAt(p).contains(PosColor.Empty) && oppLeadingPoints.contains(p)
        } {
          if (!checkLeadingPoint(p, !posIsBlack))
            gameSituation(!posIsBlack).leadingPoints -= p
        }
    }

  }

  def numberOfSameColorInDirectionWithSlotInfo(pos: Pos, dir: (Int, Int), color: PosColor,
                                               maxLen: Int = 5):
  (Int, Option[Pos]) = {

    val (dr,dc) = dir
    var i = 1
    while(i<maxLen){ // use while for better performance
      val p = pos.offset(dr*i, dc*i)
      stateAt(p) match {
        case Some(c) if c == color => // do nothing
        case Some(PosColor.Empty) => return (i-1, Some(p))
        case _ => return (i-1, None)
      }

      i += 1
    }
    (maxLen, None)
  }

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
      if stateAt(pos).contains(PosColor.Empty) && !dirtyRegion(id)
    } {
      dirtyRegion(id) = true
      interestRegion :+= id
    }
  }

  @inline
  def numberOfSameColorInDirection(pos: Pos, dir: (Int, Int), color: PosColor, maxLen: Int): Int = {
    val (n, _) = numberOfSameColorInDirectionWithSlotInfo(pos, dir, color, maxLen)
    n
  }

  @inline
  def thisPieceEndedGame(pos: Pos, color: PosColor, winNeed: Int = 5): Boolean = {
    BoardMut.dirs.foreach{
      case (d1, d2) =>
        val total = numberOfSameColorInDirection(pos, d1, color, winNeed) +
          numberOfSameColorInDirection(pos, d2, color, winNeed) + 1
        if(total>=winNeed)
          return true
    }
    false
  }

  /** assume pos is empty */
  def checkLeadingPoint(pos: Pos, isBlack: Boolean): Boolean = {
    val color = PosColor.fromIsBlack(isBlack)

    BoardMut.dirs.foreach{
      case (d1, d2) =>
        val (n1, slot1) = numberOfSameColorInDirectionWithSlotInfo(pos, d1, color, 5)
        val (n2, slot2) = numberOfSameColorInDirectionWithSlotInfo(pos, d2, color, 5)
        if(n1+n2+1==4 && slot1.nonEmpty && slot2.nonEmpty)
          return true
    }
    false
  }


  def thisPieceEndedGame(id: Int, playsBlack: Boolean): Boolean = {
    val color = if(playsBlack) PosColor.Black else PosColor.White
    thisPieceEndedGame(idToPos(id), color)
  }

  /** play a piece on the board, then return the current game winner **/
  def placeAPiece(pos: Pos, playerIsBlack: Boolean): GameWinner = {
    val color = PosColor.fromIsBlack(playerIsBlack)
    data(posToId(pos)) = color
    paintRegion(pos)
    updateSituation(pos)
    if(thisPieceEndedGame(pos, color))
      GameWinner(color)
    else
      GameWinner(PosColor.Empty)
  }

  def idToPos(id: Int): Pos = Pos (id / size, id % size)

  def posToId(pos: Pos) = pos.row * size + pos.col

  def stateAt(pos: Pos): Option[PosColor] = {
    if (pos.row >= 0 && pos.row < size && pos.col >= 0 && pos.col < size)
      Some(data(posToId(pos)))
    else None
  }

  def draw(last: Option[Pos]): String = {
    require(size < 100) // for printing

    val builder = new StringBuilder()
    val empty = Seq(" . ", ">.<")
    val black = Seq(" @ ", ">@<")
//    val white = Seq(" * ", ">*<")
    val white = Seq(" O ", ">O<")

    val colHeader = "   " + (0 until size).map(i => "%2d ".format(i)).mkString
    builder.append(colHeader)
    builder.append("\n")

    for(r <- 0 until size) {
      // draw a row
      builder.append("%2d ".format(r))
      for (c <- 0 until size) {
        val h = if(last.contains(Pos(r,c))) 1 else 0
        val s = data(r*size+c) match {
          case PosColor.Empty => empty(h)
          case PosColor.Black => black(h)
          case PosColor.White => white(h)
        }
        builder.append(s)
      }
      builder.append("\n")
    }
    builder.toString()
  }

  override def toString = draw(None)

  def duplicate = {
    new BoardMut(size, data.clone(), PlayerPair(gameSituation(true).copy, gameSituation(false).copy),
      dirtyRegion.clone(), interestRegion)
  }
}

object BoardMut{
  val dirs = Seq(
    ((0, 1), ( 0,-1)),
    ((1, 0), (-1, 0)),
    ((1, 1), (-1,-1)),
    ((1, -1),(-1, 1))
  )

  def fromData(size: Int, data: Array[PosColor]): BoardMut = {
    require(size>0)
    val board = new BoardMut(size, data, PlayerPair(PlayerSituation.empty, PlayerSituation.empty),
      data.map(_ => false), IndexedSeq())

    data.indices.foreach{ id =>
      if(data(id) != PosColor.Empty) board.placeAPiece(board.idToPos(id), data(id) == PosColor.Black)
    }
    board
  }

  def emptyBoard(size: Int) = {
    val data = Array.fill(size*size)(PosColor.Empty: PosColor)
    fromData(size, data)
  }

  def fromString(string: String) = {
    val s = string.trim.replace(" ", "")
    val lines = s.split("\n")
    val height = lines.length

    lines.foreach(l => require(l.length == height))

    val data = s.replace("\n","").map{
      case '@' => PosColor.Black
      case '*' => PosColor.White
      case '.' => PosColor.Empty
    }
    fromData(height, data.toArray)
  }
}

