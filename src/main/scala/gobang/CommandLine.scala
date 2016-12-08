package gobang

import scala.util.Random

/**
  * Play in command line mode
  */
object CommandLine {

  def main(args: Array[String]): Unit = {
//    playAGame(swap = true)
    compareAlgs()
  }

  // const vs inverse: 17 : 7
  // const vs second: 11 : 13
  // const vs cubit: 12 : 12
  // const 500 vs const 50: 9 : 1

  def compareAlgs(): Unit ={
    val seed = 1
    val p1 = new AIPlayer("AI_black_single", playsBlack = true,
      AIAlgorithm.ConstraintSpace(), AIAlgorithm.constantTurnWeight, sampleNum = 500, seed = seed, simulateOpp = false)

    val p2 = new AIPlayer("AI_white_50", playsBlack = false,
      AIAlgorithm.ConstraintSpace(), AIAlgorithm.constantTurnWeight, sampleNum = 250, seed = seed, simulateOpp = true)

//    val p2 = new AIPlayer("AI_white_cubic", playsBlack = false,
//      AIAlgorithm.ConstraintSpace(), AIAlgorithm.constantTurnWeight,
//      winFactorF = (x, y) => x*x*x-y*y*y, sampleNum = 200, seed = seed)

    val scores = compare(p1,p2, 5)
    println(s"Scores: $scores")
  }

  def playAGame(swap: Boolean = false): Unit = {
    var board = BoardMut.emptyBoard(13)
    var history = List[(BoardMut, Option[Pos])]()
    val seed = new Random().nextInt()

    val p1 = new CommandlinePlayer("player", playsBlack = true)

    val p2 = new AIPlayer("AI_white_inverse", playsBlack = false,
      AIAlgorithm.ConstraintSpace(), AIAlgorithm.secondInverseTurnWeight, sampleNum = 500, seed = seed)

    val players: IndexedSeq[GobangPlayer] =
      if(swap) IndexedSeq(p2,p1) else IndexedSeq(p1,p2)

    def endGame(msg: String): Unit = {
      println(board)
      println(msg)
      println("Game ended. Replay? (y/n/swap)")
      Console.in.readLine() match {
        case "y" | "yes" => playAGame(swap)
        case "swap" => playAGame(!swap)
        case "n" | "no" => println("Quit game.")
        case _ => endGame(msg)
      }
    }

    def playATurn(turn: Int, lastPos: Option[Pos]): Unit = {
      if(turn!=0 && board.interestRegion.isEmpty)
        endGame(" - * - Game ended as a draw! - * -")

      val player = players(turn % 2)
      println(s"---------- turn ${turn + 1} ----------")
      println(s"${player.name}'s turn.")
      println(board.draw(lastPos))

      player.makeDecision(board.duplicate) match {
        case GobangPlayerDecision.GiveUp =>
          endGame(s"$player gave up!")
        case GobangPlayerDecision.Unroll => history match {
          case _ :: (b,p) :: hs =>
            board = b
            history = hs
            playATurn(turn - 2, p)
          case _ =>
            println("No more history to unroll.")
            playATurn(turn, lastPos)
        }
        case GobangPlayerDecision.NextMove(pos) =>
          history = (board.duplicate, lastPos) :: history

          println(s"${player.name} placed a piece at $pos")
          board.placeAPiece(pos, player.playsBlack).winner match {
            case PosColor.Empty =>
              playATurn(turn + 1, Some(pos))
            case PosColor.Black => endGame(" - * - Black Won! - * -")
            case PosColor.White => endGame(" - * - White Won! - * -")
          }
      }
    }

    playATurn(0, None)
  }

  def compare(player1: AIPlayer, player2: AIPlayer, turns: Int): (Int, Int) = {
    def newBoard = BoardMut.emptyBoard(11)

    var board = newBoard
//    var history = List[(BoardMut, Option[Pos])]()

    var bigTurn = 0
    var blackWin, whiteWin = 0


    var swap = false
    val players: IndexedSeq[GobangPlayer] =
      if(swap) IndexedSeq(player2, player1) else IndexedSeq(player1, player2)

    def endGame(winner: PosColor): (Int, Int) = {
      println("------ Game End With Board: ----------")
      println(board)

      winner match {
        case PosColor.Empty =>
        case PosColor.White => whiteWin += 1
        case PosColor.Black => blackWin += 1
      }

      bigTurn += 1
      swap = !swap
      println(s"Big Scores: $blackWin vs $whiteWin (${bigTurn - blackWin - whiteWin} draws)")
      if(bigTurn == turns*2)
        return (blackWin, whiteWin)

      board = newBoard
      playATurn(0, None)
    }

    def playATurn(turn: Int, lastPos: Option[Pos]): (Int, Int) = {
      if(turn!=0 && board.interestRegion.isEmpty)
        endGame(PosColor.Empty)

      val player = players(turn % 2)
      println(s"---------- turn ${turn + 1} ----------")
      println(s"${player.name}'s turn.")
      println(board.draw(lastPos))

      player.makeDecision(board.duplicate) match {
        case GobangPlayerDecision.GiveUp =>
          ???
        case GobangPlayerDecision.Unroll =>
          ???
        case GobangPlayerDecision.NextMove(pos) =>
//          history = (board.duplicate, lastPos) :: history

          println(s"${player.name} placed a piece at $pos")
          board.placeAPiece(pos, player.playsBlack).winner match {
            case PosColor.Empty =>
              playATurn(turn + 1, Some(pos))
            case PosColor.Black => endGame(PosColor.Black)
            case PosColor.White => endGame(PosColor.White)
          }
      }
    }

    playATurn(0, None)
  }

}
