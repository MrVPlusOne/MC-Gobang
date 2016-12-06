package gobang

import scala.util.Random

/**
  * Play in command line mode
  */
object CommandLine {

  def main(args: Array[String]): Unit = {
    playAGame()
  }

  def playAGame(): Unit = {
    val board = BoardMut.emptyBoard(11)

    val p1 = new CommandlinePlayer("Jiayi", playsBlack = true)
//    val p1 = new AIPlayer("AI_black", playsBlack = true, sampleNum = 3000, new Random())
    val p2 = new AIPlayer("AI", playsBlack = false, sampleNum = 1000)
//    val p2 = new AIPlayer_1Step("AI", playsBlack = false, sampleNum = 100, new Random())
    val players = IndexedSeq[GobangPlayer](p1,p2)

    def playATurn(turn: Int, lastPos: Option[Pos]): Unit = {
      val player = players(turn % 2)
      println(s"---------- turn ${turn + 1} ----------")
      println(s"${player.name}'s turn.")
      println(board.draw(lastPos))

      val pos = player.decideWhereToMove(board.duplicate)
      println(s"Place piece at $pos")
      board.placePiece(pos, player.playsBlack).winner match {
        case PosState.Empty =>
          playATurn(turn + 1, Some(pos))
        case PosState.Black => endGame(board, " - * - Black Won! - * -")
        case PosState.White => endGame(board, " - * - White Won! - * -")
      }
    }

    playATurn(0, None)
  }



  def endGame(board: BoardMut, msg: String): Unit = {
    println(board)
    println(msg)
    println("Game ended. Replay? (y/n)")
    Console.in.readLine() match {
      case "y" | "yes" => playAGame()
      case "n" | "no" => println("Quit game.")
      case _ => endGame(board, msg)
    }
  }
}
