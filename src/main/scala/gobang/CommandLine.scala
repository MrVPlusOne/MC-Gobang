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
    val board = BoardMut.emptyBoard(13)

    val p1 = new CommandlinePlayer("Jiayi", playsBlack = true)
    val p2 = new AIPlayer("AI", playsBlack = false, sampleNum = 3000, new Random())
    val players = IndexedSeq[GobangPlayer](p1,p2)

    def playATurn(turn: Int): Unit = {
      val player = players(turn % 2)
      println(s"---------- turn ${turn + 1} ----------")
      println(s"${player.name}'s turn.")

      val pos = player.decideWhereToMove(board.duplicate)
      println(s"Place piece at $pos")
      board.placePiece(pos, player.playsBlack).winner match {
        case PosState.Empty =>
          playATurn(turn + 1)
        case PosState.Black => endGame(" - * - Black Won! - * -")
        case PosState.White => endGame(" - * - White Won! - * -")
      }
    }

    playATurn(0)
  }



  def endGame(msg: String): Unit = {
    println(msg)
    println("Game ended. Replay? (y/n)")
    Console.in.readLine() match {
      case "y" | "yes" => playAGame()
      case "n" | "no" => println("Quit game.")
      case _ => endGame(msg)
    }
  }
}
