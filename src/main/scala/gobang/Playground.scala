package gobang

import scala.util.Random

/**
  * Created by weijiayi on 05/12/2016.
  */
object Playground {
  def main(args: Array[String]): Unit = {
    val size = 15

    val b = BoardMut.emptyBoard(size)
    val r = RandomPlay.semiRandomPlay(b, Pos(size/2,size/2), currentIsBlack = true, maxTurn = 1000,
      new Random(3),verbose = false)
    println(s"result: $r")
    println(b)
  }
}
