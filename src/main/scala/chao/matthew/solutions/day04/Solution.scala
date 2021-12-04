package chao.matthew.solutions.day04

import scala.annotation.tailrec

private type BingoBoard = Seq[Seq[Int]] // 5 lists containing 5 elements each

/** @param board
  *   the bingo board (never changes)
  * @param marks
  *   the locations (row,col) that have been marked already
  */
case class BingoGame(board: BingoBoard, marks: List[(Int, Int)]) {
  def wins: Boolean = {
    @tailrec
    def winsRec(
        remainingMarks: List[(Int, Int)],
        marksInRow: Map[Int, Int],
        marksInCol: Map[Int, Int]
    ): Boolean = {
      if ((marksInRow.values ++ marksInCol.values).exists(_ == dimension)) {
        true
      } else if (remainingMarks.isEmpty) {
        false
      } else {
        val (r, c) = remainingMarks.head
        winsRec(
          remainingMarks.tail,
          marksInRow.updated(r, marksInRow(r) + 1),
          marksInCol.updated(c, marksInCol(c) + 1)
        )
      }
    }
    winsRec(marks, Map.empty.withDefaultValue(0), Map.empty.withDefaultValue(0))
  }
  private val dimension: Int = board.length
  private val sumAll: Int = {
    (for {
      row <- board
      element <- row
    } yield element).sum
  }
  def sumUnmarked: Int = {
    val sumMarked = (for {
      (r, c) <- marks
    } yield board(r)(c)).sum
    sumAll - sumMarked
  }
  private def locate(num: Int): (Int, Int) = {
    val row: Int = board.indexWhere(_.exists(_ == num))
    (row, board(row).indexWhere(_ == num))
  }
  def markNewNumber(num: Int): BingoGame = {
    if (board.exists(_.exists(_ == num))) {
      BingoGame(board, locate(num) :: marks)
    } else {
      this
    }
  }
}

object Solution {

  @main
  def solve(): Unit = {
    val lines: Iterator[String] = scala.io.Source
      .fromFile(
        "/Users/matthew.chao/practice/aoc2021/src/main/scala/chao/matthew/solutions/day04/input.txt"
      )
      .getLines
    val numsToCall = ReadStuff.readBingoNumbers(lines)
    val games: Seq[BingoGame] = ReadStuff.readGames(lines)
    println(play(games, numsToCall)) // part 1
    println(play2(games, numsToCall))
  }

  @tailrec
  def play(games: Seq[BingoGame], numsToCall: Seq[Int]): Int = {
    val markedGames = games.map(_.markNewNumber(numsToCall.head))
    val winningGameOpt = markedGames.find(_.wins)
    winningGameOpt match {
      case None              => play(markedGames, numsToCall.tail)
      case Some(winningGame) => winningGame.sumUnmarked * numsToCall.head
    }
  }

  @tailrec
  def play2(games: Seq[BingoGame], numsToCall: Seq[Int]): Int = {
    // Input is such that there is unique answer, so play2 is only ever called with not-yet-won games
    if (games.length == 1) {
      play(games, numsToCall)
    } else {
      play2(
        games.map(_.markNewNumber(numsToCall.head)).filterNot(_.wins),
        numsToCall.tail
      )
    }
  }
}

object ReadStuff {
  def readBingoNumbers(it: Iterator[String]): Seq[Int] = {
    it.next.split(',').map(Integer.parseInt).toIndexedSeq
  }
  def readGames(it: Iterator[String]): Seq[BingoGame] = {
    var boards: Seq[BingoBoard] = Seq.empty
    while (it.hasNext) {
      it.next // consume empty line
      if (it.hasNext) {
        val board = readBoard(it)
        boards = board +: boards
      }
    }
    boards.map(BingoGame(_, List.empty))
  }
  def readBoard(it: Iterator[String]): BingoBoard = {
    var board: Seq[Seq[Int]] = List.empty
    1 to 5 foreach { _ =>
      val row: Seq[Int] =
        it.next.trim.split("\\s+").map(Integer.parseInt).toSeq
      board = board appended row
    }
    board
  }
}
