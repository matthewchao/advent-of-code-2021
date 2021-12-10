package chao.matthew.solutions.day10

import scala.annotation.tailrec

val pointValue: Map[Char, Int] = Map(
  (')', 3),
  (']', 57),
  ('}', 1197),
  ('>', 25137)
)

val pointValue2: Map[Char, Int] = Map(
  (')', 1),
  (']', 2),
  ('}', 3),
  ('>', 4)
)

val openers: Seq[Char] = List('(', '[', '{', '<')

val complement: Map[Char, Char] = Map(
  ('(', ')'),
  ('[', ']'),
  ('{', '}'),
  ('<', '>')
)

// (index of the first bad char, badChar) if corrupt, or (-1, stack at end) if none is found)
def firstCorrupt(s: String): (Int, String) = {
  @tailrec
  def firstCorruptAcc(
      s: String,
      stack: String,
      numProcessedBefore: Int
  ): (Int, String) = {
    if (s.isEmpty) {
      (-1, stack)
    } else if (openers.contains(s.head)) {
      firstCorruptAcc(s.tail, s.head +: stack, numProcessedBefore + 1)
    } else if (stack.nonEmpty && s.head == complement(stack.head)) {
      firstCorruptAcc(
        s.tail,
        stack.tail, // pop stack
        numProcessedBefore + 1
      )
    } else {
      (numProcessedBefore, String.valueOf(s.head)) // wrong one found
    }
  }
  firstCorruptAcc(s, "", 0)
}

def scoreClose(stack: String): Long = {
  @tailrec
  def scoreCloseAcc(stack: String, currScore: Long): Long = {
    if (stack.isEmpty) {
      currScore
    } else {
      scoreCloseAcc(
        stack.tail,
        5L * currScore + pointValue2(complement(stack.head))
      )
    }
  }
  scoreCloseAcc(stack, 0L)
}

object Solution {
  @main
  def solve(): Unit = {
    val lines = Read.seqFromInput
    val (p1, p2) = lines.map(firstCorrupt).partition(_._1 != -1)
    val ans1 = p1.map(_._2(0)).map(pointValue).sum
    val ans2 = p2.map(_._2).map(scoreClose).sorted.apply(p2.length / 2)
    println(ans1)
    println(ans2)
  }
}

object Read {
  def seqFromInput: Seq[String] = {
    scala.io.Source
      .fromFile(
        "/Users/matthew.chao/practice/aoc2021/src/main/scala/chao/matthew/solutions/day10/input.txt"
      )
      .getLines()
      .toSeq
  }
}
