package chao.matthew.solutions.day07

def median(seq: Seq[Int]): Int = {
  val sortedSeq = seq.sorted
  if (seq.size % 2 == 1) {
    sortedSeq(sortedSeq.size / 2)
  } else {
    val (up, down) = sortedSeq.splitAt(seq.size / 2)
    val med1 = up.last
    val med2 = down.head
    (med1 + med2) / 2
  }
}

def l1(seq: Seq[Int]): Int = {
  seq.map(Math.abs).sum
}

def avg(seq: Seq[Int]): (Int, Int) = {
  val x = seq.sum / seq.length
  (x, x + 1)
}

def p2(seq: Seq[Int], meetPoint: Int): Int = {
  def geomSer(dist: Int): Int = {
    (dist * (dist + 1)) / 2
  }
  seq.map(_ - meetPoint).map(Math.abs).map(geomSer).sum
}

object Solution {
  @main
  def solve(): Unit = {
    val seq = Read.seqFromInput
    println(l1(seq.map(_ - median(seq)))) // pt1
    val (avgLo, avgHi) = avg(seq)
    println(Math.min(p2(seq, avgLo), p2(seq, avgHi))) // pt2
  }
}

object Read {
  def seqFromInput: Seq[Int] = {
    scala.io.Source
      .fromFile(
        "/Users/matthew.chao/practice/aoc2021/src/main/scala/chao/matthew/solutions/day07/input.txt"
      )
      .getLines()
      .next()
      .split(',')
      .map(Integer.parseInt)
      .toSeq
  }
}
