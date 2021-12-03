package chao.matthew.solutions.day03

import scala.collection.immutable.BitSet

case class LittleEndianBitVector(
    bits: IndexedSeq[Boolean]
) // e.g., LittleEndianBitVector(false,false,true) represents the decimal 4
object LittleEndianBitVector {
  def fromString(s: String): LittleEndianBitVector = {
    LittleEndianBitVector(s.reverse.map(_ == '1'))
  }
}

object Solution {
  @main
  def solve(): Unit = {
    val bitvectors: Seq[LittleEndianBitVector] = scala.io.Source
      .fromFile(
        "/Users/matthew.chao/practice/aoc2021/src/main/scala/chao/matthew/solutions/day03/input.txt"
      )
      .getLines()
      .toSeq
      .map(LittleEndianBitVector.fromString)

    val finalCount: Map[Int, Int] =
      bitvectors.foldLeft(Map.empty[Int, Int])(updateCount)

    val (bs1, bs2) = toBitsets(finalCount)
    val (gamma, epsilon) = (bs1.toBitMask(0), bs2.toBitMask(0))
    println(gamma * epsilon) // part 1

    var idx = bitvectors(0).bits.size - 1
    var oxygenVectors = bitvectors
    while (oxygenVectors.size > 1) {
      val count = oxygenVectors.foldLeft(Map.empty[Int, Int])(updateCount)
      val moreCommon = (count.applyOrElse(idx, _ => 0) >= 0)
      oxygenVectors = filterByIndex(oxygenVectors, idx, _ == moreCommon)
      idx -= 1
    }
    idx = bitvectors(0).bits.size - 1
    var co2Vectors = bitvectors
    while (co2Vectors.size > 1) {
      val count = co2Vectors.foldLeft(Map.empty[Int, Int])(updateCount)
      val lessCommon = (count.applyOrElse(idx, _ => 0) < 0)
      co2Vectors = filterByIndex(co2Vectors, idx, _ == lessCommon)
      idx -= 1
    }
    println(toLong(oxygenVectors(0)) * toLong(co2Vectors(0))) // part2
  }

  def filterByIndex(
      bitvectors: Seq[LittleEndianBitVector],
      idx: Int,
      pred: Boolean => Boolean
  ): Seq[LittleEndianBitVector] = {
    bitvectors.filter(bv => {
      pred(bv.bits(idx))
    })
  }

  def updateCount(
      currCount: Map[Int, Int],
      bitvector: LittleEndianBitVector
  ): Map[Int, Int] = {
    bitvector.bits.zipWithIndex.foldLeft(currCount)(
      (count: Map[Int, Int], pair) => {
        val (isBitSet, idx) = pair
        count.updated(
          idx,
          count.applyOrElse(idx, _ => 0) + { if (isBitSet) 1 else -1 }
        )
      }
    )
  }

  def toBitsets(bitSetCount: Map[Int, Int]): (BitSet, BitSet) = {
    bitSetCount.foldLeft(BitSet(), BitSet())((bitsets, pr) => {
      val (idx, count) = pr
      val (bs1, bs2) = bitsets
      if (count >= 0) {
        // more (or an equal number of) 1s than 0s at this index
        (bs1.incl(idx), bs2)
      } else {
        (bs1, bs2.incl(idx))
      }
    })
  }

  def toLong(bitVector: LittleEndianBitVector): Long = {
    var ret: Long = 0
    var twoPower = 1
    bitVector.bits.foreach(bit => {
      if (bit) {
        ret += twoPower
      }
      twoPower *= 2
    })
    ret
  }
}
