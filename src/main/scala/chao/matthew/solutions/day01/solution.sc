import scala.annotation.tailrec
// with input.txt next to this filee:
val nums: Seq[Int] = scala.io.Source
  .fromFile(
    "/Users/matthew.chao/practice/aoc2021/src/main/scala/chao/matthew/solutions/day01/input.txt"
  )
  .getLines()
  .map(_.toInt)
  .toSeq

def countIncreases(nums: Seq[Int]): Int = {
  @tailrec
  def countIncreasesAcc(
      nums: Seq[Int],
      countSoFar: Int
  ): Int = nums match {
    case first :: second :: rest =>
      countIncreasesAcc(nums.tail, countSoFar + (if (first < second) 1 else 0))
    case _ => countSoFar
  }
  countIncreasesAcc(nums, 0)
}

countIncreases(nums)

def countWindowIncreases(nums: Seq[Int]): Int = {
  @tailrec
  def countWindowIncreasesAcc(
      nums: Seq[Int],
      countSoFar: Int
  ): Int = nums match {
    case first :: _ :: _ :: fourth :: rest =>
      countWindowIncreasesAcc(
        nums.tail,
        countSoFar + (if (first < fourth) 1 else 0)
      )
    case _ => countSoFar
  }
  countWindowIncreasesAcc(nums, 0)
}

countWindowIncreases(nums)

" 5".toInt
