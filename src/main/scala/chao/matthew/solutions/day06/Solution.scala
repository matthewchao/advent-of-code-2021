package chao.matthew.solutions.day06

import scala.annotation.tailrec

final val cycle = 7
private type School =
  Map[Int, Long] // (daysLeft (>= 0), # fish with daysLeft till spawning)

def merge(map1: Map[Int, Long], map2: Map[Int, Long]): Map[Int, Long] = {
  map1 ++ map2.map { case (k, v) => k -> (v + map1.getOrElse(k, 0L)) }

}
object School {
  def population(school: School): Long = {
    school.values.sum
  }
  @tailrec
  def simulate(school: School, days: Int): School = {
    days match {
      case 0 => school
      case fewDays if fewDays < cycle =>
        simulate(simulateOneDay(school), days - 1)
      case manyDays => simulate(simulateOneCycle(school), days - cycle)
    }
  }
  def simulateOneDay(school: School): School = {
    val numOffspring = school.getOrElse(0, 0L)
    val nonSpawningSchool = school.filterNot(_._1 == 0)
    val nonSpawningSchoolAged = nonSpawningSchool.map({
      case (daysLeft, population) =>
        (daysLeft - 1, population)
    })
    val justSpawned = Map(
      cycle - 1 -> school.getOrElse(0, 0L)
    ) // everything with key 0 now has key cycle+1
    merge(
      justSpawned,
      merge(nonSpawningSchoolAged, Map(cycle + 1 -> numOffspring))
    )
  }
  def simulateOneCycle(school: School): School = {
    // all fish with daysLeft < cycle remain the same; they also produce an identical number of spawn with daysLeft+2
    // all fish with daysLeft >= cycle age one cycle
    val (spawningSchool, nonSpawningSchool) = school.partition(_._1 < cycle)
    val newOffspring =
      spawningSchool.map((daysLeft, population) => (daysLeft + 2, population))
    val nonSpawningSchoolAged = nonSpawningSchool.map((daysLeft, population) =>
      (daysLeft - cycle, population)
    )
    merge(spawningSchool, merge(newOffspring, nonSpawningSchoolAged))
  }
}

object Solution {
  @main
  def solve1(): Unit = {
    val initialSchool = Read.schoolFromInput
    println("start with school size " + School.population(initialSchool))
    val days = 256 // part 2
    println(School.population(School.simulate(initialSchool, days)))
  }
}

object Read {
  def schoolFromInput: School = scala.io.Source
    .fromFile(
      "/Users/matthew.chao/practice/aoc2021/src/main/scala/chao/matthew/solutions/day06/input.txt"
    )
    .getLines()
    .next()
    .split(',')
    .map(Integer.parseInt)
    .foldLeft(Map.empty[Int, Long].withDefaultValue(0L))((school, days) =>
      school.updated(days, school(days) + 1)
    )
}
