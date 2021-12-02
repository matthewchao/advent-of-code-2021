package chao.matthew.solutions.day02

enum Direction:
  case forward, down, up

case class Instruction(direction: Direction, magnitude: Int)

object Instruction {
  def fromString(instructionString: String): Instruction = {
    val (directionString, distanceString) =
      instructionString.splitAt(instructionString.indexWhere(_.isSpaceChar))
    Instruction(Direction.valueOf(directionString), distanceString.trim.toInt)
  }
}

case class Location(x: Int, y: Int)

case class LocationWithAim(location: Location, aim: Int)

object Solution {
  @main
  def solve(): Unit = {
    val instructions: Seq[Instruction] = scala.io.Source
      .fromFile(
        "/Users/matthew.chao/practice/aoc2021/src/main/scala/chao/matthew/solutions/day02/input.txt"
      )
      .getLines()
      .toSeq
      .map(Instruction.fromString)
    val Location(x1, y1) = instructions.foldLeft(Location(0, 0))(move)
    println(x1 * y1) // part 1
    val LocationWithAim(Location(x2, y2), _) =
      instructions.foldLeft(LocationWithAim(Location(0, 0), 0))(moveWithAim)
    println(x2 * y2) // part 2
  }

  def move(currLocation: Location, instruction: Instruction): Location =
    instruction match {
      case Instruction(Direction.forward, distance) =>
        Location(currLocation.x + distance, currLocation.y)
      case Instruction(Direction.down, distance) =>
        Location(currLocation.x, currLocation.y + distance)
      case Instruction(Direction.up, distance) =>
        Location(currLocation.x, currLocation.y - distance)
    }

  def moveWithAim(
      curr: LocationWithAim,
      instruction: Instruction
  ): LocationWithAim = instruction match {
    case Instruction(Direction.forward, distance) =>
      LocationWithAim(
        Location(
          curr.location.x + distance,
          curr.location.y + curr.aim * distance
        ),
        curr.aim
      )
    case Instruction(Direction.down, magnitude) =>
      LocationWithAim(curr.location, curr.aim + magnitude)
    case Instruction(Direction.up, magnitude) =>
      LocationWithAim(curr.location, curr.aim - magnitude)
  }
}
