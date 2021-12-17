package chao.matthew.solutions.day16

import scala.annotation.tailrec

trait Element {
  val bitLength: Int
  def toPacket: Packet
}

trait Packet extends Element {
  val bitLength: Int // # bits in the representations of this packet and all recursive subpackets
  val version: Int
  override def toPacket: Packet = this
}

object Packet {
  def sumAllVersions(p: Packet): Int = {
    p match {
      case l: LiteralPacket => l.version
      case p: ParentPacket  => p.version + p.children.map(sumAllVersions).sum
    }
  }
  val headerLength: Int = 6
  def evaluate(p: Packet): BigInt = p match
    case LiteralPacket(_, value) => BigInt(value, 2)
    case p: ParentPacket =>
      p.typeId match {
        case 0 => p.children.map(evaluate).sum
        case 1 => p.children.map(evaluate).product
        case 2 => p.children.map(evaluate).reduce(_.min(_))
        case 3 => p.children.map(evaluate).reduce(_.max(_))
        case 5 =>
          if (evaluate(p.children(0)) > evaluate(p.children(1))) 1 else 0
        case 6 =>
          if (evaluate(p.children(0)) < evaluate(p.children(1))) 1 else 0
        case 7 =>
          if (evaluate(p.children(0)) == evaluate(p.children(1))) 1 else 0
      }

}

case class LiteralPacket(version: Int, value: String) extends Packet {
  override val bitLength: Int = {
    val numGroups = value.length / 4
    assert(value.length % 4 == 0)
    Packet.headerLength + numGroups * 5
  }
}

trait ParentPacket extends Packet {
  val typeId: Int
  val children: Seq[Packet]
}

case class Parent0Packet(version: Int, children: Seq[Packet], typeId: Int)
    extends ParentPacket {
  override val bitLength: Int = {
    Parent0op.bitLength + children.map(_.bitLength).sum
  }
}

case class Parent1Packet(version: Int, children: Seq[Packet], typeId: Int)
    extends ParentPacket {
  override val bitLength: Int = {
    Parent1op.bitLength + children.map(_.bitLength).sum
  }
}

trait Op extends Element {
  override def toPacket: Packet = throw new ClassCastException
  val version: Int
  val bitLength: Int // the number of bits it takes to represent this op (without the children)
  val typeId: Int
}

case class Parent1op(version: Int, numChildren: Int, typeId: Int) extends Op {
  val bitLength: Int = Parent1op.bitLength
}

object Parent1op {
  val bitLength: Int =
    Packet.headerLength + 1 + 11
}

case class Parent0op(version: Int, lengthChildren: Int, typeId: Int)
    extends Op {
  val bitLength: Int = Parent0op.bitLength
}

object Parent0op {
  val bitLength: Int =
    Packet.headerLength + 1 + 15
}

object Parser {
  def isNextTokenLiteral(bitString: String): Boolean = {
    if (bitString.length < Packet.headerLength + 5) {
      false
    } else {
      val packetHeader = bitString.take(Packet.headerLength)
      val typeId = Integer.parseInt(packetHeader.drop(3), 2)
      typeId == 4
    }
  }
  def parseNextLiteral(bitString: String): LiteralPacket = {
    val packetHeader = bitString.take(Packet.headerLength)
    val version = Integer.parseInt(packetHeader.take(3), 2)
    val typeId = Integer.parseInt(packetHeader.drop(3), 2)
    assert(typeId == 4)
    def readGroups(s: String): String = {
      @tailrec
      def readGroupsAcc(s: String, bitString: String): String = {
        val newBitString = bitString ++ s.slice(1, 5)
        s(0) match {
          case '0' => newBitString
          case '1' => readGroupsAcc(s.drop(5), newBitString)
        }
      }
      readGroupsAcc(s, "")
    }
    LiteralPacket(
      version,
      readGroups(bitString.drop(Packet.headerLength))
    )
  }
  def isNextTokenOp(bitString: String): Boolean = {
    if (bitString.length < Math.min(Parent0op.bitLength, Parent1op.bitLength)) {
      false
    } else {
      val packetHeader = bitString.take(Packet.headerLength)
      val typeId = Integer.parseInt(packetHeader.drop(3), 2)
      val lengthTypeId = bitString.drop(Packet.headerLength).head
      if (typeId == 4) {
        false
      } else if (
        lengthTypeId == '0' && bitString.length < Parent0op.bitLength
      ) {
        false
      } else if (
        lengthTypeId == '1' && bitString.length < Parent1op.bitLength
      ) {
        false
      } else {
        true
      }
    }
  }
  def parseNextOp(bitString: String): Op = {
    assert(isNextTokenOp(bitString))
    val packetHeader = bitString.take(6)
    val lengthTypeId = bitString.drop(6).head
    val version = Integer.parseInt(packetHeader.take(3), 2)
    val typeId = Integer.parseInt(packetHeader.drop(3), 2)

    lengthTypeId match
      case '0' =>
        val lengthChildren = Integer.parseInt(
          bitString
            .take(Parent0op.bitLength)
            .drop(Packet.headerLength + 1),
          2
        )
        Parent0op(
          lengthChildren = lengthChildren,
          version = version,
          typeId = typeId
        )
      case '1' =>
        val numChildren = Integer.parseInt(
          bitString
            .take(Parent1op.bitLength)
            .drop(Packet.headerLength + 1),
          2
        )
        Parent1op(numChildren = numChildren, version = version, typeId)
  }
  def enoughPacketsForOp(op: Op, s: Seq[Element]): Boolean = {
    val topChildren = s.takeWhile(_ != op).map(_.toPacket)
    op match {
      case Parent0op(_, lengthChildren, _) =>
        assert(topChildren.map(_.bitLength).sum <= lengthChildren)
        topChildren.map(_.bitLength).sum == lengthChildren
      case Parent1op(_, numChildren, _) =>
        assert(topChildren.size <= numChildren)
        topChildren.size == numChildren
    }
  }

  def parse(bitString: String): Packet = {
    val origLength = bitString.length
    @tailrec
    def parseAcc(
        bitString: String,
        opStack: Seq[Op], // the clone of s containing only its Ops
        s: Seq[Element],
        packetsModified: Boolean
    ): Packet = {
      assert((s.map(_.bitLength).sum + bitString.length) == origLength)
      if (bitString.isEmpty) {
        assert(s.size == 1)
        assert(opStack.isEmpty)
        s(0).toPacket
      } else if (
        packetsModified && opStack.nonEmpty && enoughPacketsForOp(
          opStack.head,
          s
        )
      ) {
        val children: Seq[Packet] =
          s.takeWhile(_ != opStack.head)
            .map(_.toPacket)
            .reverse // order of children matters in part 2
        val parentOp = opStack.head
        val newPacket = parentOp match {
          case Parent0op(version, lengthChildren, typeId) =>
            assert(lengthChildren == children.map(_.bitLength).sum)
            Parent0Packet(version, children, typeId)
          case Parent1op(version, numChildren, typeId) =>
            assert(children.size == numChildren)
            Parent1Packet(version, children, typeId)
        }
        val newStack = newPacket +: s.drop(children.size).tail
        // pop from both
        parseAcc(bitString, opStack.tail, newStack, true)
      } else if (isNextTokenLiteral(bitString)) {
        val literalPacket = parseNextLiteral(bitString)
        val newStack = literalPacket +: s
        parseAcc(
          bitString.drop(literalPacket.bitLength),
          opStack,
          newStack,
          true
        )
      } else if (isNextTokenOp(bitString)) {
        val op = parseNextOp(bitString)
        val newOpStack = op +: opStack
        val newRemainingString = bitString.drop(op.bitLength)
        parseAcc(newRemainingString, op +: opStack, op +: s, false)
      } else {
        assert(opStack.isEmpty)
        assert(s.size == 1)
        s(0).toPacket
      }
    }
    parseAcc(bitString, List.empty, List.empty, false)
  }
}

object Solution {
  @main
  def solve(): Unit = {
    val hexString = Read.hexStringFromInput
    val bitString: String = hexString
      .map(ch => {
        "0000" + BigInt(String.valueOf(ch), 16).toString(2) takeRight 4
      })
      .mkString
    val packet = Parser.parse(bitString)
    println(Packet.sumAllVersions(packet)) // part 1
    println(Packet.evaluate(packet)) // part2
  }

}

object Read {
  def hexStringFromInput: String = {
    scala.io.Source
      .fromFile(
        "/Users/matthew.chao/practice/aoc2021/src/main/scala/chao/matthew/solutions/day16/input.txt"
      )
      .getLines()
      .next()
  }
}
