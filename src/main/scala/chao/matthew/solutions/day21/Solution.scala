package chao.matthew.solutions.day21

import scala.language.postfixOps

trait Die {
  def nextInt: (Int, Die)
  def timesRolled: Int
}

case class DeterministicDie(floor: Int, timesRolled: Int) extends Die {
  override def nextInt: (Int, Die) = {
    (floor + 1, DeterministicDie(floor + 1, timesRolled + 1))
  }
}

case class GameState(points1: Int, points2: Int, pos1: Int, pos2: Int) {}

case class Game(die: Die, gs: GameState, playerOneStarts: Boolean) {
  def nextMove: Game = {
    val (spaces1, die1) = die.nextInt;
    val (spaces2, die2) = die1.nextInt;
    val (spaces3, die3) = die2.nextInt;
    val spaces = spaces1 + spaces2 + spaces3;
    if (playerOneStarts) {
      val pos1 = (gs.pos1 + spaces) % 10
      val points1 = gs.points1 + (if (pos1 == 0) 10 else pos1)
      Game(die3, GameState(points1, gs.points2, pos1, gs.pos2), false)
    } else {
      val pos2 = (gs.pos2 + spaces) % 10
      val points2 = gs.points2 + (if (pos2 == 0) 10 else pos2)
      Game(die3, GameState(gs.points1, points2, gs.pos1, pos2), true)
    }
  }
}

object Solution {
  def main(args: Array[String]): Unit = {
    val n = 10 // size of board, also max point value
    val startPos1 = 10 % n
    val startPos2 = 2 % n
    val initGame =
      Game(DeterministicDie(0, 0), GameState(0, 0, startPos1, startPos2), true)
    def play(g: Game): Game = g.nextMove
    val games: LazyList[Game] = {
      def gamesAcc(game: Game): LazyList[Game] = {
        game #:: gamesAcc(game.nextMove)
      }
      gamesAcc(initGame)
    }
    val winThreshold1 = 1000
    val winGame = games
      .dropWhile(game =>
        game.gs.points1 < winThreshold1 && game.gs.points2 < winThreshold1
      )
      .head
    println("ending game: " + winGame)

    val diceMax = 3
    val diceMin = 1
    val threeDiceMax = 3 * diceMax
    val threeDiceMin = 3 * diceMin
    val winThreshold2 = 21
    val d =
      Array.ofDim[Long](winThreshold2 - 1 + n, winThreshold2 - 1 + n, n, n, 2)
    // d[pts1][pts2][pos1][pos2][is1sTurn] := the number of ways to reach the game state where player 1 has pts1 points, player 2 has pts2 points, player1 has position pos1, player2 has position pos2,
    // and is1sTurn==1 iff the next player who would go is player 1
    def dTotal(n1: Int, n2: Int, n3: Int, n4: Int, n5: Int): Long = {
      if (n1 < 0 || n2 < 0) {
        (0L)
      } else {
        d(n1)(n2)(n3)(n4)(n5)
      }
    }

    val waysToRollTotal = Array.ofDim[Int](threeDiceMax + 1)
    for {
      die1 <- diceMin to diceMax
      die2 <- diceMin to diceMax
      die3 <- diceMin to diceMax
    } {
      waysToRollTotal(die1 + die2 + die3) += 1
    }

    def p1JustWon(
        pts1: Int,
        pts2: Int,
        pos1: Int,
        pos2: Int,
        winThreshold: Int
    ): Boolean = {
      // assumes p1 just went
      val pointsJustEarned = if (pos1 == 0) n else pos1
      val prevPts1 = pts1 - pointsJustEarned
      (pts1 >= winThreshold) && (prevPts1 < winThreshold) && (pts2 < winThreshold)
    }
    def p2JustWon(
        pts1: Int,
        pts2: Int,
        pos1: Int,
        pos2: Int,
        winThreshold: Int
    ): Boolean = {
      // assumes p2 just went
      val pointsJustEarned = if (pos2 == 0) n else pos2
      val prevPts2 = pts2 - pointsJustEarned
      (pts2 >= winThreshold) && (prevPts2 < winThreshold) && (pts1 < winThreshold)
    }

    var wins1 = (0L);
    var wins2 = (0L);
    for {
      pts1 <- 0 until winThreshold2 + threeDiceMax
      pts2 <- 0 until winThreshold2 + threeDiceMax
      pos1 <- 0 until n
      pos2 <- 0 until n
      turn1 <- 0 to 1
    } {
      val isPlayer1sTurn = (turn1 == 1)
      if (
        (pts1 == 0) && (pts2 == 0) && (pos1 == startPos1) && (pos2 == startPos2) && isPlayer1sTurn
      ) {
        d(0)(0)(pos1)(pos2)(turn1) = 1 // base case
      } else if (isPlayer1sTurn) {
        // p2 just finished their turn
        var ways: Long = 0;
        for {
          dieTotal <- threeDiceMin to threeDiceMax
        } {
          val prevPos2 = (pos2 + (n - dieTotal)) % n
          val pointsJustEarned = if (pos2 == 0) n else pos2
          val prevPts2 = pts2 - pointsJustEarned

          ways =
            ways + (dTotal(pts1, prevPts2, pos1, prevPos2, 0) * waysToRollTotal(
              dieTotal
            ))
        }
        d(pts1)(pts2)(pos1)(pos2)(turn1) = ways
        if (p2JustWon(pts1, pts2, pos1, pos2, winThreshold2)) {
          wins2 += ways
        }
      } else {
        // isPlayer1sTurn == false
        // then p1 just finshed their turn
        var ways: Long = 0;
        for {
          dieTotal <- threeDiceMin to threeDiceMax
        } {
          val prevPos1 = (pos1 + (n - dieTotal)) % n
          val pointsJustEarned = if (pos1 == 0) n else pos1
          val prevPts1 = pts1 - pointsJustEarned

          ways = ways + (dTotal(prevPts1, pts2, prevPos1, pos2, 1) * (
            waysToRollTotal(dieTotal)
          ))
        }
        d(pts1)(pts2)(pos1)(pos2)(turn1) = ways
        if (p1JustWon(pts1, pts2, pos1, pos2, winThreshold2)) {
          wins1 += ways
        }

      }
    }
    println("wins1, wins2 = " + wins1 + ", " + wins2)
  }
}
