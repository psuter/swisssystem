package swisssystem
package test

import org.specs2.mutable._

import scala.util.Random
import scala.util.Try

class TournamentTests extends Specification {
  // Realistic outcome for player 1 given ratings.
  def randOutcome(rand: Random, r1: Int, r2: Int): Int = {
    if(r1 < r2) {
      2 - randOutcome(rand, r2, r1)
    } else {
      val d = r1 - r2 // >= 0

      // Totally, like, scientific.
      val p = 0.5 + Math.tanh(d / 300.0) / 2.0
      // That, on the other hand, is completely made up.
      val drawProb = (1.0 - Math.tanh(d / 300.0)) * (2.0 - 2.0 * p)
      val winProb  = p - drawProb / 2.0

      val r = rand.nextDouble

      if(r <= drawProb) 1
      else if(r <= drawProb + winProb) 2
      else 0
    }
  }

  // Plays a tournament with players with a realistic rating distribution and believable outcomes.
  def runTournament(randSeed: Int, numPlayers: Int, numRounds: Int, byeValue: Int): Try[Tournament[String]] = {
    val rand = new Random(42)
    val names: List[String] = ('A' until ('A' + numPlayers).toChar).map(_.toString).toList
    val players: Map[String,Int] = names.map { n =>
      (n, (1700 + rand.nextGaussian() * 300).toInt)
    } toMap

    def playAllGamesAndByes(t: Tournament[String], pairings: Pairing[String]) = {
      val withBye = pairings.unpaired.fold(Try(t))(p => t.withBye(p))

      pairings.pairs.foldLeft(withBye) { (tt, p) =>
        val s1 = randOutcome(rand, players(p._1), players(p._2))
        val s2 = 2 - s1
        tt.flatMap { t => t.withResult(p._1, s1, p._2, s2) }
      }
    }


    val tStart = Try(Tournament.create(players, numRounds, byeValue))

    val tEnd = (0 until numRounds).foldLeft(tStart) { (tt, i) =>
      for {
        t  <- tt
        p  <- t.pairings
        tn <- playAllGamesAndByes(t, p)
      } yield tn
    }

    tEnd
  }

  "A tournament" should {
    "run smoothly 100x with odd number of players" in {
      for(i <- 0 until 100) {
        runTournament(424242 + i, 11, 7, 2) must beSuccessfulTry
      }
      true must_== true
    }

    "run smoothly 100x with even number of players" in {
      for(i <- 0 until 100) {
        runTournament(151049 + i, 30, 7, 2) must beSuccessfulTry
      }
      true must_== true
    }

    "run smoothly 100x with exactly enough players (even)" in {
      for(i <- 0 until 100) {
        runTournament(314159 + i, 8, 7, 2) must beSuccessfulTry
      }
      true must_== true
    }

    "run smoothly 100x with exactly enough players (odd)" in {
      for(i <- 0 until 100) {
        runTournament(314159 + i, 7, 7, 2) must beSuccessfulTry
      }
      true must_== true
    }

    "run smoothly 100x with one player extra" in {
      for(i <- 0 until 100) {
        runTournament(314159 + i, 9, 7, 2) must beSuccessfulTry
      }
      true must_== true
    }

    "fail consistently 100x with too few players (even)" in {
      for(i <- 0 until 100) {
        runTournament(279543 + i, 8, 8, 2) must beFailedTry
      }
      true must_== true
    }

    "fail consistently 100x with too few players (odd)" in {
      for(i <- 0 until 100) {
        runTournament(279543 + i, 7, 8, 2) must beFailedTry
      }
      true must_== true
    }
  }
}
