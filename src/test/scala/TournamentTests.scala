package swisssystem
package test

import org.specs2.mutable._

import scala.util.Random

class TournamentTests extends Specification {
  val rand = new Random(42)

  val players: Map[String,Int] = ('A' to 'K').map(_.toString).map { n =>
    (n, (1700 + rand.nextGaussian() * 300).toInt)
  }

    
}
