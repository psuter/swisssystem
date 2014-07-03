package swisssystem

import scala.util.Try

trait Tournament[P] {
  def participants: Iterable[P]

  // Current standings, sorted by decreasing score, perf. rating, etc.
  def standings: Seq[P]

  def scores: Map[P,Int]

  // Performance rating, typically used to break ties for equal scores.
  def performances: Map[P,Int]

  // Pairings for the next round.
  def pairings : Try[Pairing[P]]

  def withResult(p1 : P, v1 : Int, p2 : P, v2 : Int) : Try[Tournament[P]]

  def withBye(p: P): Try[Tournament[P]]
}

object Tournament {
  // The initial order of participants is assumed to represent a priori
  // knowledge of strength, with the strongest participant first.
  def create[P](participants : Map[P,Int], rounds : Int, byeValue : Int) : Tournament[P] =
    Burstein.create[P](participants, rounds, byeValue)
}

