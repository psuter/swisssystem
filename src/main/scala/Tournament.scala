package swisssystem

import scala.util.Try

trait Tournament[P] {
  //def participants : Iterable[P]

  //def standings : Map[P,Int]

  //def rounds : Int

  //def withResult(p1 : P, v1 : Int, p2 : P, v2 : Int) : Try[Tournament[P]]

  //def pairings : Seq[(P,P)]

  //def bye : Option[P]
}

object Tournament {
  // The initial order of participants is assumed to represent a priori
  // knowledge of strength, with the strongest participant first.
  def create[P](participants : Map[P,Int], rounds : Int, byeValue : Int) : Tournament[P] =
    Burstein.create[P](participants, rounds, byeValue)
}

