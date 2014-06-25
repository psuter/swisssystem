package swisssystem
package test

import org.specs2.mutable._

import Common.{ Pairing, enumeratePairings }

class CommonTests extends Specification {
  "Enumeration of pairs for even participants" should {
    val ps: List[Int] = List(1, 2, 3, 4, 5, 6)

    val eps: Stream[Pairing[Int]] = Common.enumeratePairings(ps)

    val epsl: List[Pairing[Int]] = eps.toList

    "produce the correct number of pairings" in {
      epsl.length == 15
    }

    "produce pairings in the preferred order" in {
      epsl(0)  == Pairing[Int](List((1,6), (2,5), (3,4)), None) &&
      epsl(1)  == Pairing[Int](List((1,6), (2,4), (3,5)), None) &&
      epsl(2)  == Pairing[Int](List((1,6), (2,3), (4,5)), None) &&
      epsl(3)  == Pairing[Int](List((1,5), (2,6), (3,4)), None) &&
      epsl(4)  == Pairing[Int](List((1,5), (2,4), (3,6)), None) &&
      epsl(5)  == Pairing[Int](List((1,5), (2,3), (4,6)), None) &&
      epsl(6)  == Pairing[Int](List((1,4), (2,6), (3,5)), None) &&
      epsl(7)  == Pairing[Int](List((1,4), (2,5), (3,6)), None) &&
      epsl(8)  == Pairing[Int](List((1,4), (2,3), (5,6)), None) &&
      epsl(9)  == Pairing[Int](List((1,3), (2,6), (4,5)), None) &&
      epsl(10) == Pairing[Int](List((1,3), (2,5), (4,6)), None) &&
      epsl(11) == Pairing[Int](List((1,3), (2,4), (5,6)), None) &&
      epsl(12) == Pairing[Int](List((1,2), (3,6), (4,5)), None) &&
      epsl(13) == Pairing[Int](List((1,2), (3,5), (4,6)), None) &&
      epsl(14) == Pairing[Int](List((1,2), (3,4), (5,6)), None)
    }
  }

  "Enumeration of pairs for odd participants" should {
    val ps: List[Int] = List(1, 2, 3, 4, 5)

    val eps: Stream[Pairing[Int]] = Common.enumeratePairings(ps)

    val epsl: List[Pairing[Int]] = eps.toList

    "produce the correct number of pairings" in {
      epsl.length == 15
    }

    "produce pairings in the preferred order" in {
      epsl(0)  == Pairing[Int](List((1,5), (2,4)), Some(3)) &&
      epsl(1)  == Pairing[Int](List((1,5), (2,3)), Some(4)) &&
      epsl(2)  == Pairing[Int](List((1,5), (3,4)), Some(2)) &&
      epsl(3)  == Pairing[Int](List((1,4), (2,5)), Some(3)) &&
      epsl(4)  == Pairing[Int](List((1,4), (2,3)), Some(5)) &&
      epsl(5)  == Pairing[Int](List((1,4), (3,5)), Some(2)) &&
      epsl(6)  == Pairing[Int](List((1,3), (2,5)), Some(4)) &&
      epsl(7)  == Pairing[Int](List((1,3), (2,4)), Some(5)) &&
      epsl(8)  == Pairing[Int](List((1,3), (4,5)), Some(2)) &&
      epsl(9)  == Pairing[Int](List((1,2), (3,5)), Some(4)) &&
      epsl(10) == Pairing[Int](List((1,2), (3,4)), Some(5)) &&
      epsl(11) == Pairing[Int](List((1,2), (4,5)), Some(3)) &&
      epsl(12) == Pairing[Int](List((2,5), (3,4)), Some(1)) &&
      epsl(13) == Pairing[Int](List((2,4), (3,5)), Some(1)) &&
      epsl(14) == Pairing[Int](List((2,3), (4,5)), Some(1))
    }
  }
}
