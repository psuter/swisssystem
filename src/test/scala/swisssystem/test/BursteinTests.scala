package swisssystem
package test

import scala.util.{ Try, Success, Failure }

import org.specs2.mutable._

class BursteinTests extends Specification {
  "This simple tournament" should {
    val players: Map[String,Int] = Map(
      "Mike"  -> 2600,
      "Joe"   -> 2400,
      "Perry" -> 2200,
      "Mark"  -> 2000
    )

    val t0 = Try(Burstein.create[String](players, 2, 1))

    "compute standings correctly before first round" in {
      t0.get.standings.toList == List("Mike", "Joe", "Perry", "Mark")
    }

    val t1 = for {
      t0a <- t0
      t0b <- t0a.withResult("Mike", 2, "Joe", 0)
      t0c <- t0b.withResult("Perry", 1, "Mark", 1)
    } yield t0c

    "admit results" in { t1.isSuccess }

    "compute scores correctly after first round" in {
      val tx = t1.get

      tx.scores("Mike")           == 2 &&
      tx.scores("Joe")            == 0 &&
      tx.scores("Perry")          == 1 &&
      tx.scores("Mark")           == 1 &&
      tx.sonnebornBerger("Mike")  == 0 &&
      tx.sonnebornBerger("Joe")   == 0 &&
      tx.sonnebornBerger("Perry") == 1 &&
      tx.sonnebornBerger("Mark")  == 1 &&
      tx.buchholtz("Mike")        == 0 &&
      tx.buchholtz("Joe")         == 0 &&
      tx.buchholtz("Perry")       == 1 &&
      tx.buchholtz("Mark")        == 1
    }

    "compute standings correctly after first round" in {
      t1.get.standings.toList == List("Mike", "Perry", "Mark", "Joe")
    }

    val t2 = for {
      t1a <- t1
      t1b <- t1a.withResult("Mike", 2, "Perry", 0)
      t1c <- t1b.withResult("Mark", 0, "Joe", 2)
    } yield t1c

    "compute scores correctly after second round" in {
      val tx = t2.get

      tx.scores("Mike")           ==  4 &&
      tx.scores("Joe")            ==  2 &&
      tx.scores("Perry")          ==  1 &&
      tx.scores("Mark")           ==  1 &&
      tx.sonnebornBerger("Mike")  ==  6 &&
      tx.sonnebornBerger("Joe")   ==  2 &&
      tx.sonnebornBerger("Perry") ==  1 &&
      tx.sonnebornBerger("Mark")  ==  1 &&
      tx.buchholtz("Mike")        == 12 &&
      tx.buchholtz("Joe")         == 10 &&
      tx.buchholtz("Perry")       ==  5 &&
      tx.buchholtz("Mark")        ==  3
    }

    "compute standings correctly after second round" in {
      t2.get.standings.toList == List("Mike", "Joe", "Perry", "Mark")
    }
  }
}
