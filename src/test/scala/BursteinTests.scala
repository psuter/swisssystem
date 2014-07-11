package swisssystem
package test

import Common._

import scala.util.{ Try, Success, Failure }

import org.specs2.mutable._

class BursteinTests extends Specification {
  val players: Map[String,Int] = Map(
    "Mike"  -> 2600,
    "Joe"   -> 2400,
    "Perry" -> 2200,
    "Mark"  -> 2000
  )

  "This simple tournament" should {

    val t0 = Try(Burstein.create[String](players))

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

  "This tournament" should {
    val players = Map(
      "Alfred" -> 2000,
      "Bruce" -> 1800,
      "Carry" -> 2150,
      "Dieter" -> 1970,
      "Ernest" -> 2450,
      "Frank" -> 1509,
      "Grisha" -> 2590
    )

    val t1 = Try { Burstein.create(players) }

    "produce correct standings before the first round" in {
      t1.get.standings must_== List("Grisha", "Ernest", "Carry", "Alfred", "Dieter", "Bruce", "Frank")
    }

    "generate correct first round pairings" in {
      t1.get.pairings match {
        case Success(Pairing(pairs, bye)) =>
          bye must_== Some("Frank")
          pairs must_== List(("Grisha", "Bruce"), ("Dieter", "Ernest"), ("Carry", "Alfred"))

        case Failure(t) => throw t
      }
    }

    val t2 = for {
      ta <- t1
      tb <- ta.withResult("Grisha", 2, "Bruce", 0)
      tc <- tb.withResult("Dieter", 0, "Ernest", 2)
      td <- tc.withResult("Carry",  1, "Alfred", 1)
      te <- td.withBye("Frank", 2)
    } yield te

    "accept first round results" in { t2 must beSuccessfulTry }

    "produce correct standings after first round" in {
      t2.get.scores must_== Map("Alfred" -> 1, "Bruce" -> 0, "Carry" -> 1, "Dieter" -> 0, "Ernest" -> 2, "Frank" -> 2, "Grisha" -> 2)
      t2.get.standings must_== List("Grisha", "Ernest", "Frank", "Carry", "Alfred", "Dieter", "Bruce")
    }

    "generate correct second round pairings" in {
      t2.get.pairings match {
        case Success(Pairing(pairs, bye)) =>
          bye must_== Some("Bruce")
          pairs must_== List(("Frank", "Grisha"), ("Ernest", "Carry"), ("Alfred", "Dieter"))

        case Failure(t) => throw t
      }
    }

    val t3 = for {
      ta <- t2
      tb <- ta.withResult("Frank",  0, "Grisha", 2)
      tc <- tb.withResult("Ernest", 1, "Carry", 1)
      td <- tc.withResult("Alfred", 1, "Dieter", 1)
      te <- td.withBye("Bruce", 2)
    } yield te

    "accept second round results" in { t3 must beSuccessfulTry }

    "produce correct standings after second round" in {
      t3.get.scores must_== Map("Alfred" -> 2, "Bruce" -> 2, "Carry" -> 2, "Dieter" -> 1, "Ernest" -> 3, "Frank" -> 2, "Grisha" -> 4)
      t3.get.standings must_== List("Grisha", "Ernest", "Carry", "Alfred", "Bruce", "Frank", "Dieter")
    }

    "generate correct third round pairings" in {
      t3.get.pairings match {
        case Success(Pairing(pairs, bye)) =>
          bye must_== Some("Dieter")
          pairs must_== List(("Grisha", "Ernest"), ("Carry", "Frank"), ("Bruce", "Alfred"))

        case Failure(t) => throw t
      }
    }

    val t4 = for {
      ta <- t3
      tb <- ta.withResult("Grisha", 1, "Ernest", 1)
      tc <- tb.withResult("Carry",  2, "Frank",  0)
      td <- tc.withResult("Bruce",  0, "Alfred", 2)
      te <- td.withBye("Dieter", 2)
    } yield te

    "accept third round results" in { t4 must beSuccessfulTry }

    "produce correct standings after second round" in {
      t4.get.scores must_== Map("Alfred" -> 4, "Bruce" -> 2, "Carry" -> 4, "Dieter" -> 3, "Ernest" -> 4, "Frank" -> 2, "Grisha" -> 5)
      t4.get.standings must_== List("Grisha", "Ernest", "Carry", "Alfred", "Dieter", "Bruce", "Frank")
    }

    "generate correct fourth round pairings" in {
      t4.get.pairings match {
        case Success(Pairing(pairs, bye)) =>
          bye must_== Some("Alfred")
          //pairs must_== List(("Carry", "Grisha"), ("Ernest", "Bruce"), ("Dieter", "Frank"))
        
        case Failure(t) => throw t
      }
    }
  }
}
