package swisssystem

import scala.util.Try

import Common._

import scala.collection.mutable.{ Map => MutableMap }

class Burstein[P] private(
  val players : Map[P,Int],
  val results: Map[(P,P),Int] = Map.empty[(P,P),Int],
  val colorHistories: Map[P,List[Boolean]] = Map.empty[P,List[Boolean]],
  val byed: Map[P,Int] = Map.empty[P,Int]
) extends Tournament[P] {
  lazy val participants : Iterable[P] = players.keySet.toList

  private val ordering: Ordering[P] = new Ordering[P] {
    def compare(p1: P, p2: P): Int = {
      val c1 = scores(p2) - scores(p1)
      if(c1 != 0) c1 else {
        val c2 = sonnebornBerger(p2) - sonnebornBerger(p1)
        if(c2 != 0) c2 else {
          val c3 = buchholtz(p2) - buchholtz(p1)
          if(c3 != 0) c3 else {
            val c4 = median(p2) - median(p1)
            if(c4 != 0) c4 else {
              players(p2) - players(p1)
            }
          }
        }
      }
    }
  }

  // Positive means played w/ white more often.
  lazy val colorDifferences: Map[P,Int] = participants map { p =>
    val hist = colorHistories.getOrElse(p, Nil)
    val w = hist.filter(identity).length
    (p, w - (hist.length - w))
  } toMap

  // Stores the color a player had in the last two assignments if it was the same
  // or None otherwise.
  lazy val lastTwoColors: Map[P,Option[Boolean]] = participants map { p =>
    val hist = colorHistories.getOrElse(p, Nil)
    (p, hist match {
      case c1 :: c2 :: _ if c1 == c2 => Some(c1)
      case _ => None
    })
  } toMap

  lazy val dueColors: Map[P,Option[Boolean]] = if(results.isEmpty) {
    standings.zipWithIndex map {
      case (p, i) => (p, Some(i % 2 == 0))
    } toMap
  } else {
    participants map { p =>
      val d = colorDifferences(p)
      (p, if(d != 0) {
        Some(d < 0)
      } else {
        colorHistories.getOrElse(p, Nil).headOption.map(!_)
      })
    } toMap
  }

  // Given a pairing, assign colors.
  // First return value is whether p1 gets white, second is how many players
  // get their due color.
  // This doesn't check the hard constraints. It assumes it has been done before.
  def assignColors(p1: P, p2: P): (Boolean,Int) = {
    val d1 = dueColors(p1)
    val d2 = dueColors(p2)

    (dueColors(p1), dueColors(p2)) match {
      case (None, None) =>
        // FIXME: this is not what the rules say. Check whether this actually
        // happens now? Shouldn't, except in weird cases with late
        // entrants.
        (true, 2)

      case (Some(c1), None) =>
        (c1, 2)

      case (None, Some(c2)) =>
        (!c2, 2)

      case (Some(c1), Some(c2)) if c1 != c2 =>
        (c1, 2)

      case (Some(c), Some(_)) =>
        val hs = colorHistories(p1) zip colorHistories(p2)
        hs dropWhile { p => p._1 == p._2 }
        hs.headOption.map { p =>
          (!p._1, 1)
        } getOrElse {
          val p1Better = (ordering.compare(p1, p2) <= 0)
          (c ^ !p1Better, 1)
        }
        
    }
  }

  lazy val standings: List[P] = participants.toList.sorted(ordering)

  lazy val scoreGroups: List[List[P]] = standings.groupBy(scores(_)).toList.sortBy(-_._1).map(_._2)

  lazy val opponents: Map[P,Set[P]] = participants map { p =>
    (p, results.keySet.filter(_._1 == p).map(_._2))
  } toMap

  lazy val scores: Map[P,Int] = participants map { p =>
    (p, {
      val scored = results collect {
        case ((p1,_), s) if p1 == p => s
      } sum

      val fromByes = byed.getOrElse(p, 0)

      scored + fromByes
    })
  } toMap

  lazy val sonnebornBerger: Map[P,Int] = participants map { p =>
    (p, results collect {
      case ((p1,p2), s) if p1 == p => s * scores(p2)
    } sum)
  } toMap

  lazy val performances: Map[P,Int] = sonnebornBerger

  lazy val median: Map[P,Int] = participants map { p =>
    val rs = results collect {
      case ((p1,_), s) if p1 == p => s
    } toList

    (p, rs.sorted.drop(1).dropRight(1).sum)
  } toMap

  lazy val buchholtz: Map[P,Int] = participants map { p =>
    (p, scores(p) * opponents(p).map(scores).sum)
  } toMap

  // Checks that the hard constraints would be enforced:
  //  - haven't played before
  //  - color constraints:
  //     - diff <= 2
  //     - not 3 times same col. in a row.
  def canPlay(p1: P, p2: P): Boolean = {
    !results.isDefinedAt((p1,p2)) &&
    colorDifferences(p1) < 2 &&
    colorDifferences(p2) > -2 &&
    lastTwoColors(p1).forall(x => !x) &&
    lastTwoColors(p2).forall(x => x)
  }

  def validPairings(sg: List[P], df: Option[P]): Stream[Pairing[P]] = {
    val valid = enumeratePairings(df.fold(sg)(f => f +: sg), df) filter { p =>
      p.pairs forall {
        case (p1,p2) => canPlay(p1,p2) || canPlay(p2,p1)
      }
    }

    val withCount = valid.map {
      case p @ Pairing(pairs, _) => (p, pairs.map(p => assignColors(p._1, p._2)._2).sum)
    } 

    val max = 2 * ((sg.length + df.size) / 2)
    (0 to max).reverse.toStream.flatMap { s =>
      withCount.filter(_._2 == s).map(_._1)
    }
  }

  // downfloater is not included in sgs.head
  case class CantUpward() extends Exception
  def mkPairings(scoreGroups: List[List[P]], downFloater: Option[P]): Stream[Pairing[P]] = {
    scoreGroups match {
      case Nil =>
        assert(downFloater.isEmpty)
        Pairing[P](Nil, None) #:: Stream.empty

      case sg :: Nil =>
        val vps = validPairings(sg, downFloater)
        if(vps.isEmpty) {
          throw CantUpward()
        }
        vps

      case sg :: sg2 :: sgs =>
        // here maybe filter downfloaters to eliminate those of who have played everyone below?
        val vps = validPairings(sg, downFloater) 
        if(vps.isEmpty) {
          mkPairings((sg ::: sg2) :: sgs, downFloater)
        } else {
          for {
            vp <- vps
            Pairing(pairs, df) = vp
            sp <- mkPairings(sg2 :: sgs, df)
            Pairing(sbpairs, df2) = sp
          } yield {
            Pairing(pairs ::: sbpairs, df2)
          }
        }
    }
  }

  def mkPairingsWrapped(scoreGroups: List[List[P]]): Stream[Pairing[P]] = {
    scoreGroups match {
      case Nil =>
        mkPairings(Nil, None)

      case sg :: Nil => try {
        mkPairings(scoreGroups, None)
      } catch {
        case CantUpward() =>
          //println("Had to give up. Group was: " + sg.mkString("\n"))
          throw CantUpward()
      }

      case sg :: sgs => try {
        mkPairings(scoreGroups, None)
      } catch {
        case CantUpward() =>
          mkPairingsWrapped(scoreGroups.dropRight(2) :+ scoreGroups.takeRight(2).flatten)
      }
    }
  }

  private def getByed(players: Set[P]): Option[P] = {
    if(players.size % 2 == 0) None else {
      // Can't be empty.
      val min = players.toList.map(p => byed.getOrElse(p, 0)).min
      standings.reverse.find(p => players(p) && byed.getOrElse(p, 0) == min)
    }  
  }

  def pairings: Try[Pairing[P]] = pairings(players.keySet)

  def pairings(active: Set[P]): Try[Pairing[P]] = {
    val bye: Option[P] = getByed(active)
    val notByed: Set[P] = active -- bye

    // The score groups with only the subset of players, and without the bye.
    val sgs = scoreGroups.map(_.filter(notByed))

    Try {
      val best = mkPairingsWrapped(sgs).head

      // Assign colors properly:
      val assignedPairs = best.pairs.map {
        case p @ (p1,p2) =>
          val (p1w, _) = assignColors(p1,p2)
          if(p1w) p else (p2, p1)
      }

      Pairing[P](assignedPairs, bye)
    }
  }

  def withResult(p1: P, s1: Int, p2: P, s2: Int): Try[Burstein[P]] = {
    val newResults = results + ((p1,p2) -> s1) + ((p2,p1) -> s2)
    val newColorHistories = {
      val h1 =  true :: colorHistories.getOrElse(p1, Nil)
      val h2 = false :: colorHistories.getOrElse(p2, Nil)
      colorHistories + (p1 -> h1) + (p2 -> h2)
    }

    Try {
      // TODO: check that the results were for a proper pairing
      assert(!results.isDefinedAt((p1,p2)) && !results.isDefinedAt((p2,p1)))
      new Burstein[P](players, newResults, newColorHistories, byed)
    }
  }

  def withBye(p: P, v: Int): Try[Burstein[P]] = {
    Try {
      val bb = byed.getOrElse(p, 0)
      new Burstein[P](players, results, colorHistories, byed + (p -> (bb+v)))
    }
  }

}

object Burstein {
  def create[P : Ordering](players: Set[P]) = new Burstein[P](
    players.toList.sorted.reverse.zipWithIndex.toMap,
    Map.empty,
    Map.empty,
    Map.empty
  )

  def create[P](players: Map[P,Int]) = new Burstein[P](
    players,
    Map.empty,
    Map.empty,
    Map.empty
  )
}
