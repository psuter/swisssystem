package swisssystem

import scala.util.Try

class Burstein[P] private(
  val players : Map[P,Int],
  val rounds : Int,
  val byeValue : Int,
  val results: Map[(P,P),Int] = Map.empty[(P,P),Int],
  val colorHistories: Map[P,List[Boolean]] = Map.empty[P,List[Boolean]],
  val byed: Set[P] = Set.empty[P]
) extends Tournament[P] {
  val participants : Iterable[P] = players.keySet.toList

  private val ordering: Ordering[P] = new Ordering[P] {
    def compare(p1: P, p2: P): Int = {
      val c1 = scores(p2) - scores(p1)
      if(c1 != 0) c1 else {
        val c2 = sonnebornBerger(p2) - sonnebornBerger(p1)
        if(c2 != 0) c2 else {
          val c3 = buchholtz(p2) - buchholtz(p1)
          if(c3 != 0) c3 else {
            players(p2) - players(p1)
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

  lazy val dueColors: Map[P,Option[Boolean]] = participants map { p =>
    val d = colorDifferences(p)
    (p, if(d != 0) {
      Some(d < 0)
    } else {
      colorHistories(p).headOption.map(!_)
    })
  } toMap

  lazy val standings: List[P] = participants.toList.sorted(ordering)

  lazy val opponents: Map[P,Set[P]] = participants map { p =>
    (p, results.keySet.filter(_._1 == p).map(_._2))
  } toMap

  lazy val scores: Map[P,Int] = participants map { p =>
    (p, results collect {
      case ((p1,_), s) if p1 == p => s
    } sum)
  } toMap

  lazy val sonnebornBerger: Map[P,Int] = participants map { p =>
    (p, results collect {
      case ((p1,p2), s) if p1 == p => s * scores(p2)
    } sum)
  } toMap

  lazy val buchholtz: Map[P,Int] = participants map { p =>
    (p, scores(p) * opponents(p).map(scores).sum)
  } toMap

  def withResult(p1: P, s1: Int, p2: P, s2: Int): Try[Burstein[P]] = {
    val newResults = results + ((p1,p2) -> s1) + ((p2,p1) -> s2)
    val newColorHistories = {
      val h1 =  true :: colorHistories.getOrElse(p1, Nil)
      val h2 = false :: colorHistories.getOrElse(p2, Nil)
      colorHistories + (p1 -> h1) + (p2 -> h2)
    }    

    Try {
      // TODO: check that the results were for a proper pairing
      new Burstein[P](players, rounds, byeValue, newResults, newColorHistories)
    }
  }

  def withBye(p: P): Try[Burstein[P]] = {
    Try {
      // TODO check that the player hasn't been byed before
      new Burstein[P](players, rounds, byeValue, results, colorHistories, byed + p)
    }
  }
}

object Burstein {
  def create[P : Ordering](players: Set[P], rounds: Int, byeValue: Int) = new Burstein[P](
    players.toList.sorted.reverse.zipWithIndex.toMap,
    rounds,
    byeValue,
    Map.empty,
    Map.empty,
    Set.empty
  )

  def create[P](players: Map[P,Int], rounds: Int, byeValue: Int) = new Burstein[P](
    players,
    rounds,
    byeValue,
    Map.empty,
    Map.empty,
    Set.empty
  )
}
