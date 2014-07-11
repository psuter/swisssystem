package swisssystem

object Common {
  def enumeratePairings[P](participants: List[P]): Stream[Pairing[P]] = {
    // Internal worker function, where floaters are represented as paired with themselves.
    def enum(pp: List[P]): Stream[List[(P,P)]] = pp match {
      case Nil => Nil #:: Stream.empty

      case p :: Nil => ((p,p) :: Nil) #:: Stream.empty

      case p1 :: ps =>
        val reg = for {
          (p2, i) <- ps.zipWithIndex.reverse.toStream
          s <- enum(ps.take(i) ++ ps.drop(i+1))
        } yield ((p1,p2) :: s)

        reg #::: (if(ps.length % 2 == 0) {
          enum(ps).map(_ ::: List((p1,p1)))
        } else {
          Stream.empty
        })
    }

    enum(participants) map { l =>
      val (pairs, fs) = l.partition(p => p._1 != p._2)
      Pairing[P](pairs, fs.headOption.map(_._1))
    }
  }

  // Pairings where an external floater cannot be defined as a floater again.
  // Note that the floater must also be part of the participants list.
  def enumeratePairings[P](participants: List[P], floater: Option[P]): Stream[Pairing[P]] = {
    floater.map { f =>
      enumeratePairings(participants).filter { p =>
        p.unpaired != floater
      }
    } getOrElse {
      enumeratePairings(participants)
    }
  }
}
