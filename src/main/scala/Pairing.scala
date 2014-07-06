package swisssystem

case class Pairing[P](pairs: List[(P,P)], unpaired: Option[P])
