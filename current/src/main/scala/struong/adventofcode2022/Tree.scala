package struong.adventofcode2022

sealed trait Tree[+T] extends Product with Serializable
final case class Branch[+T](elem: T, children: List[Tree[T]]) extends Tree[T]
case object Leaf extends Tree[Nothing]