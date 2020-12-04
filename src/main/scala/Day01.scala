object Day01I extends App {
  val input = io.Source.fromResource("Day01").getLines().toList.map(_.toInt)

  def check(input: List[Int]): List[Int] = {
    input match {
      case Nil => Nil
      case x :: tail =>
        tail.flatMap {
        case element if (element + x) == 2020 =>
          List(element, x)
        case _ => Nil
      } ++ check(tail)
    }
  }

  val result = check(input)
  println(input.product)

  val inputSet = input.toSet

  inputSet.foreach { i =>
    val complement = 2020 - i
    if (inputSet.contains(complement)) {
      println(s"complement = ${complement}")
      println(s"i = ${i}")
      println(complement * i)
    }
  }
}

object Day01II extends App {
  val input = io.Source.fromResource("Day01").getLines().toList.map(_.toInt)

  def bruteForce(input: List[Int]): Unit = {
    for(i <- 0 to input.length - 1) {
      for(j <- 1 to input.length - 1) {
        for(k <- 2 to input.length - 1) {
          val total = input(i) + input(j)+ input(k)
          if(total == 2020) {
            println(s"i = ${input(i)}")
            println(s"j = ${input(j)}")
            println(s"k = ${input(k)}")
          }
        }
      }
    }
  }

//  bruteForce(input)

  val inputSet = input.toSet

  def go(input: List[Int], inputSet: Set[Int]): Unit = {
    for (i <- 0 to input.length - 1) {
      for (j <- 1 to input.length - 1) {
        val complement = 2020 - input(i) - input(j)
        if (inputSet.contains(complement)) {
          println(s"complement = ${complement}")
          println(s"i = ${input(i)}")
          println(s"j = ${input(j)}")
          println(complement * input(i) * input(j))
        }
      }
    }
  }

  go(input, inputSet)

}

