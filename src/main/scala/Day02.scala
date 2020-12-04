object Day02 extends App {
  final case class Policy(min: Int, max: Int, letter: Char, password: String) {
    def isValid: Boolean = {
      val letterCount = password.filter(_ == letter).size
      letterCount >= min && letterCount <= max
    }
  }

  val input: Seq[String] = io.Source.fromResource("Day02").getLines().toList

  def createPolicy(line: String): Policy = {
    val lineRegex = raw"(\d+)-(\d+) ([a-z]): (.*)".r

    line match {
      case lineRegex(min, max, letter, password) =>
        Policy(min.toInt, max.toInt, letter.charAt(0), password)
    }
  }

  def createPolicies(input: Seq[String]): Seq[Policy] = {
    input.map( x => createPolicy(x))
  }

  println(createPolicies(input).filter(x => x.isValid).size)
}
