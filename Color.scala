case class Color(r: Int, g: Int, b: Int) {
  def this(components: Array[Int]) {
    this(components(0), components(1), components(2))
  }

  override def equals(obj: Any): Boolean = obj match {
    case Color(r2: Int, g2: Int, b2: Int) =>
      r == r2 && g == g2 && b == b2
    case _ => false
  }

  override def toString: String = "(" + r + "," + g + "," + b + ")"

  def getLuminance: Int = (0.2126 * r + 0.7152 * g + 0.0722 * b).asInstanceOf[Int]
}
