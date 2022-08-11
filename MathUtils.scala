import garbage.Point

object MathUtils {
  def combinationPairs(list: List[Any]): List[(Any, Any)] = list match {
    case Nil => Nil
    case x :: xs => distribute(x, xs) ::: combinationPairs(xs)
  }

  def distribute(e: Any, list: List[Any]): List[(Any, Any)] = list match {
    case Nil => Nil
    case x :: xs => (e, x) :: distribute(e, xs)
  }

  def loopInInterval(bounds: (Int, Int), value: Int): Int =
    bounds._1 + ((((value - bounds._1) % (bounds._2 - bounds._1 + 1)) + (bounds._2 - bounds._1 + 1)) % (bounds._2 - bounds._1 + 1))


  def truncateToInterval(bounds: (Int, Int), value: Int): Int = {
    //assert(bounds._2>=bounds._1)
    Math.max(Math.min(bounds._2, value), bounds._1)
  }

  def bilinearInterpolation(point: Point, pixels: ((Point, Color), (Point, Color), (Point, Color), (Point, Color))): Color = {
    def calcLinearColor(inter: ((Int, Color), (Int, Color)), step: Int): Color = {
      val l: Double = inter._2._1 - inter._1._1
      val i: Double = step - inter._1._1
      val weight: (Double, Double) = ((l - i) / l, i / l)
      Color(((inter._1._2.r * weight._1) + (inter._2._2.r * weight._2)).asInstanceOf[Int],
        ((inter._1._2.g * weight._1) + (inter._2._2.g * weight._2)).asInstanceOf[Int],
        ((inter._1._2.b * weight._1) + (inter._2._2.b * weight._2)).asInstanceOf[Int])
    }

    val avg1 = calcLinearColor(((pixels._1._1._1, pixels._1._2), (pixels._2._1._1, pixels._2._2)), point._1)
    val avg2 = calcLinearColor(((pixels._3._1._1, pixels._3._2), (pixels._4._1._1, pixels._4._2)), point._1)
    calcLinearColor(((pixels._1._1._1, avg1), (pixels._3._1._1, avg2)), point._2)
  }

}
