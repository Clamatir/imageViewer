package object garbage {
  type Point = (Int, Int)
  type Coords = (Point, Point)
}

import garbage.{Coords, Point}

import scala.annotation.tailrec

object GeomUtils {
  def containsPoint(coords: Coords, point: Point): Boolean = {
    (coords._1._1 <= point._1) && (coords._1._2 <= point._2) &&
      (coords._2._1 >= point._1) && (coords._2._2 >= point._2)
  }

  def containsSection(contains: ((Int, Int), (Int, Int)), contained: ((Int, Int), (Int, Int))): Boolean = {
    containsPoint(contains, contained._1) &&
      containsPoint(contains, contained._2)
  }

  def minus(p1: Point, p2: Point): Point = plus(p1, (-p2._1, -p2._2))

  def plus(p1: Point, p2: Point): Point = (p1._1 + p2._1, p1._2 + p2._2)

  def multiply(point: Point, factor: Int): Point = (point._1 * factor, point._2 * factor)

  def closestPointTo(reference: Point, candidates: List[Point]): Point = {
    getSquaredDistances(candidates.map(minus(_, reference))).min._2
  }

  def furthestPointTo(reference: Point, candidates: List[Point]): Point = {
    getSquaredDistances(candidates.map(minus(_, reference))).max._2
  }

  def getSquaredDistances(list: List[Point]): List[(Int, Point)] = {
    @tailrec
    def tailrecursiveBody(list: List[Point], acc: List[(Int, Point)]): List[(Int, Point)] = list match {
      case Nil => acc
      case point :: xs => tailrecursiveBody(xs, (point._1 * point._1 + point._2 * point._2, point) :: acc)
    }

    tailrecursiveBody(list, Nil)
  }

  def min(p1: Point, p2: Point): Point = getSquaredDistances(p1 :: p2 :: Nil).min._2

  def max(p1: Point, p2: Point): Point = getSquaredDistances(p1 :: p2 :: Nil).max._2

  def arePointsEqual(p1: Point, p2: Point): Boolean = p1._1 == p2._1 && p1._2 == p2._2

  def splitIntoQuadrants(coords: Coords): (Option[Coords], Option[Coords], Option[Coords], Option[Coords]) = {
    val dim: Point = GeomUtils.getDifferences(coords)
    val x0: Int = coords._1._1
    val y0: Int = coords._1._2

    def calculateQuadrants(x0: Int, y0: Int, width: Int, height: Int): (Coords, Coords, Coords, Coords) = {
      val quad1 = ((x0, y0), (x0 + width / 2, y0 + height / 2))
      val quad2 = ((x0 + (width / 2) + 1, y0), (x0 + width, y0 + height / 2))
      val quad3 = ((x0, y0 + (height / 2) + 1), (x0 + width / 2, y0 + height))
      val quad4 = ((x0 + (width / 2) + 1, y0 + (height / 2) + 1), (x0 + width, y0 + height))
      (quad1, quad2, quad3, quad4)
    }

    if (dim._1 + 1 == 2 * dim._2 + 2) {
      val res = calculateQuadrants(x0, y0, dim._1, dim._1)
      (Some(res._1), Some(res._2), None, None)
    }
    else if (2 * dim._1 + 2 == dim._2 + 1) {
      val res = calculateQuadrants(x0, y0, dim._2, dim._2)
      (Some(res._1), None, Some(res._3), None)
    }
    else {
      val res = calculateQuadrants(x0, y0, dim._1, dim._2)
      (Some(res._1), Some(res._2), Some(res._3), Some(res._4))
    }
  }

  def isValidSection(coords: Coords): Boolean = equals(coords._1, min(coords._1, coords._2))

  def intersects(c1: Coords, c2: Coords): Boolean = isValidSection(max(c1._1, c2._1), min(c1._2, c2._2))

  def intersect(c1: Coords, c2: Coords): Option[Coords] = Some((max(c1._1, c2._1), min(c1._2, c2._2))).filter(isValidSection)

  def moveLeftUpperConnerTo(coords: Option[Coords], point: Point): Option[Coords] = coords match {
    case None => None
    case Some(coords: Coords) =>
      val xdif = coords._2._1 - coords._1._1
      val ydif = coords._2._2 - coords._1._2
      Some(point, plus(point, (xdif, ydif)))
  }

  def moveRightUpperConnerTo(coords: Option[Coords], point: Point): Option[Coords] = coords match {
    case None => None
    case Some(coords: Coords) =>
      val xdif = coords._2._1 - coords._1._1
      val ydif = coords._2._2 - coords._1._2
      deslocate(Some(((-xdif, 0), (0, ydif))), point)
  }

  def moveLeftDownerConnerTo(coords: Option[Coords], point: Point): Option[Coords] = coords match {
    case None => None
    case Some(coords: Coords) =>
      val xdif = coords._2._1 - coords._1._1
      val ydif = coords._2._2 - coords._1._2
      deslocate(Some(((0, -ydif), (xdif, 0))), point)
  }

  def moveRightDownerConnerTo(coords: Option[Coords], point: Point): Option[Coords] = coords match {
    case None => None
    case Some(coords: Coords) =>
      val xdif = coords._2._1 - coords._1._1
      val ydif = coords._2._2 - coords._1._2
      Some(plus(point, (-xdif, -ydif)), point)
  }

  def translate(coords: Option[Coords], point: Point, moveMethod: (Coords, Point) => Coords): Option[Coords] = coords match {
    case None => None
    case Some(coords: Coords) =>
      val movedToOrigin = moveMethod(coords, (0, 0))
      deslocate(Some(movedToOrigin), point)
  }

  def deslocate(coords: Option[Coords], point: Point): Option[Coords] = coords match {
    case None => None
    case Some(c: Coords) => Some(plus(c._1, point), plus(c._2, point))
  }

  def snapToArea(point: Point, coords: Option[Coords]): Point = coords match {
    case None => point
    case Some(coords: Coords) => (MathUtils.truncateToInterval((coords._1._1, coords._2._1), point._1), MathUtils.truncateToInterval((coords._1._2, coords._2._2), point._2))
  }

  def iterateInArea(point: Point, coords: Coords): Point = {
    if (point._1 < coords._2._1)
      plus(point, (1, 0))
    else (0, plus(point, (0, 1))._2)
  }

  def expandAreaToInclude(coords: Coords, point: Point): Coords = {
    ((math.min(coords._1._1, point._1), math.min(coords._1._2, point._2)), (math.max(coords._2._1, point._1), math.max(coords._2._2, point._2)))
  }

  def union(c1: Option[garbage.Coords], c2: Option[Coords]): Option[Coords] = {
    c1 match {
      case None => c2 match {
        case None => None
        case Some(coords: Coords) => Some(coords)
      }
      case Some(c1: Coords) => c2 match {
        case None => Some(c1)
        case Some(c2: garbage.Coords) => Some(expandAreaToInclude(expandAreaToInclude(c1, c2._1), c2._2))
      }
    }
  }

  def corners(coords: Coords): (Point, Point, Point, Point) = {
    val opposite = formatWithOppositeCoords(coords)
    (coords._1, opposite._1, opposite._2, coords._2)
  }

  def formatWithOppositeCoords(coords: Coords): Coords = ((coords._2._1, coords._1._2), (coords._1._1, coords._2._2))

  def getCorner(corner: Int, coords: Coords): Point = corner match {
    case 1 => coords._1
    case 2 => GeomUtils.formatWithOppositeCoords(coords)._1
    case 3 => GeomUtils.formatWithOppositeCoords(coords)._2
    case 4 => coords._2
  }

  def getDifferences(coords: Coords): Point = moveLeftUpperConnerTo(Some(coords), (0, 0)) match {
    case Some(coords: Coords) => coords._2
  }

  def isPointPreviousToArea(coords: Coords, point: Point): Boolean = {
    point._2 < coords._1._2 || (point._2 == coords._1._2 && point._1 < coords._1._1)
  }

  def isPointAfterArea(coords: Coords, point: Point): Boolean = {
    point._2 > coords._2._2 || (point._2 == coords._2._2 && point._1 > coords._2._1)
  }

  def multiply(point: Point, xfactor: Float, yfactor:Float): Point = ((point._1 * xfactor).asInstanceOf[Int], (point._2 * yfactor).asInstanceOf[Int])

}
