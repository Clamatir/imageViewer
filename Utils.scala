
import scala.annotation.tailrec

class Utils {
  type Point = (Int, Int)
  type Coords = (Point, Point)
  type Section = (Coords, Color)
  type Bitmap = List[List[Color]]
  type Data = Array[Array[Int]]

  trait QTree[+A]

  case class QNode[A](value: A,
                      one: QTree[A],
                      two: QTree[A],
                      three: QTree[A],
                      four: QTree[A]) extends QTree[A]

  case class QLeaf[A, B](value: B) extends QTree[A]

  case object QEmpty extends QTree[Nothing]

  //Bit map methods
  def dataToBitMap(data: Data): List[List[Color]] = data.map(_.map(element => new Color(ImageUtil.decodeRgb(element))).toList).toList

  def BitmapToData(bitmap: Bitmap): Array[Array[Int]] = bitmap.map(_.map(color => ImageUtil.encodeRgb(color.r, color.g, color.b)).toArray).toArray

  def getColor(bitmap: Bitmap, point: Point): Color = bitmap(point._2)(point._1)

  //Quad Tree methods
  def BitMapToQtree(bitmap: Bitmap): QTree[Coords] = {
    def inner(cursor: Point, boundaries: Option[Coords], previous: Option[Color]): QTree[Coords] = boundaries match {
      case None => QEmpty
      case Some(coords: Coords) =>
        if (!GeomUtils.containsPoint(coords, cursor)) {
          inner(GeomUtils.iterateInArea(cursor, coords), boundaries, previous)
        } else {
          val newColor = getColor(bitmap, cursor)
          if (previous match {
            case Some(color: Color) => !newColor.equals(color)
            case None => false
          }) {
            val quadrants = GeomUtils.splitIntoQuadrants(coords)
            val leaf1 = inner(GeomUtils.minus(GeomUtils.snapToArea(cursor, quadrants._1), (1, 0)), quadrants._1, None)
            val leaf2 = inner(GeomUtils.minus(GeomUtils.snapToArea(cursor, quadrants._1), (1, 0)), quadrants._2, None)
            val leaf3 = inner(GeomUtils.minus(GeomUtils.snapToArea(cursor, quadrants._1), (1, 0)), quadrants._3, None)
            val leaf4 = inner(GeomUtils.minus(GeomUtils.snapToArea(cursor, quadrants._1), (1, 0)), quadrants._4, None)
            QNode(coords, leaf1, leaf2, leaf3, leaf4)
          }
          else if (GeomUtils.arePointsEqual(cursor, coords._2)) {
            QLeaf((coords, newColor))
          } else
            inner(GeomUtils.iterateInArea(cursor, coords), Some(coords), Some(newColor))
        }
    }

    val bitmapCoords = ((0, 0), (bitmap.head.size - 1, bitmap.size - 1))
    inner((0, 0), Some(bitmapCoords), None)
  }

  def QTreeToBitMap(qt: QTree[Coords]): Bitmap = {
    def recursiveAction(qt: QTree[Coords]): Bitmap = {
      qt match {
        case QLeaf((coords: Coords, color: Color)) =>
          val translatedToOrigin = GeomUtils.moveLeftUpperConnerTo(Some(coords), (0, 0)) match {
            case Some(coords: Coords) => coords
          }
          monoColoredBitMap(translatedToOrigin._2._1 + 1, translatedToOrigin._2._2 + 1, color)
        case QNode(_: Coords, one: QTree[Coords], two: QTree[Coords], three: QTree[Coords], four: QTree[Coords]) =>
          val firstRow = mergeBMHorizontally(recursiveAction(one), recursiveAction(two))
          val secondRow = mergeBMHorizontally(recursiveAction(three), recursiveAction(four))
          mergeBMVertically(firstRow, secondRow)
        case QEmpty => Nil
      }
    }

    recursiveAction(qt)
  }

  def monoColoredBitMap(width: Int, height: Int, color: Color): Bitmap = {
    @tailrec
    def createBitMap(cursor: Int, acc: Bitmap): Bitmap = {
      if (cursor > height) acc
      else createBitMap(cursor + 1, createRow(1, Nil) :: acc)
    }

    @tailrec
    def createRow(cursor: Int, acc: List[Color]): List[Color] = {
      if (cursor > width) acc
      else createRow(cursor + 1, color :: acc)
    }

    createBitMap(1, Nil)
  }


  def dropRowEnd(bitmap: Bitmap, end: String): Bitmap = end match {
    case "first" => bitmap.dropRight(1)
    case "last" => bitmap.drop(1)
  }

  def dropColumnEnd(bitmap: Bitmap, end: String): Bitmap = {

    @tailrec
    def dropFirst(bitmap: Bitmap, acc: Bitmap): Bitmap = bitmap match {
      case Nil => acc
      case x :: xs => dropFirst(xs, acc :+ x.drop(1))
    }

    @tailrec
    def dropLast(bitmap: Bitmap, acc: Bitmap): Bitmap = bitmap match {
      case Nil => acc
      case x :: xs => dropLast(xs, acc :+ x.dropRight(1))
    }

    end match {
      case "first" => dropFirst(bitmap, Nil)
      case "last" => dropLast(bitmap, Nil)
    }
  }

  def mergeBMVertically(base: Bitmap, merge: Bitmap): Bitmap = {
    if (merge == Nil && base == Nil) Nil
    else if (base == Nil) merge
    else if (merge == Nil) base
    else base ::: merge
  }

  def mergeBMHorizontally(base: Bitmap, merge: Bitmap): Bitmap = {
    @tailrec
    def recursiveAction(base: Bitmap, merge: Bitmap, acc: Bitmap): Bitmap = base match {
      case Nil => acc
      case x :: xs => merge match {
        case Nil => acc
        case y :: ys => recursiveAction(xs, ys, acc :+ (x ::: y))
      }
    }

    if (merge == Nil && base == Nil) Nil
    else if (base == Nil) merge
    else if (merge == Nil) base
    else recursiveAction(base, merge, Nil)
  }

  def getCoords(qt: QTree[Coords]): Option[Coords] = qt match {
    case QLeaf((coords: Coords, _: Color)) => Some(coords)
    case QNode(coords: Coords, _: QTree[Coords], _: QTree[Coords], _: QTree[Coords], _: QTree[Coords]) => Some(coords)
    case QEmpty => None
  }

  def getCoordsMount(qt: QTree[Coords]): Option[Coords] = qt match {
    case QLeaf((coords: Coords, _: Color)) => Some(coords)
    case QNode(_: Coords, one: QTree[Coords], two: QTree[Coords], three: QTree[Coords], four: QTree[Coords]) =>
      GeomUtils.union(GeomUtils.union(getCoords(one), getCoords(two)), GeomUtils.union(getCoords(three), getCoords(four)))
    case QEmpty => None
  }

  def moveTo(qt: QTree[Coords], moving: (Point, (Option[Coords], Point) => Option[Coords]),
             mountNode: ((QTree[Coords], QTree[Coords], QTree[Coords], QTree[Coords]), (Point, (Option[Coords], Point) => Option[Coords])) =>
               ((QTree[Coords], QTree[Coords], QTree[Coords], QTree[Coords]), Coords)): QTree[Coords] = {
    qt match {
      case QLeaf((coords: Coords, color: Color)) => QLeaf(moving._2(Some(coords), moving._1) match {
        case Some(coords: Coords) => coords
      }, color)
      case QNode(_: Coords, one: QTree[Coords], two: QTree[Coords], three: QTree[Coords], four: QTree[Coords]) =>
        val res = mountNode((one, two, three, four), moving)
        val leafs = res._1
        QNode(res._2, leafs._1, leafs._2, leafs._3, leafs._4)
      case QEmpty => qt
    }
  }

  def mountNode(leafs: (QTree[Coords], QTree[Coords], QTree[Coords], QTree[Coords]), moving: (Point, (Option[Coords], Point) => Option[Coords]),
                orderNodes: ((QTree[Coords], QTree[Coords], QTree[Coords], QTree[Coords])) =>
                  (QTree[Coords], QTree[Coords], QTree[Coords], QTree[Coords])): ((QTree[Coords], QTree[Coords], QTree[Coords], QTree[Coords]), Coords) = {

    val ordered = orderNodes(leafs)

    @tailrec
    def MountForAnchor(qt: QTree[Coords], node: Int): (QTree[Coords], QTree[Coords], QTree[Coords], QTree[Coords]) = {

      def moveToAppend(base: QTree[Coords], appendix: QTree[Coords], moving: (Int, Point, (Option[Coords], Point) => Option[Coords])): QTree[Coords] = {
        //assert(base != QEmpty)
        moveTo(appendix, (GeomUtils.plus(GeomUtils.getCorner(moving._1, getCoordsMount(base) match {
          case Some(coords: Coords) => coords
        }), moving._2), moving._3),
          mountNode(_: (QTree[Coords], QTree[Coords], QTree[Coords], QTree[Coords]), _: (Point, (Option[Coords], Point) => Option[Coords]), orderNodes))
      }

      qt match {
        case QLeaf(_: Section) | QNode(_: Coords, _: QTree[Coords], _: QTree[Coords], _: QTree[Coords], _: QTree[Coords]) =>
          val anchor = moveTo(qt, moving,
            mountNode(_: (QTree[Coords], QTree[Coords], QTree[Coords], QTree[Coords]), _: (Point, (Option[Coords], Point) => Option[Coords]), orderNodes))
          node match {
            case 1 =>
              (anchor,
                moveToAppend(anchor, ordered._2, (2, (1, 0), GeomUtils.moveLeftUpperConnerTo)),
                moveToAppend(anchor, ordered._3, (3, (0, 1), GeomUtils.moveLeftUpperConnerTo)),
                moveToAppend(anchor, ordered._4, (4, (1, 1), GeomUtils.moveLeftUpperConnerTo))
              )
            case 2 =>
              val coordsnode3 = getCoords(ordered._3)
              (QEmpty,
                anchor,
                moveToAppend(anchor, ordered._3, (3, coordsnode3 match {
                  case None => (0, 0)
                  case Some(coords: Coords) => GeomUtils.minus((-1, 1), (GeomUtils.getDifferences(coords)._1, 0))
                }, GeomUtils.moveLeftUpperConnerTo)),
                moveToAppend(anchor, ordered._4, (3, (0, 1), GeomUtils.moveLeftUpperConnerTo))
              )
            case 3 =>
              (QEmpty,
                QEmpty,
                anchor,
                moveToAppend(anchor, ordered._4, (2, (1, 0), GeomUtils.moveLeftUpperConnerTo))
              )
            case 4 =>
              (QEmpty, QEmpty, QEmpty, anchor)
          }
        case QEmpty =>
          node match {
            case 1 => MountForAnchor(ordered._2, 2)
            case 2 => MountForAnchor(ordered._3, 3)
            case 3 => MountForAnchor(ordered._4, 4)
            case 4 => (QEmpty, QEmpty, QEmpty, QEmpty)
          }
      }
    }

    val mounted = MountForAnchor(ordered._1, 1) // coords err
    val mountedCoords = GeomUtils.union(GeomUtils.union(getCoordsMount(mounted._1), getCoordsMount(mounted._2)), GeomUtils.union(getCoordsMount(mounted._3), getCoordsMount(mounted._4)))
    //assert(mountedCoords != None)
    (mounted, mountedCoords match {
      case Some(coords: Coords) => coords
    })
  }

  def nodeOrderingOperation(qt: QTree[Coords], opt: String): QTree[Coords] = {

    def getNodeOrdering: ((QTree[Coords], QTree[Coords], QTree[Coords], QTree[Coords])) => (QTree[Coords], QTree[Coords], QTree[Coords], QTree[Coords]) = opt match {
      case "mirrorH" =>
        leafs: (QTree[Coords], QTree[Coords], QTree[Coords], QTree[Coords]) => (leafs._3, leafs._4, leafs._1, leafs._2)
      case "mirrorV" =>
        leafs: (QTree[Coords], QTree[Coords], QTree[Coords], QTree[Coords]) => (leafs._2, leafs._1, leafs._4, leafs._3)
      case "rotateL" =>
        leafs: (QTree[Coords], QTree[Coords], QTree[Coords], QTree[Coords]) => (leafs._2, leafs._4, leafs._1, leafs._3)
      case "rotateR" =>
        leafs: (QTree[Coords], QTree[Coords], QTree[Coords], QTree[Coords]) => (leafs._3, leafs._1, leafs._4, leafs._2)
    }

    moveTo(qt, ((0, 0), GeomUtils.moveLeftUpperConnerTo),
      mountNode(_: (QTree[Coords], QTree[Coords], QTree[Coords], QTree[Coords]), _: (Point, (Option[Coords], Point) => Option[Coords]), getNodeOrdering))
  }

  def mirrorH(qt: QTree[Coords]): QTree[Coords] = nodeOrderingOperation(qt, "mirrorH")

  def mirrorV(qt: QTree[Coords]): QTree[Coords] = nodeOrderingOperation(qt, "mirrorV")

  def rotateL(qt: QTree[Coords]): QTree[Coords] = nodeOrderingOperation(qt, "rotateL")

  def rotateR(qt: QTree[Coords]): QTree[Coords] = nodeOrderingOperation(qt, "rotateR")


  def noise(qt: QTree[Coords], rnd: Int => Int): QTree[Coords] = {
    colorMapEffect(color => {
      val r = rnd(256).asInstanceOf[Double] / 255

      def apply(component: Int): Int = {
        MathUtils.truncateToInterval((0, 255), (r * component).asInstanceOf[Int])
      }

      val temp = Color(apply(color.r), apply(color.g), apply(color.b))
      temp
    }, qt)
  }

  def noiseImpure(qt: QTree[Coords]): QTree[Coords] = noise(qt, scala.util.Random.nextInt)


  def contrast(qt: QTree[Coords], contrastCurve: Int => Double, factor: Double): QTree[Coords] = {
    //assert(factor >= 0 && factor <= 1)
    colorMapEffect(color => {
      val calcFactor = contrastCurve((((factor * 2) - 1) * 255).asInstanceOf[Int])

      def apply(component: Int): Int =
        MathUtils.truncateToInterval((0, 255), ((calcFactor * (component - 128)) + 128).asInstanceOf[Int])

      Color(apply(color.r), apply(color.g), apply(color.b))
    }, qt)
  }

  def defaultContrast(qt: QTree[Coords], factor: Double): QTree[Coords] =
    contrast(qt, contrast => (259 * (contrast + 255)).asInstanceOf[Double] / (255 * (259 - contrast)), factor)

  def sepia(qt: QTree[Coords]): QTree[Coords] = {
    colorMapEffect(color => {
      val newRed = 0.393 * color.r + 0.769 * color.g + 0.189 * color.b
      val newGreen = 0.349 * color.r + 0.686 * color.g + 0.168 * color.b
      val newBlue = 0.272 * color.r + 0.534 * color.g + 0.131 * color.b
      Color(MathUtils.truncateToInterval((0, 255), newRed.asInstanceOf[Int]),
        MathUtils.truncateToInterval((0, 255), newGreen.asInstanceOf[Int]),
        MathUtils.truncateToInterval((0, 255), newBlue.asInstanceOf[Int]))
    }, qt)
  }

  def colorMapEffect(f: Color => Color, qt: QTree[Coords]): QTree[Coords] = {
    val bitmap = QTreeToBitMap(qt)

    @tailrec
    def runRow(bitmap: Bitmap, acc: Bitmap): Bitmap = {
      @tailrec
      def runColumn(list: List[Color], acc: List[Color]): List[Color] = list match {
        case Nil => acc
        case x :: xs => runColumn(xs, acc :+ f(x))
      }

      bitmap match {
        case Nil => acc
        case x :: xs => runRow(xs, acc :+ runColumn(x, Nil))
      }
    }

    BitMapToQtree(runRow(bitmap, Nil))
  }

  def resizeImage(bitmap: Bitmap, dim: (Int, Int), interplForm: (Point, ((Point, Color), (Point, Color), (Point, Color), (Point, Color))) => Color): Bitmap = {
    val wRatio: Float = (bitmap.head.size - 1).asInstanceOf[Float] / dim._1
    val hRatio: Float = (bitmap.size - 1).asInstanceOf[Float] / dim._2
    //assert(wRatio > 1 && hRatio > 1)

    def fillSquare(pixels: ((Point, Color), (Point, Color), (Point, Color), (Point, Color))): Bitmap = {
      //assert(pixels._1._1._1 == pixels._3._1._1 && pixels._2._1._1 == pixels._4._1._1
      //  && pixels._1._1._2 == pixels._2._1._2 && pixels._3._1._2 == pixels._4._1._2)
      val ydif = pixels._3._1._2 - pixels._1._1._2
      val xdif = pixels._2._1._1 - pixels._1._1._1

      @tailrec
      def fillBitmap(column: Int, length: Int, acc: Bitmap): Bitmap = {
        @tailrec
        def fillRow(row: Int, length: Int, acc: List[Color]): List[Color] = {
          if (row <= length)
            fillRow(row + 1, length, interplForm((row, column), pixels) :: acc)
          else
            acc
        }

        if (column <= length)
          fillBitmap(column + 1, length, fillRow(1, ydif + 1, Nil) :: acc)
        else
          acc
      }

      fillBitmap(1, xdif + 1, Nil)
    }

    @tailrec
    def buildBitMap(column: Int, acc: Bitmap): Bitmap = {
      @tailrec
      def buildRow(square: (Point, Point, Point, Point), acc: Bitmap): Bitmap =
        if (square._2._2 <= bitmap.head.size - 1) {
          val newSquare = (square._2, GeomUtils.plus(square._2, (1, 0)), square._4, GeomUtils.plus(square._4, (1, 0)))
          val filled = fillSquare(((GeomUtils.multiply(square._1, wRatio, hRatio), getColor(bitmap, square._1)),
            (GeomUtils.multiply(square._2, wRatio, hRatio), getColor(bitmap, square._2)),
            (GeomUtils.multiply(square._3, wRatio, hRatio), getColor(bitmap, square._3)),
            (GeomUtils.multiply(square._4, wRatio, hRatio), getColor(bitmap, square._4))))

          buildRow(newSquare, mergeBMHorizontally(acc, if (square._1._1 > 0) dropColumnEnd(filled, "first") else filled))
        }
        else Nil

      if (column <= bitmap.size) {
        val newRow = buildRow(((0, 0), (1, 0), (0, 1), (1, 1)), acc)
        buildBitMap(column + 1, mergeBMVertically(acc, if (column > 1) dropRowEnd(newRow, "first") else newRow))
      } else Nil
    }

    buildBitMap(0, Nil)
  }

  def rotateL2(qt:QTree[Coords]) : QTree[Coords] = {
    qt match {
      case QNode(originalCoords: Coords, one: QTree[Coords], two: QTree[Coords], three: QTree[Coords], four: QTree[Coords]) => {
        QNode(originalCoords, rotateLAux(two,originalCoords._2 ), rotateLAux(four,originalCoords._2 ), rotateLAux(one,originalCoords._2 ), rotateLAux(three,originalCoords._2 ))
      }
      case QEmpty  => qt
      case qleaf:QLeaf[Coords,Section] => {
        QLeaf(qleaf.value._1, qleaf.value._2 )
      }

    }
  }
  def rotateLAux(qt: QTree[Coords], point: Point):QTree[Coords] ={
    qt match {
      case QNode(originalCoords: Coords, one: QTree[Coords], two: QTree[Coords], three: QTree[Coords], four: QTree[Coords]) => {
        val initialcoord = (originalCoords._1._2,point._1 - originalCoords._2._1)
        val finalcoord = (originalCoords._2._2,point._1-originalCoords._1._1)
        QNode((initialcoord,finalcoord),rotateL2(two),rotateL2(four),rotateL2(one),rotateL2(three))
      }  case qleaf:QLeaf[Coords,Section] => {
        val initialcoord:Point = ( qleaf.value._1._1._2,point._1 -  qleaf.value._1._2._1)
        val finnalcoord :Point = ( qleaf.value._1._2._2,point._1- qleaf.value._1._1._1)

        QLeaf((initialcoord,finnalcoord),qleaf.value._2)
      }case QEmpty => qt
    }
  }

}
