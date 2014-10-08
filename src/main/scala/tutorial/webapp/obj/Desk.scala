package tutorial.webapp.obj

import tutorial.webapp.obj.Movement.Movement

class Desk(points: Field) {
  require(points.forall(_.length == size), "Points are not square size")

  def size: Int = points.length

  protected def copy(patch: Field => Field) = new Desk(patch(points))

  def apply(x: Int, y: Int): Item = points(x)(y)

  def updated(x: Int, y: Int, value: Item) = copy(p => p.updated(x, p(x).updated(y, value)))

  def isEmptyPos(x: Int, y: Int): Boolean = apply(x, y).isEmpty

  def hasEmpty = points.exists(_.exists(_.isEmpty))

  def horizontalProj: Projection = new Projection {
    override def get(lineNum: Int) = points(lineNum)

    override def updated(lineNum: Int, row: Row) = copy(_.updated(lineNum, row))
  }

  def verticalProj: Projection = new Projection {
    override def get(lineNum: Int) = points.map(row => row(lineNum))

    override def updated(lineNum: Int, value: Row): Desk = copy(_.zipWithIndex.map {
      case (row, i) => row.updated(lineNum, value(i))
    })
  }

  import Movement._

  def projectionByMovement(move: Movement): Projection = move match {
    case Left | Right => horizontalProj
    case Up | Down => verticalProj
  }

}

object Desk {
  def empty(size: Int): Desk = new Desk(List.fill(size, size)(None))

  def shift(vector: Row): Row = vector match {
    case Nil => Nil
    case n :: Nil => vector
    case None :: tail =>
      tail.find(_.nonEmpty).fold(vector) { case nonEmptyEl =>
        nonEmptyEl :: shift(tail.updated(tail.indexOf(nonEmptyEl), None))
      }
    case Some(a) :: Some(b) :: tail if a == b =>
      Some(a * b) :: shift(tail) ++ (None :: Nil)
    case head :: tail =>
      head :: shift(tail)
  }

  def mkMove(initial: Desk, move: Movement): Desk = {
    def moveRow(desk: Desk, rowNum: Int): Desk = {
      val projection = desk.projectionByMovement(move)
      val row =
        if (move.reversedDirection) shift(projection(rowNum).reverse).reverse
        else shift(projection(rowNum))

      projection.updated(rowNum, row)
    }

    (initial /: (0 until initial.size))(moveRow)
  }

}

trait Projection {
  def get(lineNum: Int): Row

  def apply(lineNum: Int) = get(lineNum)

  def updated(lineNum: Int, value: Row): Desk
}

object Generators {

  def startNumber = Iterator.continually {
    Some(if (math.random < 0.9) 2 else 4)
  }

  def position(size: Int) = {
    def rand = math.floor(math.random * size).toInt

    Iterator.continually((rand, rand))
  }

  def defaultValues(initial: Desk) =
    Iterator.iterate(initial) { case desk =>
      if (desk.hasEmpty) {
        val (x, y) = position(desk.size).filter((desk.isEmptyPos _).tupled).next()
        desk.updated(x, y, startNumber.next())
      } else desk
    }

}

object Movement {

  sealed trait Movement {
    val isHorizontal = false

    val isVertical = false

    val reversedDirection = false
  }

  case object Up extends Movement {
    override val isVertical = true
  }

  case object Down extends Movement {
    override val isVertical = true

    override val reversedDirection = true
  }

  case object Left extends Movement {
    override val isHorizontal = true

    override val reversedDirection = true
  }

  case object Right extends Movement {
    override val isHorizontal = true
  }

}