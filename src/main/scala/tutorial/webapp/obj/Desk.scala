package tutorial.webapp.obj

import tutorial.webapp.obj.Movement.Movement


class Desk(points: List[List[Int]]) {
  require(points.forall(_.length == size), "Points are not square size")

  def size: Int = points.length

  protected def copy(patch: List[List[Int]] => List[List[Int]]) = new Desk(patch(points))

  def apply(x: Int, y: Int): Int = points(x)(y)

  def updated(x: Int, y: Int, value: Int) = copy(p => p.updated(x, p(x).updated(y, value)))

  def isEmptyPos(x: Int, y: Int): Boolean = apply(x, y) == Desk.EmptyValue

  def hasEmpty = points.exists(_.contains(Desk.EmptyValue))

  def horizontalProj: Projection = new Projection {
    override def get(lineNum: Int) = points(lineNum)

    override def updated(lineNum: Int, value: List[Int]) = copy(_.updated(lineNum, value))
  }

  def verticalProj: Projection = new Projection {
    override def get(lineNum: Int) = points.map(row => row(lineNum))

    override def updated(lineNum: Int, value: List[Int]): Desk = copy(_.zipWithIndex.map {
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
  val GameSize = 4

  val EmptyValue: Int = 0

  def empty: Desk = new Desk(List.fill(GameSize, GameSize)(EmptyValue))

  def shift(vector: List[Int]): List[Int] = vector match {
    case Nil => Nil
    case n :: Nil => vector
    case head :: tail =>
      val thead :: ttail = tail

      if (head == EmptyValue) {
        tail.find(_ != EmptyValue).fold(vector) { case nonEmptyEl =>
          nonEmptyEl :: shift(tail.updated(tail.indexOf(nonEmptyEl), EmptyValue))
        }
      } else if (head == thead) {
        (head * thead) :: shift(ttail) ++ (EmptyValue :: Nil)
      } else {
        head :: shift(tail)
      }
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
  def get(lineNum: Int): List[Int]

  def apply(lineNum: Int) = get(lineNum)

  def updated(lineNum: Int, value: List[Int]): Desk
}

object Generators {

  def startNumber = Iterator.continually {
    if (math.random < 0.9) 2 else 4
  }

  def position(size: Int) = {
    def rand = math.floor(math.random * size).toInt

    Iterator.continually((rand, rand))
  }

  def defaultValues(initial: Desk = Desk.empty) =
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