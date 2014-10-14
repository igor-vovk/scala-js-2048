package tutorial.webapp.obj

import org.scalacheck.{Arbitrary, Gen}
import tutorial.webapp.obj.Movement._

trait Gens {

  val desk: Gen[Desk] = for {
    size <- Gen.choose(1, 50)
    nonEmptyValuesCount <- Gen.choose(0, 100)
  } yield Generators.defaultValues(Desk.empty(size)).drop(nonEmptyValuesCount).next()
  implicit val deskA: Arbitrary[Desk] = Arbitrary(desk)

  val movement: Gen[Movement] = Gen.oneOf(Left, Right, Up, Down)
  implicit val movementA: Arbitrary[Movement] = Arbitrary(movement)

}
