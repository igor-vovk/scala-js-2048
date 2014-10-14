package tutorial.webapp.obj

import org.scalacheck.Gen
import tutorial.webapp.UnitSpec

class DeskSpec extends UnitSpec with Gens {

  describe("Desk") {
    describe("r/w operations by implicitly passing coordinates") {
      they("should return written values") {
        forAll { (desk: Desk, value: Int) =>
          val coordGen = Gen.choose(0, desk.size - 1)

          forAll(coordGen, coordGen) { (x: Int, y: Int) =>
            assert(desk.updated(x, y, Some(value)).get(x, y) == Some(value))
          }
        }
      }
    }

    describe("row shifting") {
      it("should return rows with the same size as passed") {
        forAll { (row: Row) =>
          assert(Desk.shift(row).size == row.size)
        }
      }
      it("should work") {
        forAll(Table(
          ("before", "after"),
          (List(None, None), List(None, None)),
          (List(None, Some(2)), List(Some(2), None)),
          (List(Some(2), Some(2)), List(Some(4), None)),
          (List(Some(2), None, Some(2)), List(Some(4), None, None)),
          (List(Some(2), Some(2), Some(2), Some(2)), List(Some(4), Some(4), None, None)),
          (List(None, Some(2), Some(2)), List(Some(4), None, None))
        )) { (before: Row, after: Row) =>
          assert(Desk.shift(before) == after)
        }
      }
    }

  }

}

