package util.test

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import util.Ops.IntCoordsOps

class IntCoordsTest extends AnyWordSpecLike with Matchers {
  s"IntCoords" should {

    // (0, 0) (1, 0) (2, 0)
    // (0, 1)        (2, 1)
    // (0, 2) (1, 2) (2, 2)
    "calculate neighbours correctly with 1 element" in {
      Seq((1, 1)).neighbours() shouldBe Set(
        //@formatter:off
        (0, 0), (1, 0), (2, 0),
        (0, 1), (2, 1),
        (0, 2), (1, 2), (2, 2),
        //@formatter:on
      )
    }

    "calculate neighbours correctly with 2 element" in {
      Seq((1, 1), (2, 1)).neighbours() shouldBe Set(
        //@formatter:off
        (0, 0), (1, 0), (2, 0), (3, 0),
        (0, 1),                 (3, 1),
        (0, 2), (1, 2), (2, 2), (3, 2),
        //@formatter:on
      )
    }

    "calculate neighbours correctly with 3 element curving" in {
      Seq((1, 1), (2, 1), (2, 2)).neighbours() shouldBe Set(
        //@formatter:off
        (0, 0), (1, 0), (2, 0), (3, 0),
        (0, 1),                 (3, 1),
        (0, 2), (1, 2),         (3, 2),
                (1, 3), (2, 3), (3, 3),
        //@formatter:on
      )
    }
  }
}
