package util.test

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import util.Ops.IntCoordOps

class IntCoordTest extends AnyWordSpecLike with Matchers {
  s"IntCoord" should {
    "calculate up correctly" in {
      (1, 1).up() shouldBe (0, 1)
    }

    "calculate down correctly" in {
      (1, 1).down() shouldBe (2, 1)
    }

    "calculate left correctly" in {
      (1, 1).left() shouldBe (1, 0)
    }

    "calculate right correctly" in {
      (1, 1).right() shouldBe (1, 2)
    }

    "calculate up-left correctly" in {
      (1, 1).upLeft() shouldBe (0, 0)
    }

    "calculate up-right correctly" in {
      (1, 1).upRight() shouldBe (0, 2)
    }

    "calculate down-left correctly" in {
      (1, 1).downLeft() shouldBe (2, 0)
    }

    "calculate down-right correctly" in {
      (1, 1).downRight() shouldBe (2, 2)
    }
  }
}
