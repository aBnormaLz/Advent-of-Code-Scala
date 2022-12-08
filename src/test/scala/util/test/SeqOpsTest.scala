package util.test

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import util.Ops.{SeqOps, SplitSettings}
import util.enumeration.EmptyElementSettings._
import util.enumeration.MatchSettings._

class SeqOpsTest extends AnyWordSpecLike with Matchers {
  s"SeqOps with $dropMatch and $keepEmptyElement" should {
    val setting = SplitSettings(dropMatch, keepEmptyElement)

    "split in the middle correctly" in {
      val split = Seq("a", "s", "d", "a", "s", "d", "f").split("s", setting)
      split shouldBe Seq(Seq("a"), Seq("d", "a"), Seq("d", "f"))
    }

    "split empty Seq() correctly" in {
      val split = Seq().split("", setting)
      split shouldBe Seq()
    }

    "split at the beginning correctly" in {
      val split = Seq("a", "s", "d").split("a", setting)
      split shouldBe Seq(Seq(), Seq("s", "d"))
    }

    "split at the end correctly" in {
      val split = Seq("a", "s", "d").split("d", setting)
      split shouldBe Seq(Seq("a", "s"), Seq())
    }
  }

  s"SeqOps with $dropMatch and $dropEmptyElement" should {
    val setting = SplitSettings(dropMatch, dropEmptyElement)

    "split in the middle correctly" in {
      val split = Seq("a", "s", "d", "a", "s", "d", "f").split("s", setting)
      split shouldBe Seq(Seq("a"), Seq("d", "a"), Seq("d", "f"))
    }

    "split empty Seq() correctly" in {
      val split = Seq().split("", setting)
      split shouldBe Seq()
    }

    "split at the beginning correctly" in {
      val split = Seq("a", "s", "d").split("a", setting)
      split shouldBe Seq(Seq("s", "d"))
    }

    "split at the end correctly" in {
      val split = Seq("a", "s", "d").split("d", setting)
      split shouldBe Seq(Seq("a", "s"))
    }
  }

  s"SeqOps with $keepMatchRight and $keepEmptyElement" should {
    val setting = SplitSettings(keepMatchRight, keepEmptyElement)

    "split in the middle correctly" in {
      val split = Seq("a", "s", "d", "a", "s", "d", "f").split("s", setting)
      split shouldBe Seq(Seq("a"), Seq("s", "d", "a"), Seq("s", "d", "f"))
    }

    "split empty Seq() correctly" in {
      val split = Seq().split("", setting)
      split shouldBe Seq()
    }

    "split at the beginning correctly" in {
      val split = Seq("a", "s", "d").split("a", setting)
      split shouldBe Seq(Seq(), Seq("a", "s", "d"))
    }

    "split at the end correctly" in {
      val split = Seq("a", "s", "d").split("d", setting)
      split shouldBe Seq(Seq("a", "s"), Seq("d"))
    }
  }

  s"SeqOps with $keepMatchRight and $dropEmptyElement" should {
    val setting = SplitSettings(keepMatchRight, dropEmptyElement)

    "split in the middle correctly" in {
      val split = Seq("a", "s", "d", "a", "s", "d", "f").split("s", setting)
      split shouldBe Seq(Seq("a"), Seq("s", "d", "a"), Seq("s", "d", "f"))
    }

    "split empty Seq() correctly" in {
      val split = Seq().split("", setting)
      split shouldBe Seq()
    }

    "split at the beginning correctly" in {
      val split = Seq("a", "s", "d").split("a", setting)
      split shouldBe Seq(Seq("a", "s", "d"))
    }

    "split at the end correctly" in {
      val split = Seq("a", "s", "d").split("d", setting)
      split shouldBe Seq(Seq("a", "s"), Seq("d"))
    }
  }

  s"SeqOps with $keepMatchLeft and $keepEmptyElement" should {
    val setting = SplitSettings(keepMatchLeft, keepEmptyElement)

    "split in the middle correctly" in {
      val split = Seq("a", "s", "d", "a", "s", "d", "f").split("s", setting)
      split shouldBe Seq(Seq("a", "s"), Seq("d", "a", "s"), Seq("d", "f"))
    }

    "split empty Seq() correctly" in {
      val split = Seq().split("", setting)
      split shouldBe Seq()
    }

    "split at the beginning correctly" in {
      val split = Seq("a", "s", "d").split("a", setting)
      split shouldBe Seq(Seq("a"), Seq("s", "d"))
    }

    "split at the end correctly" in {
      val split = Seq("a", "s", "d").split("d", setting)
      split shouldBe Seq(Seq("a", "s", "d"), Seq())
    }
  }

  s"SeqOps with $keepMatchLeft and $dropEmptyElement" should {
    val setting = SplitSettings(keepMatchLeft, dropEmptyElement)

    "split in the middle correctly" in {
      val split = Seq("a", "s", "d", "a", "s", "d", "f").split("s", setting)
      split shouldBe Seq(Seq("a", "s"), Seq("d", "a", "s"), Seq("d", "f"))
    }

    "split empty Seq() correctly" in {
      val split = Seq().split("", setting)
      split shouldBe Seq()
    }

    "split at the beginning correctly" in {
      val split = Seq("a", "s", "d").split("a", setting)
      split shouldBe Seq(Seq("a"), Seq("s", "d"))
    }

    "split at the end correctly" in {
      val split = Seq("a", "s", "d").split("d", setting)
      split shouldBe Seq(Seq("a", "s", "d"))
    }
  }
}
