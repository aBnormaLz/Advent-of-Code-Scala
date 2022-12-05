package util

object Assertion {
  implicit class CheckSize[I <: IterableOnce[_]](i: I) {
    def checkSizeEquals(size: Int): I = {
      assert(i.iterator.size == size, s"expected - $size, actual - ${i.iterator.size}")
      i
    }

    def checkSizeMax(size: Int): I = {
      assert(i.iterator.size <= size, s"expected - $size, actual - ${i.iterator.size}")
      i
    }
  }
}
