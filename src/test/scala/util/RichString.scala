package util

import scala.collection.immutable

object RichString {
  implicit class PadLeftTo(str: String) {
    def padLeftTo[B >: Char](len: Int, elem: B): immutable.IndexedSeq[B] = {
      str.reverse.padTo(len, elem).reverse
    }
  }
}
