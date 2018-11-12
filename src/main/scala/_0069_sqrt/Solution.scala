package _0069_sqrt


object Solution {
  def mySqrt(x: Int): Int = {
    if (x <= 1) return x

    import scala.annotation.tailrec
    @tailrec
    def guess(left: Int, right: Int): Int = {
      val mid = left + (right - left) / 2

      val isMid = mid - x / mid
      val isMidPlus = (mid + 1) - x / (mid + 1)

      if (isMid <= 0 && isMidPlus > 0) return mid

      val (newLeft, newRight) =
        if (isMid < 0) (mid + 1, right) else (left, mid)

      guess(newLeft, newRight)
    }

    guess(1, x)
  }
}

object Main0069 extends App {

  println(Solution.mySqrt(6))
  println( Solution.mySqrt(2147395599))
}
