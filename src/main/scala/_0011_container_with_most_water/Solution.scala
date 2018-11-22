package _0011_container_with_most_water



object Solution {

  class SolutionHelper(height: Array[Int]) {

    import scala.annotation.tailrec

    def area(start: Int, end: Int) = (end - start) * Math.min(height(start), height(end))

    @tailrec
    final def maxArea(start: Int, end: Int, currentMax: Int): Int =
      if (height(start) == 0 || start == end) currentMax
      else {
        val a = area(start, end)
        val newMax = Math.max(a, currentMax)
        if (height(start) <= height(end)) newMax
        else maxArea(start, end -1, newMax)
      }

    @tailrec
    final def maxArea(start: Int, currentMax: Int): Int =
      if (start >= height.size - 1) currentMax
      else {
        val a = maxArea(start, height.size - 1, currentMax)
        val newMax = Math.max(currentMax, a)
        maxArea(start + 1, newMax)
      }
  }

  object SolutionHelper {
    def apply(h: Array[Int]) = new SolutionHelper(h)
  }

  def testSolutionHelper() = {
    // SolutionHelper(Array(1, 8, 1, 6)).area(1, 3)
    // SolutionHelper(Array(12, 8, 9, 7)).maxArea(0, 3, 0)
    SolutionHelper(Array(1,8,6,2,5,4,8,3,7)).maxArea(0, 0)
  }

  def maxArea(height: Array[Int]): Int = {
    // testSolutionHelper()
    SolutionHelper(height).maxArea(0, 0)
  }
}

object Main0011 extends App {
  println(Solution.maxArea(Array(1,8,6,2,5,4,8,3,7)))
}
