package _0456_132_pattern

// O(n2)
object Solution1 {
  def find132pattern(nums: Array[Int]): Boolean = {

    import scala.annotation.tailrec
    @tailrec
    def findJ(minAi: Int, j: Int): Boolean = {
      if (j >= nums.size - 1) return false
      val aj = nums(j)
      val newAi = if (aj < minAi) aj else minAi
      val nextIndex = j + 1
      val found = nums.takeRight(nums.size - j - 1)
        .filter(ak => ak < aj && ak > minAi).nonEmpty

      if (found) return true
      findJ(newAi, nextIndex)
    }

    nums.size >= 3 && findJ(nums.head, 1)
  }
}

// perhaps better performing ...
object Solution2 {
  def find132pattern(nums: Array[Int]): Boolean = {
    if (nums.size < 3) return false

    val lastJ = nums.size - 2

    // fill min(i) for a given j
    val mins = Array.ofDim[Int](nums.size - 1)
    mins(0) = nums.head

    (1 to lastJ).foreach { j =>
      mins(j) = if (nums(j) < mins(j-1)) nums(j) else mins(j - 1)
    }

    var k: List[Int] = if (nums.last > mins.last) List(nums.last) else List[Int]()

    val jReverse = lastJ.to(1, -1)

    jReverse.foreach { j =>
      if (nums(j) > mins(j) + 1) {
        k = k.filter(_ > mins(j))
        if (k.find(_ < nums(j)).isDefined) return true
        k = k :+ nums(j)
      }
    }

    return false
  }
}

object Main0132 extends App {

  println("Solution1")

  println(Solution1.find132pattern(Array(3, 1, 4, 2)))
  println(Solution1.find132pattern(Array(1, 2, 3, 4)))
  println(Solution1.find132pattern(Array(-1,3,2,0)))
  println(Solution1.find132pattern(Array()))

  println("Solution2")

  println(Solution2.find132pattern(Array(3, 1, 4, 2)))
  println(Solution2.find132pattern(Array(1, 2, 3, 4)))
  println(Solution2.find132pattern(Array(-1,3,2,0)))
  println(Solution2.find132pattern(Array()))
}


