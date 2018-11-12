package _0004_median_of_two_sorted_arrays

// Combining and sorting
object Solution1 {
  def findMedianSortedArrays(nums1: Array[Int], nums2: Array[Int]): Double = {

    val combined = (nums1 ++ nums2).sorted

    val midIndex = combined.size / 2
    if (combined.size % 2 == 0) (combined(midIndex - 1) +  combined(midIndex)).toDouble / 2 else combined(midIndex).toDouble
  }
}

//
object Solution2 {
  def findMedianSortedArrays(nums1: Array[Int], nums2: Array[Int]): Double = {
    // use binary search over 2 arrays TBC...
    val combined = Array.ofDim[Int](nums1.size + nums2.size)

    var i = 0
    var j = 0
    var k = 0

    while (i < nums1.size && j < nums2.size) {
      if (nums1(i) < nums2(j)) {
        combined(k) = nums1(i)
        i = i + 1
      } else {
        combined(k) = nums2(j)
        j = j + 1
      }
      k = k + 1
    }

    if (i < nums1.size) {
      nums1.slice(i, nums1.size).foreach { n =>
        combined(k) = n
        k = k + 1
      }
    }

    if (j < nums2.size) {
      nums2.slice(j, nums2.size).foreach { n =>
        combined(k) = n
        k = k + 1
      }
    }

    val midIndex = combined.size / 2
    if (combined.size % 2 == 0) (combined(midIndex - 1) +  combined(midIndex)).toDouble / 2 else combined(midIndex).toDouble
  }
}

object Main0004 extends App {
  println("Solution1")
  println(Solution1.findMedianSortedArrays(Array(1, 3), Array(2)))
  println(Solution1.findMedianSortedArrays(Array(1, 2), Array(3, 4)))
  println(Solution1.findMedianSortedArrays(Array(), Array(2, 3)))

  println("Solution2")
  println(Solution2.findMedianSortedArrays(Array(1, 3), Array(2)))
  println(Solution2.findMedianSortedArrays(Array(1, 2), Array(3, 4)))
  println(Solution2.findMedianSortedArrays(Array(), Array(1)))
  println(Solution2.findMedianSortedArrays(Array(), Array(2, 3)))
}
