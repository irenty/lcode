package _0004_median_of_two_sorted_arrays

object Solution1 {
  def findMedianSortedArrays(nums1: Array[Int], nums2: Array[Int]): Double = {

    val combined = (nums1 ++ nums2).sorted

    val midIndex = combined.size / 2
    if (midIndex % 2 == 1) combined(midIndex) else (combined(midIndex - 1) +  combined(midIndex)) / 2.0
  }
}

object Solution2 {
  def findMedianSortedArrays(nums1: Array[Int], nums2: Array[Int]): Double = {
    // use binary search over 2 arrays TBC...
    1
  }
}

object Main0004 extends App {
  println("Solution1")
  println(Solution1.findMedianSortedArrays(Array(1, 3), Array(2)))
  println(Solution1.findMedianSortedArrays(Array(1, 2), Array(3, 4)))

  println("Solution2")
  println(Solution2.findMedianSortedArrays(Array(1, 3), Array(2)))
  println(Solution2.findMedianSortedArrays(Array(1, 2), Array(3, 4)))
}
