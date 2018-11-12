package _0001_two_sum

object Solution {

  def twoSum(nums: Array[Int], target: Int): Array[Int] = {

    import scala.annotation.tailrec
    @tailrec
    def findSumPair(tailNums: Array[Int], numToIndex: Map[Int, Int]): Option[Array[Int]] = {
      if (tailNums.isEmpty) None
      val thisIndex = nums.size - tailNums.size
      val thisNumber = tailNums.head
      val otherNumber = target - thisNumber
      if (!numToIndex.contains(otherNumber)) {
        findSumPair(tailNums.tail, numToIndex + (thisNumber -> thisIndex))
      } else {
        Some(Array(numToIndex(otherNumber), thisIndex))
      }
    }

    findSumPair(nums, Map[Int, Int]()).getOrElse(Array.emptyIntArray)
  }
}

object Main0001 extends App {
  val nums = Array(230,863,916,585,981,404,316,785,88,12,70,435,384,778,887,755,740,337,86,92,325,422,815,650,920,125,277,336,221,847,168,23,677,61,400,136,874,363,394,199,863,997,794,587,124,321,212,957,764,173,314,422,927,783,930,282,306,506,44,926,691,568,68,730,933,737,531,180,414,751,28,546,60,371,493,370,527,387,43,541,13,457,328,227,652,365,430,803,59,858,538,427,583,368,375,173,809,896,370,789)

  val target = 542
  Solution.twoSum(nums, target).foreach(println(_))
}

