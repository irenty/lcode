package _0002_add_two_numbers


class ListNode(var _x: Int = 0) {
  var next: ListNode = null
  var x: Int = _x

  override def toString: String = s"$x -> ${next.x} -> ${next.next.x}"
}

object Solution {

  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {

    import scala.annotation.tailrec
    @tailrec
    def addPositionsWithCarry(numbers: Seq[ListNode], outcome: ListNode): Unit = {
      val sum = numbers.map(_.x).sum
      outcome.x = sum % 10
      val nextNumbers = numbers.map { n => Option[ListNode](n.next) }.flatten
      val nextNumbersWithCurry = if (sum / 10 > 0) nextNumbers :+ new ListNode(sum / 10) else nextNumbers
      if (nextNumbersWithCurry.isEmpty) return
      outcome.next = new ListNode(0)
      addPositionsWithCarry(nextNumbersWithCurry, outcome.next)
    }

    val result = new ListNode(0)
    addPositionsWithCarry(Seq(l1, l2), result)
    result
  }
}

object Main0002 extends App {

  def numToNode2(v: Int): ListNode = {
    def linkDec(n: ListNode, dec: Int): ListNode = {
      if (dec == 0) return n
      n.next = new ListNode(dec % 10)
      linkDec(n.next, dec / 10)
    }

    val firstNode = new ListNode(v % 10)
    linkDec(firstNode, v / 10)
    firstNode
  }


  val x = Solution.addTwoNumbers(numToNode2(342), numToNode2(465))
  println(x)
}

