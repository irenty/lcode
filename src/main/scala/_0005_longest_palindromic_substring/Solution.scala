package _0005_longest_palindromic_substring

object Solution {
  def longestPalindrome(s: String): String = {

    def findPalindrom(indexStart: Int, indexEnd: Int, letters: Int): String =
      if (indexStart - letters < 0 || indexEnd + letters >= s.size) s.slice(indexStart - letters + 1, indexEnd + letters)
      else if (s(indexStart - letters) != s(indexEnd + letters)) s.slice(indexStart - letters + 1, indexEnd + letters)
      else findPalindrom(indexStart, indexEnd, letters + 1)

    if (s.isEmpty) return ""
    if (s.size == 1) return s

    def findIndexEnd(indexStart: Int, indexEnd: Int): Int =
      if (indexEnd > s.length - 1) indexEnd - 1
      else if (s(indexStart) != s(indexEnd)) indexEnd - 1
      else findIndexEnd(indexStart, indexEnd + 1)

    def findLongest(indexStart: Int, longestFound: String): String = {
      if (indexStart >= s.size - 1) return longestFound
      val indexEnd = findIndexEnd(indexStart, indexStart)
      val palindrom = findPalindrom(indexStart, indexEnd, 1)
      val newLongest = if (palindrom.size > longestFound.size) palindrom else longestFound
      findLongest(indexEnd + 1, newLongest)
    }

    findLongest(0, s.take(1))
  }
}


object Main0005 extends App {
  println(Solution.longestPalindrome("bb"))
  println(Solution.longestPalindrome("babad"))
  println(Solution.longestPalindrome("cbbd"))

  println(Solution.longestPalindrome("babadeksalcjbaaaabjchghdcbbd"))
}

