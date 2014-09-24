package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    countChange(4, List(1,2))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    assert(c >= 0 && r >= 0 && c <= r, s"c = $c, r = $r")

    if (c == 0) 1 // the left boundary
    else if (c == r) 1 // the right boundary (this also takes care of the tip)
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def innerBalance(numLeftMinusRight: Int, chars: List[Char]): Boolean = {
      if (chars.isEmpty) numLeftMinusRight == 0
      else if (numLeftMinusRight < 0) false
      else if (chars.head == '(') innerBalance(numLeftMinusRight + 1, chars.tail)
      else if (chars.head == ')') innerBalance(numLeftMinusRight - 1, chars.tail)
      else innerBalance(numLeftMinusRight, chars.tail)
    }

    innerBalance(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0) 0
    else if (money == 0) 1
    else if (coins.isEmpty) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
