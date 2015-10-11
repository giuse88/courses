package week_1.assignment

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
   if ( c == r || c == 0) 1
   else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */

  def balance(chars: List[Char]): Boolean = {

    def balanceIter(acc: Int, chars: List[Char]): Boolean = {
      if (chars.isEmpty) return acc == 0
      else if (acc < 0 ) return false
      else if (chars.head == '(') return balanceIter(acc+1, chars.tail)
      else if (chars.head == ')') return balanceIter(acc-1, chars.tail)
      return balanceIter(acc, chars.tail)
    }

    balanceIter(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0 || coins.isEmpty) 0
    else if (money == 0) 1
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }

}
