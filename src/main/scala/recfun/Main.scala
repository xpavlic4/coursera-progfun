package recfun

import scala.annotation.tailrec

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
    if (c < 0) 0
    else if (c > r) 0
    else if (r == 0) 1
    else {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def b(chars: List[Char], acc: Int): Boolean = {
      if (acc < 0)false
        else if (chars.isEmpty) acc == 0
      else if (chars.head == '(')
        b(chars.tail, acc + 1)
      else if (chars.head == ')')
        b(chars.tail, acc - 1)
      else b(chars.tail, acc)
    }
    b(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def c(money: Int, coins: List[Int]): Int = {
      if (money < 0) 0
      else if (0 == money) 1
      else {
        var k = 0
        for (a <- coins.indices) {
          k += c(money - coins(a), coins.filter(_ >= coins(a)))
        }
        k
      }
    }

    c(money, coins)
  }
}
