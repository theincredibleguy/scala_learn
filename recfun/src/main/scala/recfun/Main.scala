package recfun

import java.util

import scala.collection.mutable

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    val charArray = "((()))()".toCharArray.toList
    println(balance(charArray))
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if ((c == 0) || (c == r)) return 1 else pascal (c-1, r-1) + pascal(c, r-1)
  }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def loop(index: Int, intStack: util.Stack[Int]): Int = {
        if (index == chars.size) intStack.size
        else {
          if (chars(index) == '(') {
            intStack.push(1)
          } else {
            if (chars(index) == ')') intStack.pop()
          }
          loop(index + 1, intStack)
        }
      }
      try {
        if (loop(0, new util.Stack[Int]) == 0) {
          true
        } else false
      } catch {
        case _ => false
      }
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      var ways = 0
      def calculate(remainingAmount: Int, index: Int): Int = {
        if (remainingAmount == 0) ways = ways + 1
        var temp = index;
        while(temp < coins.size) {
          if (remainingAmount >= coins(temp)) {
            calculate(remainingAmount - coins(temp), temp)
          }
          temp = temp + 1
        }
        ways
      }
      calculate(money, 0)
    }
  }
