package recfun

object RecFun extends RecFunInterface {
  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row) {
        print(s"${pascal(col, row)} ")
      }
      println()
    }
  }

  /** Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    val isCorner = (c == 0 || c == r)
    if (isCorner)
      then { 1 }
    else { pascal(c - 1, r - 1) + pascal(c, r - 1) }
  }

  /** Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def reqBalance(chars:List[Char], count: Int): Boolean = {
      if (count < 0) then { false }
      else if (chars.isEmpty)
        then { count == 0 }
      else {
        chars.head match {
          case '(' => reqBalance(chars.tail, count + 1)
          case ')' => reqBalance(chars.tail, count - 1)
          case _   => reqBalance(chars.tail, count)
        }
      }
    }

    reqBalance(chars, 0)
  }

  /** Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int =
    {

      0
    }
}
