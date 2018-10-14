object MyModule {
  def abs(n: Int): Int = {
    if (n < 0) -n else n
  }

  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int = {
      if(n <= 0) acc else go(n - 1, n * acc)
    }
    go(n, 1)
  }

  private def formatAbs(x: Int) = {
    val msg = "the absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  private def formatFactorial(n: Int) = {
    val msg = "the factorial of %d is %d"
    msg.format(n, factorial(n))
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "the %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatResult("absolute value", -42, abs))
    println(formatFactorial(7))
    println(formatResult("factorial", 7, factorial))
  }
}
