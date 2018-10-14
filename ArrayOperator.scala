object ArraySearcher {

  def findFirst[T](ss: Array[T], p: T => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = 
      if (n >= ss.length) -1
      else if (p(ss(n))) n
      else loop(n + 1)
    loop(0)
  }

  def isSorted[A](as: Array[A], orderd: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(n: Int): Boolean = 
      if (n >= as.length-1) true
      else if (orderd(as(n), as(n + 1))) false
      else go(n + 1)
    go(0)
  }
  
  def main(args: Array[String]):Unit = {
    println(findFirst[String](Array("hoge", "fuga", "piyo"), (s: String) => s == "piyo"))
    println(findFirst[Int](Array(1, 2, 3), (i: Int) => i == 3))
    println(findFirst[Array[Int]](Array(Array(1, 2), Array(3, 4), Array(5, 6)), (a: Array[Int]) => a.deep == Array(1, 2).deep))

    println(isSorted[Int](Array(1, 2, 3), (x: Int, y: Int) => x > y))
    println(isSorted[Int](Array(1), (x: Int, y: Int) => x > y))
    println(isSorted[Int](Array(1, 3, 2), (x: Int, y: Int) => x > y))
  }
}
