sealed trait List[+A] // + meands allow sub class
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] = 
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*)) // * means variable length argument
  
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("setHead on empty list")
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = 
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n-1)
    }
  
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h, t) if (f(h)) => dropWhile(t)(f)
    case _ => l
  }

  def append[A](hl: List[A], tl: List[A]): List[A] = hl match {
    case Nil => tl
    case Cons(h, t) => Cons(h, append(t, tl))
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }
}   
  

object Main extends App {
  println(List(1, 2, 3))
  println(List.sum(List(1, 2, 3)))
  println(List.tail(List(1, 2, 3)))
  println(List.setHead(List(1, 3, 4), 2))
  println(List.drop(List(1, 2, 3), 1))
  val xs = List(1, 2, 3)
  println(List.dropWhile(xs)(i => i < 3))
  println(List.append(List(1, 2, 3), List(4, 5, 6)))
}
