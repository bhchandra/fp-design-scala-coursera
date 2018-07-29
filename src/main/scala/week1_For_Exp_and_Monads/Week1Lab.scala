package week1_For_Exp_and_Monads

import java.util.concurrent.ThreadLocalRandom

object Week1Lab {


  trait Generator[+T] {

    self => //an alias for this
    //    def self: Generator[T] = this

    def generate: T

    def map[S](f: T => S): Generator[S] = new Generator[S] {
      def generate: S = f(self.generate)
    }

    def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
      def generate: S = f(self.generate).generate
    }

  }


  val integers: Generator[Int] = new Generator[Int] {
    val rand = ThreadLocalRandom.current()

    override def generate: Int = rand.nextInt()
  }

  val booleans: Generator[Boolean] = integers map (x => x > 0)


  //map & flatmap functions can be replaced with For-Exp.
  //hence the boolean generator can be rewritten as follows.
  val booleans2: Generator[Boolean] = for (x <- integers) yield x > 0


  def pairs[T, U](t: Generator[T], u: Generator[U]): Generator[(T, U)] =
    t flatMap { x => u map { y => (x, y) } }

  //rewriting it with For-Exp
  def pairs2[T, U](t: Generator[T], u: Generator[U]): Generator[(T, U)] =
    for {
      x <- t
      y <- u
    } yield (x, y)

  def single[T](x: T): Generator[T] = new Generator[T] {
    def generate: T = x
  }

  def choose(lo: Int, hi: Int): Generator[Int] =
    for (x <- integers) yield lo + x % (hi - lo)


  def oneOf[T](xs: T*): Generator[T] = {
    for (n <- choose(0, xs.length)) yield xs(n)
  }


  //**********************************************************//
  //*********************A LIST GENERATOR********************//

  def lists: Generator[List[Int]] = for {
    isEmpty <- booleans
    list <- if (isEmpty) emptyList else nonEmptyList
  } yield list

  def emptyList: Generator[List[Int]] = single(Nil)

  def nonEmptyList: Generator[List[Int]] =
    for {
      head <- integers
      tail <- lists
    } yield head :: tail
}
