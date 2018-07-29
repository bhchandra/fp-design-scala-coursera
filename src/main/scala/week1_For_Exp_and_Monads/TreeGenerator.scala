package week1_For_Exp_and_Monads

import java.util.concurrent.ThreadLocalRandom

import week1_For_Exp_and_Monads.Week1Lab.Generator

object TreeGenerator {

  trait Tree

  case class Inner(left: Tree, right: Tree) extends Tree

  case class Leaf(x: Int) extends Tree

  val integers: Generator[Int] = new Generator[Int] {
    val rand = ThreadLocalRandom.current()

    override def generate: Int = rand.nextInt()
  }

  def trees: Generator[Tree] = {

    def leafs: Generator[Tree] = for (x <- integers) yield Leaf(x)

    def inners: Generator[Tree] =
      for {
        left <- leafs
        right <- leafs
      } yield Inner(left, right)

    for {
      isLeaf <- integers.map(x => x > 0)
      tree <- if (isLeaf) leafs else inners
    } yield tree
  }

}
