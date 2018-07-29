package week2_Lazy_Evaluation

object primes extends App {

  def from(n: Int): Stream[Int] = n #:: from(n + 1)

  val naturalNumbers: Stream[Int] = from(0)

  val natList: List[Int] = naturalNumbers take 10 toList

  natList foreach (x => print(x + ",")) //0,1,2,3,4,5,6,7,8,9,
  println()

  /**
    * Sieve of Eratosthenes
    */
  def sieve(s: Stream[Int]): Stream[Int] =
    s.head #:: sieve(s.tail filter (_ % s.head != 0))

  val primes = sieve(from(2))


  val first100Primes = primes.take(100).toList

  first100Primes foreach (x => print(x + ",")) //2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,.....
}
