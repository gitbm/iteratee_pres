package pkg.it1

object BasicIteratee {
  trait Input[+E]

  case class El[E](e: E) extends Input[E]
  case object EOF extends Input[Nothing]
  //case object Empty extends Input[Nothing] // Ignore for now

  trait Iteratee[E, A] {
    def run: A = {
      this match {
        case Done(a, _) => a
        case Cont(k) => k(EOF) match {
          case Done(a, _) => a
          case _ => sys.error("diverging iteratee after Input.EOF")
        }

      }
    }

    def map[B](f: A => B): Iteratee[E, B] = flatMap(a => Done(f(a), EOF))

    def flatMap[B](f: A => Iteratee[E, B]): Iteratee[E, B] = {
      this match {
        case Done(x, e) => f(x) match {
          case Done(y, _) => Done(y, e)
          case Cont(k) => k(e)
        }
        case Cont(k) => Cont(e => k(e) flatMap f)
      }
    }
  }

  case class Done[E, A](a: A, rem: Input[E] = EOF) extends Iteratee[E, A]
  case class Cont[E, A](k: Input[E] => Iteratee[E, A]) extends Iteratee[E, A]
  //case class Error[E, A](errMsg: String, rem: Input[E]) extends Iteratee[E, A]

  trait Enumerator[E] {
    def apply[A](it: Iteratee[E, A]): Iteratee[E, A]
    def run[A](it: Iteratee[E, A]): A = apply(it).run
  }

  case class ListEnumerator[E](list: List[E]) extends Enumerator[E] {
    def apply[A](it: Iteratee[E, A]): Iteratee[E, A] = {
      def step(stepList: List[E], stepIt: Iteratee[E, A]): Iteratee[E, A] = {
        (stepList, stepIt) match {
          case (head :: tail, Cont(k)) => step(tail, k(El(head)))
          case _ => stepIt
        }
      }

      step(list, it)
    }
  }

  class Println[E] extends Cont[E, Unit]({
    case El(e) => println(e); new Println
    case EOF => println("EOF"); Done((), EOF)
  })
  
  def getStringIt : Iteratee[Byte, String] = Done("Foo Bar")
  def getIntIt: Iteratee[Byte, Int] = Done(22)

  case class User(name: String, age: Int)
  def getUserIt: Iteratee[Byte, User] = 
    for (name <- getStringIt; age <- getIntIt) yield User(name, age)

  def main(args: Array[String]) {
    val le = ListEnumerator(List(1, 2, 3))
    val u = le.run(new Println)
    
    val be = ListEnumerator(List[Byte](61, 62, 63))
    val user = be.run(getUserIt)
    println(user)
    }
}