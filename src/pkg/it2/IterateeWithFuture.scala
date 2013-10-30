package pkg.it2

import scala.concurrent._
import scala.concurrent.Future._
import ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object IterateeWithFuture {
  trait Input[+E]

  case class El[E](e: E) extends Input[E]
  case object EOF extends Input[Nothing]
  //case object Empty extends Input[Nothing] // Ignore for now

  trait Iteratee[E, A] {
    def run: Future[A] = {
      this match {
        case Done(a, _) => successful { a }
        case Cont(k) => k(EOF) map {
          case Done(a, _) => a
          case _ => sys.error("diverging iteratee after Input.EOF")
        }

      }
    }

    def map[B](f: A => Future[B]): Future[Iteratee[E, B]] = flatMap(a => f(a).map { b => Done(b, EOF) })

    def flatMap[B](f: A => Future[Iteratee[E, B]]): Future[Iteratee[E, B]] = {
      this match {
        case Done(x, e) => f(x) flatMap {
          case Done(y, _) => successful { Done(y, e) }
          case Cont(k) => k(e)
        }
        case Cont(k) => successful { Cont(e => k(e).flatMap(_.flatMap(f))) }
      }
    }
  }

  case class Done[E, A](a: A, rem: Input[E] = EOF) extends Iteratee[E, A]
  case class Cont[E, A](k: Input[E] => Future[Iteratee[E, A]]) extends Iteratee[E, A]
  //case class Error[E, A](errMsg: String, rem: Input[E]) extends Iteratee[E, A]

  trait Enumerator[E] {
    def apply[A](it: Iteratee[E, A]): Future[Iteratee[E, A]]
    def run[A](it: Iteratee[E, A]): Future[A] = apply(it).flatMap(_.run)
  }

  case class ListEnumerator[E](list: List[E]) extends Enumerator[E] {
    def apply[A](it: Iteratee[E, A]): Future[Iteratee[E, A]] = {
      def step(stepList: List[E], stepItF: Future[Iteratee[E, A]]): Future[Iteratee[E, A]] = {
        stepItF.flatMap { stepIt =>
          (stepList, stepIt) match {
            case (head :: tail, Cont(k)) => step(tail, k(El(head)))
            case _ => stepItF
          }
        }
      }

      step(list, successful(it))
    }
  }

  case class StreamEnumerator[E](stream: Stream[Future[E]]) extends Enumerator[E] {
    def apply[A](it: Iteratee[E, A]): Future[Iteratee[E, A]] = {
      def step(stepStream: Stream[Future[E]], stepItF: Future[Iteratee[E, A]]): Future[Iteratee[E, A]] = {
        stepItF.flatMap { stepIt =>
          (stepStream, stepIt) match {
            case (headF #:: tail, Cont(k)) => headF.flatMap { head => step(tail, k(El(head))) }
            case _ => stepItF
          }
        }
      }

      step(stream, successful(it))
    }
  }

  class Println[E] extends Cont[E, Unit]({
    case El(e) =>
      println(e); successful { new Println }
    case EOF => println("EOF"); successful { Done((), EOF) }
  })

  def getStringIt: Iteratee[Byte, String] = Done("Foo Bar")
  def getIntIt: Iteratee[Byte, Int] = Done(22)

  case class User(name: String, age: Int)
  def getUserIt: Future[Iteratee[Byte, User]] =
    getStringIt.flatMap(name => getIntIt.map(age => successful(User(name, age) ) ) )

  def main(args: Array[String]) {
    val se = StreamEnumerator(List(1, 2, 3).map(i => future { Thread.sleep(i * 1000); i }).toStream)
    val uF = se.run(new Println)

    Await.ready(uF, Duration.Inf)

    val be = ListEnumerator(List[Byte](61, 62, 63))
    val userF = getUserIt.flatMap(be.run(_) )
    val user = Await.result(userF, Duration.Inf)
    println(user)

  }

  def main0(args: Array[String]) {
    val le = ListEnumerator(List(1, 2, 3))
    val it = le.run(new Println)

    Await.ready(it, Duration.Inf)
  }
}