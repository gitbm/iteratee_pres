package pkg.it5

import scalaz._
import iteratee._
import Iteratee._
import Input.{Element, Eof, Empty}
import scala.concurrent._
import scala.concurrent.Future._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration



object ScalazIterateeF {

  type Iteratee[E, A] = IterateeT[E, Future, A]
  type Step[E, A] = StepT[E, Future, A]

   implicit val futureMonad = new Monad[Future] {
    override def point[A](a: => A): Future[A] =
      future { a }

    override def bind[A, B](fa: Future[A])(f: A => Future[B]): Future[B] =
      fa.flatMap(f)
  }

  
  def getStringIt: Iteratee[Byte, String] = {
    def step(curr: String): Iteratee[Byte, String] =
      cont {
        case Element(e) if (e != 0) => step(curr + e.toChar)
        case Empty() => step(curr)
        case Eof() => done(curr, eofInput)
        case _ => done(curr, emptyInput)
      }
    step("")
  }

  def getIntIt: Iteratee[Byte, Int] = {
    def step(curr: Int, byteCount: Int): Iteratee[Byte, Int] =
      cont {
        case Eof() => done(curr, eofInput)
        case _ if byteCount == 4 => done(curr, emptyInput)
        case Element(e) if byteCount < 4 => iterateeT( future { Thread.sleep(2000); println("*"); }.flatMap {_ => step(curr * 256 + e, byteCount + 1).value } );
        case Empty() => step(curr, byteCount)
      }
    step(0, 0)
  }

  case class User(name: String, age: Int)
  def getUserIt: Iteratee[Byte, User] =
    for (name <- getStringIt; age <- getIntIt) yield User(name, age)

  def main(args: Array[String]): Unit = {
    val be = enumList[Byte, Future](List[Byte](65, 66, 0, 0, 0, 1, 7))
    val userF = (getUserIt &= be).run
    val user = Await.result(userF, Duration.Inf)
    println(user)
  }
}
