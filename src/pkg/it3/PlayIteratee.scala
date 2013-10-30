package pkg.it3

import scala.concurrent._
import scala.concurrent.Future._
import ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import play.api.libs.iteratee.Iteratee
import play.api.libs.iteratee.Done
import play.api.libs.iteratee.Enumerator
import play.api.libs.iteratee.Cont
import play.api.libs.iteratee.Input.El
import play.api.libs.iteratee.Input

object PlayIteratee {

  //  def getStringIt: Iteratee[Byte, String] = Done("Foo Bar")
  //  def getIntIt: Iteratee[Byte, Int] = Done(22)
  def getStringIt: Iteratee[Byte, String] = {
    def step(curr: String): Iteratee[Byte, String] = 
    Cont {
      case El(e) if (e != 0) => step(curr + e.toChar )
      case Input.Empty => step(curr)
      case Input.EOF => Done(curr, Input.EOF)
      case _ => Done(curr, Input.Empty)
    }
    step("")
  }

  def getIntIt: Iteratee[Byte, Int] = {
    def step(curr: Int, byteCount: Int): Iteratee[Byte, Int] = 
    Cont {
      case Input.EOF => Done(curr, Input.EOF)
      case  _ if byteCount == 4 => Done(curr, Input.Empty)
      case El(e) if byteCount < 4 => Iteratee.flatten(future { Thread.sleep(2000); println("*"); step(curr * 256 + e, byteCount + 1) })
      case Input.Empty => step(curr, byteCount)
    }
    step(0, 0)
  }

  case class User(name: String, age: Int)
  def getUserIt: Iteratee[Byte, User] =
    for (name <- getStringIt; age <- getIntIt) yield User(name, age)

  def main(args: Array[String]): Unit = {
    val be = Enumerator.enumerate(List[Byte](65, 66, 0, 0, 0, 1, 7))
    val userF = be.run(getUserIt)
    val user = Await.result(userF, Duration.Inf)
    println(user)
  }

}