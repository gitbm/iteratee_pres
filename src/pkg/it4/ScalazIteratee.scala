package pkg.it4

import scalaz.iteratee._
import Iteratee._
import Input.{Element, Eof, Empty}

object ScalazIteratee {

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
        case Element(e) if byteCount < 4 => step(curr * 256 + e, byteCount + 1) //Iteratee.flatten(future { Thread.sleep(2000); println("*"); step(curr * 256 + e, byteCount + 1) })
        case Empty() => step(curr, byteCount)
      }
    step(0, 0)
  }

  case class User(name: String, age: Int)
  def getUserIt: Iteratee[Byte, User] =
    for (name <- getStringIt; age <- getIntIt) yield User(name, age)

  def main(args: Array[String]): Unit = {
    val be = enumList(List[Byte](65, 66, 0, 0, 0, 1, 7))
    val user = (getUserIt &= be).run
    println(user)
  }

}