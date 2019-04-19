import java.io.PrintStream
import java.net.{InetAddress, Socket}

import scala.io.BufferedSource

object Client {
  val CRLF = "\r\n"

  def main(args: Array[String]): Unit = {
    val socket = new Socket(InetAddress.getByName("localhost"), 4545)
    lazy val input = new BufferedSource(socket.getInputStream()).getLines()
    val out = new PrintStream(socket.getOutputStream())

    def getInput(input: Iterator[String], commands: List[Any]): List[Any] = {
      val head = input.next()
      println(head)
      head.toCharArray.toList match {
        case List('+', _*) => List(head)
        case List(':', _*) => getInput(input, commands :+ head.diff(":").toInt)
        case Nil => commands
        case List('*', i, _*) => getInputWithLength(input, Nil, i - '0')
        case List('$', '-', '1', _*) => getInput(input, commands :+ null)
        case List('$', _*) => getInput(input, commands :+ input.next())
        case _ => getInput(input, commands :+ head.toInt)
      }
    }

    def getInputWithLength(input: Iterator[String], commands: List[Any], length: Int): List[Any] = {
      if (commands.length == length) {
        commands
      } else {
        val head = input.next()
        println(head)
        head.toCharArray.toList match {
          case Nil => commands
          case List('$', '-', '1', _*) => getInputWithLength(input, commands :+ null, length)
          case List('$', _*) => getInputWithLength(input, commands :+ input.next(), length)
          case List(':', _*) => getInputWithLength(input, commands :+ head.diff(":").toInt, length)
          case _ => getInputWithLength(input, commands :+ head.toInt, length)
        }
      }
    }

    val numberStream = Stream.from(0).iterator
    while (true) {
      val count = numberStream.next()
      out.println(
        "*3" + CRLF +
          "$3" + CRLF +
          "SET" + CRLF +
          ":" + count.toString + CRLF +
          ":" + count.toString + CRLF)
      out.flush()
      out.println(
        "*3" + CRLF +
          "$3" + CRLF +
          "incrby" + CRLF +
          ":" + count.toString + CRLF +
          ":" + count.toString + CRLF)
      out.flush()
      out.println(
        "*2" + CRLF +
          "$3" + CRLF +
          "get" + CRLF +
          ":" + count.toString + CRLF)
      out.println(
        "*2" + CRLF +
          "$3" + CRLF +
          "exists" + CRLF +
          ":" + count.toString + CRLF)
      out.flush()
      while(input.hasNext){
        println(getInput(input,Nil))
      }
    }
    socket.close()
  }
}
