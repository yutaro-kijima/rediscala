import java.net._

import scala.io._
import java.io._
import java.util.Scanner

object Server {
  val CRLF = "\r\n"

  def main(args: Array[String]): Unit = {
    //サーバーソケットを生成しポート番号をバインドする
    val serverSocket = new ServerSocket(4545)
    val socket: Socket = serverSocket.accept()
    println("Serve on host:localhost port:4545 ...")
    while (true) {
      val input = new BufferedSource(socket.getInputStream()).getLines()
      val output = new PrintStream(socket.getOutputStream)
      println(getInput(input, Nil))
      output.print("+PONG" + CRLF)
      output.flush()
    }
    socket.close()
  }

  /*
  def exec(str:String): String = {
  }
  */

  //CRLFでsplitされたPESP配列からコマンドだけの配列を返す [*3,$5,index,$4,desc,$2,ss]
  def getInput(input: Iterator[String], commands: List[Any]): List[Any] = {
    val head = input.next()
    head.toCharArray.toList match {
      case List('+', _*) => List(head)
      case Nil => commands
      case List('*', i, _*) => getInputWithLength(input, Nil, i - '0')
      case List('$', '-', '1', _*) => getInput(input, commands :+ null)
      case List('$', _*) => getInput(input, commands :+ input.next())
      case _ => getInput(input, commands :+ head.toInt)
    }
  }

  def getInputWithLength(input: Iterator[String], commands: List[Any], length: Int): List[Any] = {
    if (commands.length == length) {
      return commands
    } else {
      val head = input.next()
      head.toCharArray.toList match {
        case Nil => commands
        case List('$', '-', '1', _*) => getInputWithLength(input, commands :+ null, length)
        case List('$', _*) => getInputWithLength(input, commands :+ input.next(), length)
        case _ => getInputWithLength(input, commands :+ head.toInt, length)
      }
    }

  }

  def exec(commandsList: List[String]): String = {

  }
}
