import java.io.PrintStream
import java.net.{ServerSocket, Socket}

import scala.io.BufferedSource

object Server {
  val CRLF = "\r\n"

  def main(args: Array[String]): Unit = {
    //サーバーソケットを生成しポート番号をバインドする
    val serverSocket = new ServerSocket(4545)


    while (true) {
      val socket: Socket = serverSocket.accept()
      println("Serve on host:localhost port:4545 ...")
      val input = new BufferedSource(socket.getInputStream()).getLines()
      val output = new PrintStream(socket.getOutputStream)
      while(true) {
        if (input.hasNext) {
          val commands = getInput(input, Nil) match{
            case Nil => List("?")
            case i => i
          }
          println(commands)
          output.print(encode(exec(commands)) + CRLF)
          output.flush()
        }
      }
    }
  }


  def exec(commands: List[Any]): Any = commands.head.asInstanceOf[String].toUpperCase match {
    case "PING" => if (commands.length == 1) "PONG" else commands(1)
    case "SET" => onSet(commands.drop(1))
    case "INCRBY" => store.incrBy(commands(1), commands(2).toString.toLong)
    case "DECRBY" => store.decrBy(commands(1), commands(2).toString.toLong)
    case "GET" => store.get(commands(1))
    case "DEL" => store.delete(commands.drop(1))
    case "EXISTS" => store.exists(commands.drop(1))
    case "COMMAND" => "OK"
    case _ => "?"
  }

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

  def encode(res: Any): String = res match {
    case List("-", i: String) => "-" + i
    case i: String => "+" + i
    case i: Long => ":" + i.toString
    case i: Any => "+" + i.toString
  }

  def onSet(commands: List[Any]): Any = commands match {
    case List(key, value) => store.set(key, value)
    case List(key, value, option: String) => setWithOption(key, value, option)
    case _ => List("-", "invalid argument" + commands)
  }

  def setWithOption(key: Any, value: Any, option: String): Any = option.toUpperCase match {
    case "NX" => store.setNX(key, value)
    case "XX" => store.setXX(key, value)
    case _ => List("-", "option not Supported" + option)
  }

  object store {
    var data: Map[Any, Any] = Map()

    def set(key: Any, value: Any): String = {
      println("SET")
      data = data + (key -> value)
      println(data.getOrElse(key, None))
      "OK"
    }

    def setNX(key: Any, value: Any): Any = {
      if (data.contains(key)) {
        0
      } else {
        set(key, value)
      }
    }

    def setXX(key: Any, value: Any): Any = {
      if (data.contains(key)) {
        set(key, value)
      } else {
        0
      }
    }

    def get(key: Any): Any = {
      data.getOrElse(key, "-") match {
        case "-" => List("-", "Data not found")
        case i: Any => i
      }
    }

    def incrBy(key: Any, value: Long):Any = {
      val preVal = get(key)
      preVal match {
        case i: List[String] => i
        case i: Long => {data = data + (key -> (i + value)); i + value }
        case i: String => {data = data + (key -> (i.toLong + value)); i.toLong + value }
        case _ => List("-", "type mismatch")
      }
    }

    def decrBy(key:Any, value:Long):Any = {
      val preVal = get(key)
      preVal match {
        case i: List[String] => i
        case i: Long => {data = data + (key -> (i - value)); i - value }
        case i: String => {data = data + (key -> (i.toLong - value)); i.toLong - value }
        case _ => List("-", "type mismatch")
      }
    }

    def delete(keys: List[Any]): Int = {
      val preSize = data.size
      data = data -- keys
      preSize - data.size
    }

    def exists(keys: List[Any]): Int = {
      data.size - (data -- keys).size
    }
  }
}
