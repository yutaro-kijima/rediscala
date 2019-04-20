import java.io.PrintStream
import java.net.{ServerSocket, Socket}

import scala.io.BufferedSource

object Server {
  val CRLF = "\r\n"
  lazy val store = new ThreadLocal[Map[Any, Any]] {
    override def initialValue(): Map[Any, Any] = Map()
  }

  def main(args: Array[String]): Unit = {
    //サーバーソケットを生成しポート番号をバインドする
    val serverSocket = new ServerSocket(4545)

    while (true) {
      val socket: Socket = serverSocket.accept()
      println("Serve on host: localhost port:4545 ....")
      new RedisThread(socket).start()
    }
  }

  class RedisThread(socket: Socket) extends Thread {
    val input: Iterator[String] = new BufferedSource(socket.getInputStream()).getLines()
    val output = new PrintStream(socket.getOutputStream)
    while (input.hasNext) {
      val commands = getInput(input, Nil)
      if (commands != Nil) {
        println("commands:" + commands)
        output.print(encode(exec(commands)) + CRLF)
        output.flush()
        println(store.get())
      }
    }

    def exec(commands: List[Any]): Any = commands.head.asInstanceOf[String].toUpperCase match {
      case "PING" => if (commands.length == 1) "PONG" else commands(1)
      case "SET" => onSet(commands.drop(1))
      case "INCRBY" => db.incrBy(commands(1), commands(2).toString.toLong)
      case "DECRBY" => db.decrBy(commands(1), commands(2).toString.toLong)
      case "GET" => db.get(commands(1))
      case "DEL" => db.delete(commands.drop(1))
      case "EXISTS" => db.exists(commands.drop(1))
      case "COMMAND" => "OK"
      case _ => "invalid argument"
    }

    def getInput(input: Iterator[String], commands: List[Any]): List[Any] = {
      val head = input.next()
      println(head)
      head.toCharArray.toList match {
        case List('+', _*) => List(head)
        case List(':', _*) => getInput(input, commands :+ toLongIfAble(head.diff(":")))
        case Nil => commands
        case List('*', i, _*) => getInputWithLength(input, Nil, i - '0')
        case List('$', '-', '1', _*) => getInput(input, commands :+ null)
        case List('$', _*) => getInput(input, commands :+ toLongIfAble(input.next()))
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
          case List('$', _*) => getInputWithLength(input, commands :+ toLongIfAble(input.next()), length)
          case List(':', _*) => getInputWithLength(input, commands :+ toLongIfAble(head.diff(":")), length)
          case _ => getInputWithLength(input, commands :+ toLongIfAble(head), length)
        }
      }
    }

    def encode(res: Any): String = res match {
      case List("-", i: String) => "-" + i
      case i: String => "+" + i
      case i: Long => ":" + i.toString
      case i: Any => "+" + i.toString
    }

    def tryToLong(str: String): Option[Long] = {
      import scala.util.control.Exception._
      catching(classOf[NumberFormatException]) opt str.toLong
    }

    def toLongIfAble(value: Any): Any = value match {
      case i: Long => i
      case s: String => tryToLong(s).getOrElse(value)
      case _ => value
    }

    def onSet(commands: List[Any]): Any = commands match {
      case List(key, value) => db.set(key, value)
      case List(key, value, option: String) => setWithOption(key, value, option)
      case _ => List("-", "invalid argument" + commands)
    }

    def setWithOption(key: Any, value: Any, option: String): Any = option.toUpperCase match {
      case "NX" => db.setNX(key, value)
      case "XX" => db.setXX(key, value)
      case _ => List("-", "option not Supported" + option)
    }

    object db {

      def set(key: Any, value: Any): String = {
        println("SET")
        val data = store.get()
        store.set(data + (key -> value))
        println(get(key))
        "OK"
      }

      def setNX(key: Any, value: Any): Any = {
        val data = store.get()
        if (data.contains(key)) {
          0
        } else {
          set(key, value)
        }
      }

      def setXX(key: Any, value: Any): Any = {
        val data = store.get()
        if (data.contains(key)) {
          set(key, value)
        } else {
          0
        }
      }

      def get(key: Any): Any = {
        println("get")
        val data = store.get()
        data.getOrElse(key, "-") match {
          case "-" => List("-", "Data not found")
          case i: Any => i
        }
      }

      def incrBy(key: Any, by: Long): Any = {
        println("incrby")
        val data = store.get()
        val num = data.getOrElse(key, 0).asInstanceOf[Long]
        store.set(data + (key -> (by + num)))
        get(key)
      }

      def decrBy(key: Any, by: Long): Any = {
        println("decrby")
        val data = store.get()
        val num = data.getOrElse(key, 0).asInstanceOf[Long]
        store.set(data + (key -> (by + num)))
        get(key)
      }

      def delete(keys: List[Any]): Int = {
        val data = store.get()
        val preSize = data.size
        store.set(data -- keys)
        preSize - data.size
      }

      def exists(keys: List[Any]): Int = {
        println("exists")
        val data = store.get()
        data.size - (data -- keys).size
      }
    }

  }

}
