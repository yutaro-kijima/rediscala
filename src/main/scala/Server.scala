import java.io.PrintStream
import java.net.{ServerSocket, Socket}
import scala.collection.convert.decorateAsScala._
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.ConcurrentHashMap

import scala.io.BufferedSource

object Server {
  val CRLF = "\r\n"
  //TODO めっちゃ遅いので自分でスレッドセーフを実装する
  var data: scala.collection.concurrent.Map[String, Any] = new ConcurrentHashMap().asScala

  def main(args: Array[String]): Unit = {
    val serverSocket = new ServerSocket(4545)
    println("listen on port: 4545")

    while (true) {
      val socket: Socket = serverSocket.accept()
      println("Serve on host: localhost port:4545 ....")
      new RedisThread(socket).start()
    }
    serverSocket.close()
  }

  def tryToLong(str: String): Option[Long] = {
    import scala.util.control.Exception._
    catching(classOf[NumberFormatException]) opt str.toLong
  }

  def toLongIfAble(value: Any): Any = value match {
    case i: Int => i.toLong
    case i: Long => i
    case s: String => tryToLong(s).getOrElse(value)
    case _ => value
  }

  class RedisThread(socket: Socket) extends Thread {

    override def run(): Unit = {

      val input: Iterator[String] = new BufferedSource(socket.getInputStream()).getLines()
      val output = new PrintStream(socket.getOutputStream)
      while (true) {
        if (input.hasNext) {
          val commands = getInput(input, Nil)
          if (commands != Nil) {
            println("commands:" + commands)
            val res = encode(exec(commands))
            output.print(res + CRLF)
            output.flush()
          }
        }
      }
    }

    def exec(commands: List[Any]): Any = commands.head.asInstanceOf[String].toUpperCase match {
      case "PING" => if (commands.length == 1) "PONG" else commands(1)
      case "SET" => onSet(commands.drop(1))
      case "INCRBY" => store.incrBy(commands(1).asInstanceOf[String], commands(2).toString.toLong)
      case "DECRBY" => store.decrBy(commands(1).asInstanceOf[String], commands(2).toString.toLong)
      case "GET" => store.get(commands(1).asInstanceOf[String])
      case "DEL" => store.delete(commands.drop(1).asInstanceOf[List[String]])
      case "EXISTS" => store.exists(commands.drop(1).asInstanceOf[List[String]])
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
          case List(':', _*) => getInputWithLength(input, commands :+ toLongIfAble(head.diff(":")), length)
          case _ => getInputWithLength(input, commands :+ toLongIfAble(head), length)
        }
      }
    }

    def encode(res: Any): String = res match {
      case "$-1" => {println(data); "$-1"}
      case List("-", i: String) => "-" + i
      case i: String => "+" + i
      case i: Long => ":" + i.toString
      case i: Int => ":" + i.toString
      case i: Number => ":" + i.toString
      case i: Any => ":" + i.toString
    }

    def tryToLong(str: String): Option[Long] = {
      import scala.util.control.Exception._
      catching(classOf[NumberFormatException]) opt str.toLong
    }

    def toLongIfAble(value: Any): Any = value match {
      case i: Int => i.toLong
      case i: Long => i
      case s: String => tryToLong(s).getOrElse(value)
      case _ => value
    }

    def onSet(commands: List[Any]): Any = commands match {
      case List(key: String, value) => store.set(key, value.toString)
      case List(key: String, value, option: String) => setWithOption(key, value.toString, option)
      case _ => List("-", "invalid argument" + commands)
    }

    def setWithOption(key: String, value: String, option: String): Any = option.toUpperCase match {
      case "NX" => store.setNX(key, value)
      case "XX" => store.setXX(key, value)
      case _ => List("-", "option not Supported" + option)
    }


    object store {

      def set(key: String, value: String): String = {
        println("SET")
        data += (key -> toLongIfAble(value))
        println(get(key))
        "OK"
      }

      def setNX(key: String, value: String): Any = {
        println("SETNX")
        if (data.contains(key)) {
          0
        } else {
          set(key, value)
          1
        }
      }

      def setXX(key: String, value: String): Any = {
        println("SETXX")
        if (data.contains(key)) {
          set(key, value)
          1
        } else {
          0
        }
      }

      def get(key: String): String = {
        println("get")
        data.getOrElse(key, "$-1").toString
      }

      def getNumber(key: String): Option[Long] = {
        data.getOrElse(key, null) match {
          case null => null
          case i: String => tryToLong(i)
          case i: Long => Option(i)
          case i: Int => Option(i)
          case _ => null
        }
      }

      def incrBy(key: String, by: Long): Any = {

        def pass(i: Long): Any = {
          data.replace(key ,(i + by))
          getNumber(key).getOrElse("$-1")
        }

        getNumber(key).getOrElse(null) match {
          case null => "$-1"
          case i: Long => pass(i)
        }
      }

      def decrBy(key: String, by: Long): Any = {
        println("decrby")

        def pass(i: Long): Any = {
          data.replace(key , (i - by))
          getNumber(key).getOrElse("$-1")
        }

        getNumber(key).getOrElse(null) match {
          case null => "$-1"
          case i: Long => pass(i)
        }
      }

      def delete(keys: List[String]): Long = {
        val preSize = data.size
        data --= keys
        preSize - data.size
      }

      def exists(keys: List[String]): Long = {
        data.size - (data -- keys).size
      }
    }

  }

}
