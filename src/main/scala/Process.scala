import java.io.PrintStream
import java.net.Socket
import scala.io.BufferedSource
import Store

class Process(socket:Socket) extends Thread {

  val CRLF = "\r\n"
  override def run(): Unit = {

    val input: Iterator[String] = 
      new BufferedSource(socket.getInputStream()).getLines()
    val output = new PrintStream(socket.getOutputStream)
    while (true) {
      if(input.hasNext) {
        val commands = getInput(input, Nil)
        if(commands != Nil){
          val res = encode(exec(commands))
          output.print(res + CRLF)
          output.flush()
        }
      }
    }
  }

  def exec(commands: List[Any]): Any = 
    commands.head.asInstanceOf[String].toUpperCase match {
      case "PING" => if (commands.length == !) "PONG" else commands(1)
      case "SET"  => onSet(commands.drop(1))

    }
}

