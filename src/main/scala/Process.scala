import java.io.PrintStream
import java.net.Socket
import scala.io.BufferedSource
import Store

class Process(socket:Socket) extends Thread {

  override def run(): Unit = {

    val input: Iterator[String] = 
      new BufferedSource 
  }
}

