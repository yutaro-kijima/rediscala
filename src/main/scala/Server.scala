import java.net._
import scala.io._
import java.io._
val CRLF = "\r\n"
object Server {

  def main(args: Array[String]): Unit = {
    //サーバーソケットを生成しポート番号をバインドする
    val serverSocket = new ServerSocket(4545)
    println("Serve on host:localhost port:4545 ...")
    val socket: Socket = serverSocket.accept()
    while (true) {
      //val text = lines.mkString("\n")
      //val stream: java.io.InputStream = new java.io.ByteArrayInputStream(text.getBytes(java.nio.charset.StandardCharsets.UTF_8.name))
      val input = new  BufferedSource(socket.getInputStream).getLines().mkString("\n")
      val output = new PrintStream(socket.getOutputStream)
      println(input)
      output.print("+PONG"+CRLF)
      output.flush()
    }
  }

/*
  def exec(str:String): String = {
  }

  //CRLFでsplitされたPESP配列からコマンドだけの配列を返す [*3,$5,index,$4,desc,$2,ss]
  def loop(lst: List[String], commands: List[Any], index: Int): List[Any] = lst(index).asInstanceOf[List[Char]] match {
    case List('+', _*)            => List(lst(0))
    case Nil                      => commands
    case List('*', _*)            => loop(lst, commands, index + 1)
    case List('$', '-', '1', _*)  => loop(lst, commands :+ null, index + 1)
    case List('$', _*)            => loop(lst, commands :+ lst(index + 1), index + 2)
    case _                        => loop(lst, commands :+ lst(index).toInt, index + 1)
  }

//  def exec(commandsList: List[String]): String = {
//
/ }
*/
}
