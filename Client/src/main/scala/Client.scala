object Client {
  val CRLF = "\r\n"

  def main(args: Array[String]): Unit = {
    val socket = new Socket(InetAddress.getByName("localhost"), 4545)
    lazy val input = new BufferedSource(socket.getInputStream()).getLines()
    val out = new PrintStream(socket.getOutputStream())

    def getInput(input: String): String = {
      input.toCharArray.toList match {
        case List('+', _*)           => input.diff("+")
        case List(':', _*)           => input.diff(":")
        case List('$', '-', '1', _*) => null
        case _                       => "?"
      }
    }

    val numberStream = Stream.from(0).iterator
    while (numberStream.next() <= 11) {
      val count = numberStream.next().toString
      out.print(
        "*3" + CRLF +
          "$3" + CRLF +
          "SET" + CRLF +
          ":" + count + CRLF +
          ":" + count + CRLF
      )
      println(s"client> SET ${count} ${count}")
      println("server>" + input.next())
      out.print(
        "*3" + CRLF +
          "$3" + CRLF +
          "incrby" + CRLF +
          ":" + count.toString + CRLF +
          ":" + count.toString + CRLF
      )
      println(s"client> INCRBY ${count} ${count}")
      println("server>set " + input.next())
      out.print(
        "*2" + CRLF +
          "$3" + CRLF +
          "get" + CRLF +
          ":" + count.toString + CRLF
      )
      println(s"client> GET ${count}")
      println(input.next())
      out.print(
        "*2" + CRLF +
          "$3" + CRLF +
          "exists" + CRLF +
          ":" + count.toString + CRLF
      )
      println(s"client> EXISTS ${count}")
      println(input.next())
    }
    socket.close()
  }
}
