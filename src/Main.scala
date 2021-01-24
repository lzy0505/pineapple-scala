import Lexer.{Lexer, Token}
import Backend.Backend

object Main {

  var lexer = new Lexer()

  def main(args: Array[String]): Unit = {
    Backend.execute("$a = \"pen pineapple apple pen.\"\nprint($a)")
  }
}
