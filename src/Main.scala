import Backend.Backend

import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    if (args.length !=1){
      printf("invalid arguments.")
    }else{
      val buffer = Source.fromFile(args(0))
      val code = buffer.mkString
      Backend.execute(code)
      buffer.close()
    }
  }
}
