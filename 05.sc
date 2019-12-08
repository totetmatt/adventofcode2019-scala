import scala.io.Source

def intcode(opcode:String,noun:Int=12,verb:Int=2) : Array[Int] = {
  val opcodeA : Array[Int] = opcode.split(",").map(_.toInt)
  opcodeA.update(1,noun)
  opcodeA.update(2,verb)
  for(i <- 0.until(opcodeA.length,4)) {
    opcodeA(i) match {
      case 1 => opcodeA.update(opcodeA(i+3),opcodeA(opcodeA(i+1)) + opcodeA(opcodeA(i+2)))
      case 2 => opcodeA.update(opcodeA(i+3),opcodeA(opcodeA(i+1)) * opcodeA(opcodeA(i+2)))
      case 99 => return opcodeA
    }
  }
  opcodeA
}


var sourceOpCode:String = Source
  .fromFile("/home/totetmatt/Projects/adventofcode2019-scala/input02")
  .getLines().take(1).mkString

val q = for{
  noun <- 0 until 100
  verb <- 0 until 100
} yield (noun,verb,intcode(sourceOpCode,noun,verb)(0)) if 1 == 1

q.filter(_._3 == 19690720).foreach{ q =>
  println(q)
  println(100*q._1 + q._2)
}