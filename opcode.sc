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
  .fromFile("C:\\Users\\totetmatt\\IdeaProjects\\adventofcode2019\\input02")
  .getLines().toSeq.head
val test = sourceOpCode.split(",").map(_.toInt)

def instruction1(opCode: Array[Int], pointer:Int) = opCode.update(opCode(pointer+3),opCode(opCode(pointer+1)) + opCode(opCode(pointer+2)))
def instruction2(opCode: Array[Int], pointer:Int) = opCode.update(opCode(pointer+3),opCode(opCode(pointer+1)) * opCode(opCode(pointer+2)))

def instruction3(opCode: Array[Int], pointer:Int, input:Int) = opCode.update(opCode(pointer+1),input)
def instruction4(opCode: Array[Int], pointer:Int, input:Int) = println(opCode(opCode(pointer+1)))

def evalInstruction(opCode: Array[Int], pointer:Int) : Array[Int] = {
    opCode(pointer) match {
      case 1 =>
        instruction1(opCode, pointer)
        evalInstruction(opCode, pointer + 4)
      case 2 =>
        instruction2(opCode, pointer)
        evalInstruction(opCode, pointer + 4)
      case 3 =>
      case 4 =>
      case 99 => opCode
    }
}
def initOpCode(opCode:Array[Int],noun:Int=12,verb:Int=2 ) = {
  opCode.update(1,noun)
  opCode.update(2,verb)
  evalInstruction(opCode,0)
}

initOpCode(test)