import scala.io.Source

var sourceOpCode:String = Source
  .fromFile("C:\\Users\\totetmatt\\IdeaProjects\\adventofcode2019\\input05")
  .getLines().toSeq.head
val test = sourceOpCode.split(",").map(_.toInt)

def inst1(intCode:Array[Int], ptr1:Int,ptr2:Int,ptr3:Int) = intCode.update(ptr3,intCode(ptr1) + intCode(ptr2))
def inst2(intCode:Array[Int], ptr1:Int,ptr2:Int,ptr3:Int) = intCode.update(ptr3,intCode(ptr1) * intCode(ptr2))

def inst3(intCode:Array[Int], ptr1:Int,value:Int) = intCode.update(ptr1,value)
def inst4(intCode:Array[Int], ptr1:Int) = println(intCode(ptr1))

def inst5(intCode:Array[Int],ptr1:Int,ptr2:Int) = if(intCode(ptr1) != 0)  Some(intCode(ptr2)) else None
def inst6(intCode:Array[Int],ptr1:Int,ptr2:Int) = if(intCode(ptr1) == 0)  Some(intCode(ptr2)) else None

def inst7(intCode:Array[Int],ptr1:Int,ptr2:Int,ptr3:Int)  = if(intCode(ptr1) < intCode(ptr2)) intCode.update(ptr3,1) else intCode.update(ptr3,0)
def inst8(intCode:Array[Int],ptr1:Int,ptr2:Int,ptr3:Int)  = if(intCode(ptr1) == intCode(ptr2)) intCode.update(ptr3,1) else intCode.update(ptr3,0)

def getParamPtr(intCode:Array[Int],ptr:Int,mode:Int) = if(mode == 0) intCode(ptr) else ptr
def parseInstruction(intCode:Array[Int],ptr:Int) =  {

  val (opMode,instruction) = "%05d".format(intCode(ptr)).splitAt(3)

  val Array(mode1,mode2,mode3) = opMode.reverse.map(_.toString.toInt).toArray
  instruction.toString.toInt match {
    case 99 => (instruction.toString.toInt,0,0,0)
    case 1 =>  (instruction.toString.toInt,getParamPtr(intCode,ptr+1,mode1),getParamPtr(intCode,ptr+2,mode2),getParamPtr(intCode,ptr+3,mode3))
    case 2 =>  (instruction.toString.toInt,getParamPtr(intCode,ptr+1,mode1),getParamPtr(intCode,ptr+2,mode2),getParamPtr(intCode,ptr+3,mode3))
    case 3 =>  (instruction.toString.toInt,getParamPtr(intCode,ptr+1,mode1),0,0)
    case 4 =>  (instruction.toString.toInt,getParamPtr(intCode,ptr+1,mode1),0,0)
    case 5 =>  (instruction.toString.toInt,getParamPtr(intCode,ptr+1,mode1),getParamPtr(intCode,ptr+2,mode2),0)
    case 6 =>  (instruction.toString.toInt,getParamPtr(intCode,ptr+1,mode1),getParamPtr(intCode,ptr+2,mode2),0)
    case 7 =>  (instruction.toString.toInt,getParamPtr(intCode,ptr+1,mode1),getParamPtr(intCode,ptr+2,mode2),getParamPtr(intCode,ptr+3,mode3))
    case 8 =>  (instruction.toString.toInt,getParamPtr(intCode,ptr+1,mode1),getParamPtr(intCode,ptr+2,mode2),getParamPtr(intCode,ptr+3,mode3))
  }

}


def intCodeEval(intCode:Array[Int], input:Array[Int], ptr:Int=0) : Array[Int] = {

  val (instruction,ptr1,ptr2,ptr3) = parseInstruction(intCode,ptr)

  instruction match {
    case 1 => inst1(intCode,ptr1,ptr2,ptr3)
      intCodeEval(intCode,input,ptr+4)
    case 2 => inst2(intCode,ptr1,ptr2,ptr3)
      intCodeEval(intCode,input,ptr+4)
    case 3 => inst3(intCode,ptr1,input(0))
      intCodeEval(intCode,input,ptr+2)
    case 4 => inst4(intCode,ptr1)
      intCodeEval(intCode,input,ptr+2)
    case 5 => intCodeEval(intCode,input,inst5(intCode,ptr1,ptr2).getOrElse(ptr+3))

    case 6 => intCodeEval(intCode,input,inst6(intCode,ptr1,ptr2).getOrElse(ptr+3))

    case 7 => inst7(intCode,ptr1,ptr2,ptr3)
      intCodeEval(intCode,input,ptr+4)
    case 8 => inst8(intCode,ptr1,ptr2,ptr3)
      intCodeEval(intCode,input,ptr+4)
    case 99 => intCode
  }

  intCode

}

intCodeEval(test,Array(5))
