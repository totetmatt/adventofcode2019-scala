import scala.io.Source

var sourceOpCode:String = Source
  .fromFile("/home/totetmatt/Projects/adventofcode2019-scala/input07")
  .getLines().toSeq.head
val test = sourceOpCode.split(",").map(_.toInt)

def inst1(intCode:Array[Int], ptr1:Int,ptr2:Int,ptr3:Int) = intCode.update(ptr3,intCode(ptr1) + intCode(ptr2))
def inst2(intCode:Array[Int], ptr1:Int,ptr2:Int,ptr3:Int) = intCode.update(ptr3,intCode(ptr1) * intCode(ptr2))

def inst3(intCode:Array[Int], ptr1:Int,value:Int) = intCode.update(ptr1,value)
def inst4(intCode:Array[Int], ptr1:Int) = intCode(ptr1)

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


def intCodeEval(intCode:Array[Int], input:Array[Int], ptr:Int=0, output:Option[Int]=None) : Option[Int] = {

  val (instruction,ptr1,ptr2,ptr3) = parseInstruction(intCode,ptr)

  instruction match {
    case 1 => inst1(intCode,ptr1,ptr2,ptr3)
      intCodeEval(intCode,input,ptr+4,output)
    case 2 => inst2(intCode,ptr1,ptr2,ptr3)
      intCodeEval(intCode,input,ptr+4,output)
    case 3 => inst3(intCode,ptr1,input.head)
      intCodeEval(intCode,input.tail,ptr+2,output)
    case 4 =>
      intCodeEval(intCode,input,ptr+2,Some(inst4(intCode,ptr1)))

    case 5 => intCodeEval(intCode,input,inst5(intCode,ptr1,ptr2).getOrElse(ptr+3),output)

    case 6 => intCodeEval(intCode,input,inst6(intCode,ptr1,ptr2).getOrElse(ptr+3),output)

    case 7 => inst7(intCode,ptr1,ptr2,ptr3)
      intCodeEval(intCode,input,ptr+4,output)
    case 8 => inst8(intCode,ptr1,ptr2,ptr3)
      intCodeEval(intCode,input,ptr+4,output)
    case 99 => output
  }

}

Seq(4,3,2,1,0).foldLeft(0)((agg,el) =>  intCodeEval(test,Array(el,agg)).get )

Seq(0,1,2,3,4)
  .permutations
  .map(
    x=> x.foldLeft(0)((agg,el) =>  intCodeEval(test,Array(el,agg)).get)
  ).map(
    x=> Seq(5,6,7,8,9).permutations.map(p=> p.foldLeft(x)((agg,el) =>  intCodeEval(test,Array(el,agg)).get)).max
  ).foreach(println)

