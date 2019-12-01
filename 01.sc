
import scala.io.Source

def f(mass:Int) = { Math.max(0,(mass/3)-2) }

def fullfuel(mass:Int):Int  = {
  if(mass == 0) 0
  else   f(mass) + fullfuel(f(mass))
}

// Part 2
Source.fromFile("C:\\Users\\totetmatt\\IdeaProjects\\adventofcode2019\\input01")
  .getLines
  .map(_.toInt)
  .map(fullfuel _)
  .sum == 4717699

// Part 1
Source.fromFile("C:\\Users\\totetmatt\\IdeaProjects\\adventofcode2019\\input01")
  .getLines
  .map(_.toInt)
  .map(f _)
  .sum

