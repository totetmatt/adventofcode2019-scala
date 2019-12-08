import scala.io.Source


var l = Source
  .fromFile("C:\\Users\\totetmatt\\IdeaProjects\\adventofcode2019\\input08")
  .getLines
  .toSeq
  .head

val w = 25
val h = 6
val mmap = Map('0'->'_','1'->'X','2'->' ')

(0 until w*h).map{ x=>
  l
  .toSeq.drop(x)
  .sliding(1,w*h)
  .toSeq
  .foldLeft('2'){ (agg,el) =>
  if(agg=='2') el.head
  else agg }}
  .sliding(w,w)
  .foreach{ line =>
     line.foreach(x=>print(mmap(x)))
    println()
  }
