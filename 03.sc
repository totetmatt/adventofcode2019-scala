import scala.io.Source

case class Coor(x:Int, y:Int)  extends Ordered[Coor]{

  def +(other:Coor) = Coor(x+other.x, y+other.y)
  override def compare(that: Coor) = if(x == that.x) y.compare(that.y) else x.compare(that.x)
}
case class Line(t:String,o:Int,l:Int,h:Int,time:Int)


def parse(s:String) = s.split(",").map{ d=>
  d.head match {
    case 'R' => Coor(d.tail.toInt,0)
    case 'L' => Coor(-d.tail.toInt,0)
    case 'D' => Coor(0,-d.tail.toInt)
    case 'U' => Coor(0,d.tail.toInt)
  }
}.foldLeft(Seq[Coor](Coor(0,0)) ){ (agg,el) =>
  agg :+ (el + agg.last)
}.sliding(2).map(_.sorted).map{
  q=>
    if(q(0).x == q(1).x) Line("X",q(0).x,q(0).y,q(1).y,q(0).x)
    else Line("Y",q(0).y,q(0).x,q(1).x,q(0).y)
}
  .foldLeft(Seq.empty[Line]){
    (agg,el) => if(agg.isEmpty) agg :+ el else agg :+ el.copy(time=el.time+agg.last.time)
  }
  .toSeq

/*val path = Source.fromFile("C:\\Users\\totetmatt\\IdeaProjects\\adventofcode2019\\input03")
  .getLines
  .map(k=> parse(k))
  .toSeq
*/

val path = Seq(parse("R8,U5,L5,D3"),parse("U7,R6,D4,L4"))
val q = path(0)
path(1).flatMap( x=> {
  q.filter(y => y.t != x.t &&
    y.l <= x.o && y.h >= x.o &&
    x.l <= y.o && x.h >= y.o)
    //.map(y=> math.abs(y.o) + math.abs(x.o))
})





