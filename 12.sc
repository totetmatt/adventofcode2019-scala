import scala.io.Source




case class Vec3(x:Int,y:Int,z:Int) {
  def +(other:Vec3) = Vec3(x+other.x,y+other.y,z+other.z)
  def energy = math.abs(x) + math.abs(y) + math.abs(z)
}
case class Planet(pos:Vec3,velocity:Vec3) {
  def gravity(other:Planet) = {
    val nx = if(other.pos.x < pos.x) -1 else if(other.pos.x > pos.x) +1 else 0
    val ny = if(other.pos.y < pos.y) -1 else if(other.pos.y > pos.y) +1 else 0
    val nz = if(other.pos.z < pos.z) -1 else if(other.pos.z > pos.z) +1 else 0
    Vec3(nx,ny,nz)
  }
  def applyVelocity(velocity:Vec3) = {
    this.copy(pos+velocity+this.velocity,velocity+this.velocity)
  }
  def energy = pos.energy * velocity.energy
}

var sourceOpCode = Source
  .fromFile("/home/totetmatt/Projects/adventofcode2019-scala/input12")
  .getLines()
  .map{l=>
    val Seq(x,y,z) = l.replace("<","")
      .replace(">","")
      .split(",")
      .map(_.split("=").last.toInt).toSeq
    Planet(Vec3(x,y,z),Vec3(0,0,0))
  }.toSeq
sourceOpCode.foreach(println)
var q =sourceOpCode

(1 to 1000).map { c =>

  q = q.map(p =>
    (p.applyVelocity(q.map(p.gravity).reduce(_ + _)))
  )
}
q.map(_.energy).sum


