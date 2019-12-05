
Range(152085,670283)
 .count{
   x=>
     val sl=x.toString.toSeq.sliding(2).toSeq
     sl.count(x=>x(0) > x(1)) == 0 &&
       sl.count(x=> x(0) == x(1)) >=1
 }




val sl=111111.toString.toSeq.sliding(2)
sl.count(x=>x(0) > x(1)) == 0
sl.count(x=> x(0) == x(1))


"112233".toSeq.groupMap(identity)(identity).view.mapValues(_.size).toMap
"122133".toSeq.groupMap(identity)(identity).view.mapValues(_.size).toMap
"121233".toSeq.groupMap(identity)(identity).view.mapValues(_.size).toMap
