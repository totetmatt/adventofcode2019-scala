
Range(152085,670283)
 .count{
   x=>
     val sl=x.toString.toSeq.sliding(2).toSeq
     sl.count(x=>x(0) > x(1)) == 0 &&
     sl.count(x=> x(0) == x(1)) >=1 &&
       x.toString.toSeq.groupMap(identity)(identity).view.mapValues(_.size).toMap.values.toSeq.contains(2)
 }



