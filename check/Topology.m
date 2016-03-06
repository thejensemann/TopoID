Exit[];
<< TopoID`
<< ./init.m;
<< TopoID`Common`;
<< TopoID`Topology`;
<< TopoID`Mapping`
<<TopoID/init.m

(* --- setup -------------------------------------------------------- *)

set = Setup[
  Externals -> {p1, p2, p3, p4},
  Internals -> {k1},
  Masses ->
  {"gh" -> 0, "gl" -> 0, "hb" -> mh, "qd" -> 0, "qu" -> 0, "si" -> mk},
  Constants -> {s, mh^2},
  Constraints ->
  {p3 -> p1, p4 -> p2, p1^2 -> 0, p2^2 -> 0, p1*p2 -> -s/2},
  Rules -> {mh^2 -> s*x}];

Print["
(* ---  ---------------------------------------------------------- *)"];

TopologyPattern[All]
First /@ TopologyPattern[All]

TopologyPattern["Setup", None]
First /@ TopologyPattern["Setup", None]

TopologyPattern[Full]
















TopologyPattern[]

TopologyPattern[All]

TopoID`Common`$TopoIDData = True

TopologyPattern["Alpha", None]

Print["
(* ---  ---------------------------------------------------------- *)"];

top = Topology[
  name -> "T1",
  facs -> {d1 -> mh^2 + k1^2,
           d2 -> k1^2 + 2*p1*k1 + 2*p2*k1 - s,
           d3 -> k1^2 + 2*p2*k1}]

top = Topology[facs -> {d[1] -> 2*mh^2 + k1,
                        d[3] -> k1^2 + 2*p1*k1 + 2*p2*k1}];

top = Topology[facs -> {d[1] -> mh^2 + k1^2,
                        d[3] -> k1^2 + 2*p1*k1 + 2*p2*k1}];

top = Topology[facs -> {d[1] -> mh^2 + k1^2,
                        d[2] -> k1^2 + 2*p1*k1,
                        d[3] -> k1^2 + 2*p1*k1 + 2*p2*k1}];

top = Topology[facs -> {d[1] -> mh^2 + k1^2,
                        d[2] -> k1^2 + 2*p1*k1,
                        d[3] -> k1^2 + 2*p1*k1 + 2*p2*k1,
                        d[4] -> k1^2 + 2*p2*k1}];

mat = TopologyMatrix[top, set]

TopologyConsistentQ[top, set]

TopologyMatrixReduce[mat, set]

TopologyMatrixReduced[top, set]

TopologyRank[top, set]

TopologyCompleteQ[top, set]

TopologyDependentQ[top, set]

TopologySolve[top, set]

TopologyReduce[top, set]

TopologyScalarProducts[top, set]

TopologyMasses[top, set]

TopologyMomentaFlows[top, set]

TopologyMomentaShifts[top,top,set, Method -> {"Momenta", "FindInstance"}]

Print["
(* ---  ---------------------------------------------------------- *)"];



Print["
(* ---  ---------------------------------------------------------- *)"];



Print["
(* ---  ---------------------------------------------------------- *)"];



(* --- *)

Exit[];
