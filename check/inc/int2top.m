<< ../../aux/init.m;
<< ../../topoid.m;



<< ../../inc/syst.m;
<< ../../inc/dat.m;
<< ../../inc/top.m;
<< ../../inc/poly.m;
<< ../../inc/map.m;
<< ../../inc/obj.m;

<< ../../inc/MAIN/int2int.m;
<< ../../inc/MAIN/int2top.m;
<< ../../inc/MAIN/top2int.m;
<< ../../inc/MAIN/top2top.m;




Print["
(* --- setup ----------------------------------------------------- *)"];

(set = Setup[
  Externals
    -> {p1, p2, p3, p4},
  Internals
    -> {k1},
  Masses
    -> {"gh" -> 0, "gl" -> 0, "hb" -> mh,
        "qd" -> 0, "qu" -> 0, "si" -> Null},
  Constants
    -> {s, mh^2},
  Constraints
    -> {p3 -> p1, p4 -> p2, p1^2 -> 0, p2^2 -> 0, p1*p2 -> -s/2},
  Rules
    -> {mh^2 -> s*x}]);

Print /@ set;

Print["
(* --- topology -------------------------------------------------- *)"];

(top = Topology[
  name -> "T1",
  facs -> {d[1] -> mh^2 + k1^2,
           d[2] -> k1^2 + 2*p2*k1,
           n[3] -> k1^2 + 2*p1*k1 + 2*p2*k1 - s}]);

Print /@ top;

{map1, top} = CompleteTopology[top, set];
map2
Print /@ top;

top = ReduceTopology[top, set];

Print /@ top;

{map2, top} = CanonicalizeTopology[top, set];
map2
Print /@ top;

top = InspectTopology[top];

Print /@ top;

Print["
(* --- integral -------------------------------------------------- *)"];

(int = Topology[
  name -> "I1",
  inds -> {2, 2},
  facs -> {d[2] -> k1^2,
           d[1] -> mh^2 + k1^2 + 2*p1*k1 + 2*p2*k1 - s}]);

{iap, int} = CanonicalizeIntegral[int, set];
iap
IntegralQ[int]




MapTopologyToTopology[int, top, Method -> {}]

MapIntegralToTopology[int, top, set]

expr = MapIntegralToTopology["v3", int, top, set]






(* --- *)

Exit[];
