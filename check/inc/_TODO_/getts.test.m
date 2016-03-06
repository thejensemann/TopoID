<< "../../aux/init.m";
<< "../../topoid.m";
(*<< "../../inc/getts.m";*)


test = GetTops[{"TTA.def", "TTB.def"}, Path -> "../top"];
Print[test // ListView];


Print[ExtractSetup[test[[1]]] // ListView];


setup = {ps -> {b1, b2},
         ks -> {w1, w2},
         vs -> {p1, p2, v1, v2},
         ss -> {s1 -> p1*v1, s2 -> p1*v2, s3 -> p2*v1, s4 -> p2*v2, s5 -> v1^2, s6 -> v1*v2, s7 -> v2^2},
         is -> {p1*v1 -> s1, p1*v2 -> s2, p2*v1 -> s3, p2*v2 -> s4, v1^2 -> s5, v1*v2 -> s6, v2^2 -> s7},
         xs -> {s, mh^2}, (* xs -> {MM, MM*x}, *)
         cs -> {p1^2 -> 0, p2^2 -> 0, p1*p2 -> -s/2}, (* cs -> {p1^2 -> 0, p2^2 -> 0, p1*p2 -> -MM/2}, *)
         rs -> {mh^2 -> s*x}, (* {s -> MM, mh^2 -> MM*x}, *) (* rs -> {}, *)
         ms -> {mh}, (* ms -> {}, *)
         zs -> {p1^2, p2^2},
         np -> 2,
         nk -> 2,
         ns -> 7};

Print[ApplySetup[test[[1]], setup] // ListView];


Quit[];
