<< "../../aux/init.m";
<< "../../topoid.m";
(*<< "../../inc/mapti.m";*)

topfiles = FileNames["TT*.def", {"../top"}];
tops = GetTops[topfiles];

Get["../inds.m"];

Print[ListView[tops[[1]], Depth -> 3]];

setup = {ps -> {p1, p2},
         ks -> {v1, v2},
         vs -> {p1, p2, v1, v2},
         ss -> {s1 -> p1*v1, s2 -> p1*v2, s3 -> p2*v1, s4 -> p2*v2, s5 -> v1^2, s6 -> v1*v2, s7 -> v2^2},
         is -> {p1*v1 -> s1, p1*v2 -> s2, p2*v1 -> s3, p2*v2 -> s4, v1^2 -> s5, v1*v2 -> s6, v2^2 -> s7},
         xs -> {s, mh^2}, (* xs -> {MM, MM*x}, *)
         cs -> {p1^2 -> 0, p2^2 -> 0, p1*p2 -> -s/2}, (* cs -> {p1^2 -> 0, p2^2 -> 0, p1*p2 -> -MM/2}, *)
         rs -> {s -> MM, mh^2 -> MM*x}, (* rs -> {}, *)
         ms -> {mh}, (* ms -> {}, *)
         zs -> {p1^2, p2^2},
         np -> 2,
         nk -> 2,
         ns -> 7};

tops = CanTops[tops, setup];

Print[ListView[tops[[1 ;; 1]], Depth -> 3, PageWidth -> 100]];

ints = MapTopInt[tops, ARGS];

Print[ListView[ints[[1 ;; 1]], Depth -> 3, PageWidth -> 100]];

ints = CanTops[ints, setup];

Print[ListView[ints[[1 ;; 1]], Depth -> 3, PageWidth -> 100]];

Quit[];
