<< "../../aux/init.m";
<< "../../topoid.m";
(*<< "../../inc/mapti.m";*)


topfiles = FileNames["TT*.def", {"../top"}];
Atops = GetTops[topfiles];


Jtops = GetTops["2.qr.def", Path -> "/users/ttp/jens/projects/gg_to_h/dat/2.qr/map"];


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


Atops = CanTops[Atops, setup];


(* Alexey's master integrals *)
Ainds = {
             TTA[1, 1, 1, 0, 0, 0, 0],  (* U1  *)
             TTA[1, 1, 1,-1, 0, 0, 0],  (* U1a *)
             TTF[1, 0, 1, 1, 0, 1, 0],  (* U2  *)

             TTF[1, 0, 1, 0, 1, 1, 0],  (* U3  *)
             TTF[1, 1, 0, 0, 1, 1, 0],  (* U4  *)
             TTF[1, 1, 1, 0, 1, 1, 0],  (* U5  *)

             TTE[1, 0, 1, 1, 1, 0, 1],  (* U6  *)
             TTG[1, 0, 1, 1, 1, 0, 1],  (* U7  *)
             TTG[1,-1, 1, 1, 1, 0, 1],  (* U7a *)

             TTA[1, 1, 1, 1, 0, 1, 0],  (* U8  *)
             TTE[1, 0, 1, 1, 1, 1, 0],  (* U9  *)
             TTC[1, 1, 1, 0, 1, 1, 0],  (* U10 *)

             TTK[1, 1, 1, 1, 1, 1, 0],  (* U11 *)
             TTJ[1, 1, 1, 1, 1, 1, 0],  (* U12 *)
             TTA[1, 1, 1, 1, 1, 1, 1],  (* U13 *)

             TTC[1, 1, 1, 1, 1, 1, 1],  (* U14 *)
             TTD[1, 1, 1, 1, 1, 1, 1],  (* U15 *)
             TTE[1, 1, 1, 1, 1, 1, 1],  (* U16 *)

             TTG[1, 1, 1, 1, 1, 1, 1],  (* U17 *)
             TTK[1, 1, 1, 1, 1, 1, 1],  (* U18 *)
             TTH[1, 1, 1, 1, 1, 1, 1]   (* U19 *)
         };

Aints = MapTopInt[Atops, Ainds];

Aints = CanTops[Aints, setup];

Aints = Aints /. {MM -> s};


Jtops = CanTops[Jtops, setup];


Jinds = {GT44s1[1, 1, 0, 1, 0, 0, 1],
         GT44s1[1, 1, -1, 0, 1, 1, 0],
         GT44s1[1, 1, 0, 0, 1, 1, 0],
         GT44s1[1, 0, 1, 1, 0, 0, -1],
         GT44s1[1, 1, 1, 0, 1, 1, 0]};

Jints = MapTopInt[Jtops, Jinds];

Jints = CanTops[Jints, setup];

Jints = Jints /. {MM -> s};


Print[ScalefulQ[(arep /. #)[[1]]*(arep /. #)[[2]], apar /. #] & /@ Jints];


MapIntInt[Join[Select[Jints, ((name /. #) === "GT44s1m1") &], Select[Aints, ((name /. #) === "TTFm2") &]]]


MapIntInt[Join[Aints, Jints]][[2]]


Print[ListView[Select[Aints, ((name /. #) === "TTEm1") &]]];


Quit[];
