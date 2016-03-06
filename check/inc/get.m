(* -- name space -- *)
If[0 == 1,
   nos = Names["Global`*"];
   << ../../inc/scale.m;
   nns = Names["Global`*"];
   Print[Complement[nns, nos]]
];

(* -- testing -- *)
<< "../../aux/init.m";
<< "../../topoid.m";
(*<< "../../inc/getds.m";*)

SetOptions[Status, Temporary -> True];

SetOptions[GetDias, Path -> "../dia", Status -> True, Verbosity -> 0];

dias = GetDias["0.gg.dia"];
dias = GetDias["1.gr.dia"];
dias = GetDias["2.qr.dia"];
dias = GetDias["3.qp.dia"];

Print[ListView[dias[[1 ;; 1]], Depth -> 3]];

Quit[];
