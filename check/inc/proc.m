(* -- variables in context -- *)
If[False,

   nos = Names["Global`*"];
   << ../../inc/mapdt.m;
   nns = Names["Global`*"];
   Print[Complement[nns, nos]];
   Exit[];

];

(* -- loading -- *)
<< "../../aux/init.m";
<< "../../topoid.m";
<< "../../inc/map.m";

<< ../../inc/top.m


If[False,
   AbsoluteTiming[ParallelMap[{$KernelID,Factorial[#]}&,Range[100]]]//Print;
];


(* -- testing -- *)
If[True,

   Print["Step 0..."];
   setup = {Externals -> {p1, p2, p3, p4},
            Internals -> {v1, v2},
            Internals -> {v1},
            Masses -> {"qu" -> 0, "qd" -> 0, "hb" -> mh,
                       "gl" -> 0, "gh" -> 0, "si" -> 0,
                       "Qu" -> 0, "Qd" -> 0},
            Constants -> {mh^2, s},
            Constraints -> {p3 -> p1, p4 -> p2, p1^2 -> 0, p2^2 -> 0, p1*p2 -> -s/2},
            Rules -> {mh^2 -> s*x}};
   (*Print[ListView[setup, Depth -> 1]];*)

   Print["Step 1..."];
   setup = InitSetup[setup];
   (*Print[ListView[setup, Depth -> 1]];*)

   Print["Step 2..."];
   dias = GetDiagrams["2.qr.dia",
                      Path -> "../dia",
                      Status -> False,
                      Verbosity -> False];
   Print[ListView[dias[[1 ;; 1]], Depth -> 3]];

   Print["Step 3..."];
   {dmaps, dtops} = MapDiagramsToTopologies[dias, setup,
                                            Graph -> True,
                                            Metric -> Euclidean,
                                            Naming -> Inherit,
                                            Parallel -> True,
                                            Verbosity -> False];
   Print[ListView[dmaps[[1 ;;]], Depth -> 1]];
   (*Print[ListView[dtops[[1 ;; 1]], Depth -> 3]];*)

   Print["Step 4..."];
   {gmaps, gtops} = MapTopologiesToTopologies[dtops,
                                              Naming -> Inherit,
                                              Parallel -> True,
                                              Verbosity -> False];
   Print[ListView[gmaps[[1 ;;]], Depth -> 1]];
   (*Print[ListView[gtops[[1 ;; 1]], Depth -> 3]];*)
   gtops[[1]]
   Print["Step 5..."];
   ( {lmaps, ltops} = MapTopologiesToIndependents[gtops, setup,
                                                  Graph -> True,
                                                  Naming -> Inherit,
                                                  Parallel -> False,
                                                  Symmetries -> False,
                                                  Verbosity -> False]; );
   Print[ListView[lmaps[[1 ;;]], Depth -> 1]];
   (*Print[ListView[ltops[[1 ;; 1]], Depth -> 3]];*)

   Print["Step 6..."];
   {bmaps, btops} = MapTopologiesToTopologies[ltops,
                                              Naming -> Inherit,
                                              Parallel -> True,
                                              Verbosity -> False];
   Print[ListView[bmaps[[1 ;;]], Depth -> 1]];
   Print[ListView[btops[[1 ;;]], Depth -> 3]];

];

(* -- leaving -- *)
(*Exit[];*)
(
Get["../../aux/feynmf.m"];

(txt = GraphToFeynMF[gtops[[1 ;;]],
                     (*ImageMargins -> {{10, 15}, {10, 15}},*)
                     EdgeLabeling -> False,
                     PageWidth -> 72,
                     PlotLabel -> True,
                     VertexLabeling -> True];
 str = OpenWrite["../../doc/test.tex"];
 WriteString[str, txt];
 Close[str]);
 );

(txt = GraphToFeynMF[gtops[[1 ;;]],
                     (*ImageMargins -> {{10, 15}, {10, 15}},*)
                     ImageSize -> {200,100},
                     EdgeLabeling -> True,
                     PageWidth -> 200,
                     PlotLabel -> True,
                     VertexLabeling -> False];
 str = OpenWrite["../../doc/test.tex"];
 WriteString[str, txt];
 Close[str]);

Get["../../aux/feynmf.m"];
( GraphToFeynMF[dias[[55;;55]], EdgeLabeling -> Automatic, VertexLabeling -> Automatic] )
