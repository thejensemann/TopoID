(* ::Package:: *)

Quit[];


TopoIDMetric = Plus;
<< TopoID`;


(* ::Subsection:: *)
(*Setup the process and kinematics...*)


setup = Setup[
  Externals -> {p1, p2, p3, p4},
  Internals -> {v1, v2},
  Masses -> {
    "gh" -> 0,
    "gl" -> 0,
    "hb" -> mh,
    "qd" -> 0,
    "qu" -> 0,
    "si" -> Null},
  Constants -> {s, mh^2},
  Constraints -> {
    p3 -> p2,
    p4 -> p1,
    p1^2 -> 0,
    p2^2 -> 0,
    p1*p2 -> -s/2},
  Rules -> {mh^2 -> s*x}
];


(* ::Subsection:: *)
(*Read the diagrams from QGRAF ...*)


SetDirectory[NotebookDirectory[]]


tops[0] = GetDiagrams["2.gr.dia", Path -> "../etc/dia"];


DiagramGrid[RandomChoice[tops[0], 20]]


(* ::Subsection:: *)
(*Translate diagrams to topologies...*)


{maps[1], tops[1]} = MapDiagramToTopology[tops[0], setup];


TopologyGrid[RandomChoice[tops[1], 20]]


tops[1] = CutsTopology[tops[1]];


(* ::Subsection:: *)
(*Minimize their set...*)


{maps[2], tops[2]} = MinimizeTopologies[tops[1], Naming -> Iterate];


TopologyGrid[tops[2], VertexCoordinateRules -> {}]


TopologyDependentQ[tops[2], setup]


(* ::Subsection:: *)
(*Map to linearly independent ones...*)


{maps[3], tops[3]} = MapTopologyToIndependents[tops[2], setup];


TopologyGrid[tops[3], VertexCoordinateRules -> {}]


tops[3] = CutsTopology[tops[3]];


TopologyDependentQ[tops[3], setup]


TopologyCompleteQ[tops[3], setup]


CutsQ[tops[3]]


(* ::Subsection:: *)
(*Minimize their set once more...*)


{maps[4], tops[4]} = MinimizeTopologies[tops[3], Naming -> Iterate["BT"]];


TopologyGrid[tops[4], VertexCoordinateRules -> {}]


TopologyDependentQ[tops[4], setup]


TopologyCompleteQ[tops[4], setup]


Print /@ Cuts[tops[4]];


(* ::Subsection:: *)
(*Make nicer graphs...*)


TopologyManipulate[tops[4][[1]]]


TopologyManipulate[tops[4][[5]]]


TopologyManipulate[tops[4][[6]]]


TopologyGrid[tops[4]]


(* ::Subsection:: *)
(*Visualization of the mapping...*)


LayeredGraphPlot[MappingToGraph[maps[3], maps[4]], VertexLabeling -> True, ImageSize -> {1500, 500}]


SelectMapping[maps /@ Range[4], {"D123"}]


(* ::Subsection:: *)
(*Prepare "generic" topologies...*)


(gtops = tops[2]
 // ReduceTopology[#, setup] &
 // CutsTopology
 // ZeroCutsTopology
 // PrepareTopology[#, setup] & );


ttop = gtops[[1]];


subt /. ttop


Join @@ (subt /. ttop)


smaps = MapIndexed[Mapping[(name /. ttop) <> "s" <> ToString[#2[[1]]], name /. ttop, #1] & , Join @@ (subt /. ttop)];
stops = MapToTopology[ReverseMapping[smaps, ne /. setup], ttop];


TopologyGrid[stops]


zero /. ttop


zmaps = MapIndexed[Mapping[(name /. ttop) <> "z" <> ToString[#2[[1]]], name /. ttop, #1] & , zero /. ttop];
ztops = MapToTopology[ReverseMapping[zmaps, ne /. setup], ttop];


TopologyGrid[ztops]


cuts /. ttop


(* ::Subsection:: *)
(*Prepare "basic" topologies...*)


gbmaps = ComposeMapping[maps[3], maps[4]];


LayeredGraphPlot[MappingToGraph[gbmaps], VertexLabeling -> True, ImageSize -> {1200, 400}]


SymmetrizeTopology//Options


cuts /. tops[4]


(btops = tops[4]
 // ReduceTopology[#, setup] &
 // SymmetrizeTopology
 // CutsTopology
 // ZeroCutsTopology
 // PrepareTopology[#, setup] & );


ttop = btops[[1]];


subt /. ttop


zero /. ttop


cuts /. ttop


symm /. ttop


ymaps = MapIndexed[Mapping[(name /. ttop) <> "y" <> ToString[#2[[1]]], name /. ttop, #1] & , Join @@ (symm /. ttop)];
ytops = MapToTopology[ReverseMapping[ymaps, ne /. setup], ttop];


TopologyGrid[ytops]


(* ::Subsection:: *)
(*Generate FORM code...*)


gcode = FORMTopologyProcess[gbmaps, gtops, btops];


WriteStringFile["out/H-NNLO_gcode.inc", ToFORMCodeString[Riffle[gcode, "\n\n\n"]]];


bcode = FORMTopology[btops];


WriteStringFile["out/H-NNLO_bcode.inc", ToFORMCodeString[Riffle[bcode, "\n\n\n"]]];
