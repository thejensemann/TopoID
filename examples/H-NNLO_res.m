(* ::Package:: *)

Quit[];


TopoIDMetric = Plus;
<< TopoID`;


(* ::Subsection:: *)
(*Load a result and insert reductions...*)


SetDirectory[NotebookDirectory[]]


res = Get["in/2.gr.m"];


Cases[res, L[_], {0, Infinity}]


Variables[Cases[res, Acc[x_] -> x, {0, Infinity}]]


TopologyIntegralList[res] // Length


reds = LookUp[res, "NNLOr*", Method -> {"KLink"}, Path -> "in/"];


red = Collect[res /. reds, {L[_], TopologyIntegralPattern[]}, Acc[Factor[# /. Acc -> Identity]] & ]; 


TopologyIntegralList[red] // Length


(* ::Subsection:: *)
(*Minimize the number of master integrals...*)


(*Get["out/H-NNLO.map"];*)


Get["in/NNLOr.map"];
setup = ATSetup["NNLOr"];
btops = ATTops["pre-b", "NNLOr"];


mrels = IntegralRelations[red, btops, setup, Method -> {"Rules", "Minimize", "KLink"}, Path -> "in/"];


fin = Collect[red /. mrels, {L[_], TopologyIntegralPattern[]}, Acc[Factor[# /. Acc -> Identity]] & ];


(* ::Subsection:: *)
(*Draw the master integrals...*)


mis = TopologyIntegralList[fin];


Length[mis]


IntegralShow[mis, btops]
