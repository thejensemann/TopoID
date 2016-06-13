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


red = Collect[res /. reds, {L[_], TopologyIntegralPattern[]}, Acc[Factor[# /. Acc -> Identity]] & ] /. Acc[0] -> 0;


nis = TopologyIntegralList[red]
Length[nis]


(* ::Subsection:: *)
(*Minimize the number of master integrals...*)


(*Get["out/H-NNLO.map"];*)


Get["in/NNLOr.map"];
setup = ATSetup["NNLOr"];
btops = ATTops["pre-b", "NNLOr"];


setup = setup /. Rule[rs, _] -> Rule[rs, {mh^i_?EvenQ -> (s*x)^(i/2)}];


mrels = IntegralRelations[nis, btops, setup, Method -> {"Rules", "Minimize", "KLink"}, Path -> "in/"];


mrels


fin = Collect[red /. mrels, {L[_], TopologyIntegralPattern[]}, Acc[Factor[# /. Acc -> Identity]] & ] /. Acc[0] -> 0;


(* ::Subsection:: *)
(*Draw the master integrals...*)


mis = TopologyIntegralList[fin]
Length[mis]


IntegralShow[mis, btops]


fin /. i_?TopologyIntegralQ :> IntegralPlot[i, btops, Background -> None, EdgeLabeling -> False]
