Exit[];

<< ./init.m;
<< TopoID`Mapping`;

(* --- data --------------------------------------------------------- *)

(dmaps =
 {{fr -> "D1", to -> "DT1", id -> {0, 0, 2, 1}},
  {fr -> "D2", to -> "DT2", id -> {0, 2, 3, 1}},
  {fr -> "D3", to -> "DT3", id -> {0, 2, 1, 3}},
  {fr -> "D4", to -> "DT4", id -> {0, 3, 1, 2}},
  {fr -> "D5", to -> "DT5", id -> {0, 2, 3, 1}},
  {fr -> "D6", to -> "DT6", id -> {0, 2, 1, 3}},
  {fr -> "D7", to -> "DT7", id -> {0, 3, 1, 2}},
  {fr -> "D8", to -> "DT8", id -> {2, 3, 1, 2}},
  {fr -> "D9", to -> "DT9", id -> {2, 1, 3, 2}},
  {fr -> "D10", to -> "DT10", id -> {2, 4, 1, 3}},
  {fr -> "D11", to -> "DT11", id -> {2, 1, 4, 3}}});

(gmaps =
 {{fr -> "DT10", to -> "GT10", id -> {1, 2, 3, 4}},
  {fr -> "DT11", to -> "GT10", id -> {1, 2, 3, 4}},
  {fr -> "DT2", to -> "GT10", id -> {1, 3, 4}},
  {fr -> "DT3", to -> "GT10", id -> {1, 3, 4}},
  {fr -> "DT5", to -> "GT10", id -> {1, 3, 4}},
  {fr -> "DT6", to -> "GT10", id -> {1, 3, 4}},
  {fr -> "DT8", to -> "GT10", id -> {1, 3, 4}},
  {fr -> "DT9", to -> "GT10", id -> {1, 3, 4}},
  {fr -> "DT1", to -> "GT10", id -> {1, 4}},
  {fr -> "DT4", to -> "GT4", id -> {1, 2, 3}},
  {fr -> "DT7", to -> "GT4", id -> {1, 2, 3}}});

(lmaps =
 {{fr -> "GT10", to -> "GT10s1", id -> {0, 1, 2, 3}},
  {fr -> "GT10", to -> "GT10s2", id -> {1, 0, 2, 3}},
  {fr -> "GT10", to -> "GT10s3", id -> {1, 2, 3, 0}},
  {fr -> "GT4", to -> "GT4s1", id -> {1, 2, 3}}});

(bmaps =
 {{fr -> "GT10s1", to -> "GT10s1", id -> {1, 2, 3}},
  {fr -> "GT10s2", to -> "GT10s2", id -> {1, 2, 3}},
  {fr -> "GT10s3", to -> "GT10s3", id -> {1, 2, 3}},
  {fr -> "GT4s1", to -> "GT4s1", id -> {1, 2, 3}}});

(src = Topology[
  name
    -> "LOggDT1",
  facs
    -> {p[4] -> mh^2 + v1^2,
        p[3] -> -s - 2*p1*v1 - 2*p2*v1 + v1^2}]);

(trg = Topology[
  name
    -> "LOggGT1",
  facs
    -> {p[3] -> mh^2 + 2*p2*v1 + v1^2,
        p[1] -> v1^2,
        p[4] -> s - 2*p1*v1 + 2*p2*v1 + v1^2,
        p[2] -> -2*p1*v1 + v1^2}]);

Print["
(* --- MappingPattern -------------------------------------------- *)"];

?fr;
?to;
?id;

??MappingPattern;

MappingPattern[] // Print;

Print["
(* --- MappingQ -------------------------------------------------- *)"];

?MappingQ;

MappingQ[map] // Print;

map = Mapping["D1", "DT1", {0, 0, 1, 2}];

Print[map];

MappingQ[map] // Print;

Print["
(* --- MappingsQ ------------------------------------------------- *)"];

?MappingsQ;


(* TODO *)


MappingQ[map] // Print;

map = Mapping["D1", "DT1", {0, 0, 1, 2}];

Print[map];

MappingQ[map] // Print;


MappingListQ[{1}]


Print["
(* --- Mapping --------------------------------------------------- *)"];

?Mapping;

Mapping["D1", "DT1", {0, 0, 1, 2}] // Print;

Mapping["D1", "DT1"] // Print;
Mapping["D1"] // Print;

Mapping["D1", {1, 2, 3, 4}] // Print;

Mapping[{1, 2, 3, 4}] // Print;

Mapping[] // Print;

Mapping[src, "GT1", {1}] // Print;
Mapping[src, {1}] // Print;

Mapping["DT1", trg, {1}] // Print;
Mapping["DT1", trg] // Print;

Mapping[src, trg, {1}] // Print;

Mapping["DT1", "DT1", 4] // Print;

Mapping[src, "GT1", 4] // Print;
Mapping[src, trg, 4] // Print;
Mapping[src, "GT1"] // Print;
Mapping[src, 4] // Print;
Mapping[src, trg] // Print;
Mapping[src] // Print;

Print["
(* --- PermuteMapping -------------------------------------------- *)"];

PermuteMapping[dmaps[[1]], {1, 2}] // Print;
PermuteMapping[dmaps[[1]], {2, 1}] // Print;

(* overload: plural *)
PermuteMapping[dmaps, {1, 2, 3, 4}] // Print;
PermuteMapping[dmaps, {2, 4, 1, 3}] // Print;

(* check: permutation length *)
PermuteMapping[dmaps[[2]], {1, 2}] // Print;

(* trap: no permutation *)
PermuteMapping[dmaps[[2]], {0, 1}] // Print;

PermuteMapping[{}, {1, 2}] // Print;

Print["
(* --- ReverseMapping -------------------------------------------- *)"];

ReverseMapping[dmaps[[1]]] // Print;
ReverseMapping[dmaps[[2]]] // Print;
ReverseMapping[dmaps[[3]]] // Print;
ReverseMapping[dmaps[[-1]]] // Print;
ReverseMapping[dmaps[[-2]]] // Print;

(* with length *)
ReverseMapping[dmaps[[1]], 4] // Print;

(* TODO *)
(* with target *)
ReverseMapping[{fr -> "LOggDT1", to -> "LOggGT1", id -> {1, 4}}]
ReverseMapping[{fr -> "LOggDT1", to -> "LOggGT1", id -> {1, 4}}, trg]
ReverseMapping[{fr -> "LOggDT1", to -> "LOggGT1", id -> {1, 3}}, trg]

(* overload: plural *)
ReverseMapping[dmaps] // Print;

ReverseMapping[{}] // Print;

Print["
(* --- ComposeMapping -------------------------------------------- *)"];

(* main: atomic *)

ComposeMapping[id /. dmaps[[1]], id /. gmaps[[-3]]] // Print;

ComposeMapping[id /. dmaps[[-1]], id /. gmaps[[3]]] // Print;

(* main: singular, singular *)

ComposeMapping[dmaps[[1]], gmaps[[-3]]] // Print;

ComposeMapping[dmaps[[-1]], gmaps[[3]]] // Print;

(* overload: singular, plural *)
ComposeMapping[dmaps[[1]], gmaps] // Print;
ComposeMapping[dmaps[[1]], lmaps] // Print;

(* overload: plural, singular *)
ComposeMapping[lmaps, bmaps[[1]]] // Print;
ComposeMapping[dmaps, lmaps[[1]]] // Print;

(* overload: plural, plural *)
ComposeMapping[lmaps, bmaps] // Print;
ComposeMapping[dmaps, lmaps] // Print;

(* overload: singular *)
ComposeMapping[dmaps[[1]]] // Print;
ComposeMapping[bmaps] // Print;

(* overload: multiple mapping(s) *)
ComposeMapping[dmaps, gmaps, lmaps] // Print;
ComposeMapping[gmaps, lmaps, bmaps] // Print;
ComposeMapping[dmaps, gmaps, lmaps, bmaps] // Print;

ComposeMapping[{}] // Print;

(* overload: nothing *)
ComposeMapping[] // Print;

(* overload: rewrite list as sequence *)
ComposeMapping[{lmaps, bmaps}] // Print;

(*
Show[LayeredGraphPlot[MappingToGraph[%]]];
*)

Print["
(* --- SelectMapping --------------------------------------------- *)"];

SelectMapping[{dmaps}, {"D1"}] // Print;
SelectMapping[{dmaps, gmaps}, {"D1"}] // Print;
SelectMapping[{dmaps, gmaps, lmaps}, {"D1"}] // Print;
SelectMapping[{dmaps, gmaps, lmaps, bmaps}, {"D1"}] // Print;

SelectMapping[{dmaps, gmaps, lmaps, bmaps}, {"D1", "D2"}] // Print;

(* overload: rewrite single mapping(s) *)
SelectMapping[{dmaps, gmaps, lmaps[[1]], bmaps}, {"D1"}] // Print;

(* overload: rewrite mapping(s) as list *)
SelectMapping[dmaps, {"D1"}] // Print;
SelectMapping[dmaps, gmaps, {"D1"}] // Print;
SelectMapping[dmaps, gmaps, lmaps, {"D1"}] // Print;
SelectMapping[dmaps, gmaps, lmaps, bmaps, {"D1"}] // Print;

(* overload: rewrite key(s) as list *)
SelectMapping[{dmaps, gmaps, lmaps, bmaps}, "D1", "D2"] // Print;
SelectMapping[dmaps, gmaps, lmaps, bmaps, "D1", "D2"] // Print;

(* nothing *)
SelectMapping[{}, {}] // Print;
SelectMapping[{}] // Print;
SelectMapping[] // Print;

bmaps


SelectMapping[dmaps, gmaps, lmaps, bmaps, "*0s*"]  // Print;
SelectMapping[dmaps, gmaps, lmaps, bmaps, RegularExpression["GT\\d$"]] // Print;


SelectMapping[{dmaps, gmaps, lmaps, bmaps}, "GT10"]

SelectBackwardMapping[{dmaps, gmaps, lmaps}, {"GT10s*"}]

SelectMapping[dmaps, {""}]

(*
Show[LayeredGraphPlot[MappingToGraph[%]]];
*)

Print["
(* --- MappingToRule --------------------------------------------- *)"];

MappingToRule[lmaps[[1]]] // Print;

MappingToRule[lmaps] // Print;

(* trap: conversion *)
MappingToRule[Mapping["*", "*", {1, 1, 1}]] // Print;

Print["
(* --- MappingToGraph -------------------------------------------- *)"];

MappingToGraph[bmaps] // Print;
MappingToGraph[lmaps, bmaps] // Print;
MappingToGraph[gmaps, lmaps, bmaps] // Print;
MappingToGraph[dmaps, gmaps, lmaps, bmaps] // Print;

MappingToGraph[{}] // Print;
MappingToGraph[] // Print;

(* overload: rewrite single mapping(s) *)
MappingToGraph[lmaps, bmaps[[1]]] // Print;

(* overload: rewrite list as sequence *)
MappingToGraph[{lmaps, bmaps}] // Print;

(*
Show[LayeredGraphPlot[%]];
*)

Print["
(* --- ToMapping ------------------------------------------------- *)"];

expr = TOP[-1,1,2] + TOP[1,-1,2] + TOP[2,1,-1] + AAA*TOP[2,1,-1] + FOP[1];

<< TopoID`Mapping`;
ToMapping[expr, {"TOP"}]
ToMapping[expr, "TOP", FOP, Naming -> "InheritUnique"]

ToMapping::example

ToMapping[TOP[1,2,3]]
ToMapping[{TOP[1,2,3], TOP[2,3,4]}]

ToMapping[{}]
ToMapping[]


ToMapping[int[1,2]->i1]
ToMapping[i1->int[1,2]]
ToMapping[{i1->int[1,2], i2 -> int[2,1]}]
ToMapping[{i1->int[1,2], int[2,1] -> i2}]
ToMapping[int[1,2]->int[2,1]]
ToMapping[a -> b[1]]
ToMapping[a -> b]


TopoID`Mapping`Private`$ToMappingAccountRules//FullForm

MappingToRule[%]

(* --- *)

Exit[];











Some examples:
- contract lines of the source
    {fr -> \"D1\", to -> \"DT1\", id -> {0, 0, 1, 2}},
- merely rearrange lines
    {fr -> \"D33\", to -> \"DT33\", id -> {2, 1, 4, 3}},
- select lines from the target
    {fr -> \"DT1\", to -> \"GT33\", id -> {1, 4}},
- identical mapping
    {fr -> \"DT33\", to -> \"GT33\", id -> {1, 2, 3, 4}}.

Some examples:
- Mapping[\"D1\", \"DT1\", {0, 0, 1, 2}]
  {fr -> \"D1\", to -> \"DT1\", id -> {0, 0, 1, 2}},
- Mapping[\"D1\", {1, 2, 3, 4}]
  {fr -> \"D1\", to -> \"D1\", id -> {1, 2, 3, 4}},
- Mapping[\"DT1\", \"DT1\", 4]
  {fr -> \"DT1\", to -> \"DT1\", id -> {1, 2, 3, 4}},
- Mapping[<src>, <trg>]
  {fr -> \"DT1\", to -> \"LOggGT1\", id -> {1, 2}},
- Mapping[<src>]
  {fr -> \"DT1\", to -> \"LOggDT1\", id -> {1, 2}}.

Possible calls:
- ToMapping[TTA[1, 1, 1, 1, 0, 0, 0]],
- ToMapping[{TTA[1, 1, 1, 1, 0, 0, 0], TTB[1, 1, 1, 0, 1, 1, 0]}],
- ToMapping[U1 -> TTA[1, 1, 1, 0, 0, 0, 0]],
- ToMapping[TTA[1, 1, 1, 0, 0, 0, 0] -> U1],
- ToMapping[<exp>],
- ToMapping[<exp>, {TTA}],
- ToMapping[<exp>, {\"TTA\"}],
- ToMapping[<exp>, \"TT*\"],
- ToMapping[<exp>, RegularExpression[\"TT.*\"]].
