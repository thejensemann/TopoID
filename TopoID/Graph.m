(* -- "Graph.m": Visualization Routines ----------------------------- *)

(* --- provided functions:                                              TODO

GraphTopologies[<top(s)>, [<set>], [<lvl>], [<n>]] -> <tops>
-- reconstruct graphs based on denominators and integrate them into
   given topology data
[>facs<, [>setp<] -> >pros<, >legs<]

[(facs, set) -> (pros, legs)]

TopologyToGraphs[<top(s)>, [opts]] -> <graph(s)>
-- convert internal graph data into standard Mathematica format

TopologyPlot...

*)

(* --- package begin ------------------------------------------------ *)

Unprotect["TopoID`Graph`*"];

ClearAll["TopoID`Graph`*", "TopoID`Graph`Private`*"];

BeginPackage[
  "TopoID`Graph`",
  {"TopoID`Common`", "TopoID`System`",  (* TODO *)
   "TopoID`Setup`",
   "TopoID`Topology`",
   "TopoID`Mapping`",
   "TopoID`Object`",
   "TopoID`Core`"}];

{$TopologyToGraphKeywords,TopologyToGraph};

{NickelIndex, NickelRules};

{$GraphTopologiesExternals, $GraphTopologiesMethods,
 GraphTopologies, GraphTopology};

{TopologyStyle, $TopologyStyle, $DiagramStyle, $IntegralStyle,
 $TopologyPlotVertexCoordinateRules,
 TopologyPlot, DiagramPlot, IntegralPlot};

{TopologyGrid, DiagramGrid, IntegralGrid,
 TopologyShow, DiagramShow, IntegralShow};

{TopologySlider, DiagramSlider, IntegralSlider};

{TopologyManipulate, DiagramManipulate, IntegralManipulate};

{MappingPlot};

Begin["`Private`"];

(* --- TopologyToGraph ---------------------------------------------- *)

$TopologyToGraphKeywords =
{"None", "Labels", "Momenta", "Symbols", "Factors", "Indices",
 "Positions", Subscript[_]};

Options[TopologyToGraph] =
{Externals -> Subscript["Momenta"],
 Internals -> "Positions"};
(* cf. $TopologyToGraphKeywords *)

TopologyToGraph::usage = "\
TopologyToGraph[<top(s)>, [opts]] converts the internal graph data of \
<top(s)> into standard Mathematica form.
The options [Externals] and [Internals] allow to select information \
used for creating edge labels, cf. $TopologyToGraphKeywords.";

TopologyToGraph::invalid = "\
Warning: Invalid value \"`2`\" for option [`1`] will be ignored.  \
Possible option values can be chosen from `3` (as string).";

(* overload: plural *)
TopologyToGraph[tops_?TopologyListQ, opts:OptionsPattern[]] :=
  TopologyToGraph[#, opts] & /@ tops;

(* main *)
TopologyToGraph[
  top:TopologyPattern["Graph"], opts:OptionsPattern[]] :=
    Module[
      {sf, is,fs,ls,ps, fl,il, f,k, g},

      (* helper: rewrite subscripts *)
      sf = (#
            /. s_Symbol /; Context[s] =!= "System`" :> ToString[s]
            /. s_String :> First[StringCases[
              s,
              {RegularExpression["(\\D+)(\\d+)"] -> Subscript["$1", "$2"],
               RegularExpression["(.*)"] -> "$1"}]]
            /. s_[i_] :> Subscript[s, i]) & ;

      (* necessary data *)
      {is, fs, ls, ps} = {inds, facs, legs, pros} /. top;

      (* check: no indices *)
      If[is === inds,
         is = MapIndexed[a[#2[[1]]] & , ps]];

      (* check: no factors *)
      If[fs === {},
         fs = #[[1]] -> #[[2]]^2 - m[#[[1]]]^2 & /@ ps];

      (* externals: no factors *)
      fl = Function[
        l, If[Length[#] > 1, #, #[[1]]] & @
          Cases[l[[{3, 4}]], in[_] | out[_]] -> l[[2]]^2 -
            If[Head[l[[1]]] === String, m[l[[1]]]^2, l[[1]]^2]] /@ ls;

      (* externals: no indices *)
      il = MapIndexed[e[#2[[1]]] & , ls];

      (*
      external, internal data:
        from -> to, label, momentum, symbol -> factor, index, position
      *)

      ls = MapThread[
        {Rule @@ #1[[{3, 4}]], #1[[1]], #1[[2]], #2, #3, #4} & ,
        {ls, fl, il, Range[Length[ls]]}];

      ps = MapThread[
        {Rule @@ #1[[{3, 4}]], #1[[1]], #1[[2]], #2, #3, #4} & ,
        {ps, fs, is, Range[Length[ps]]}];

      (* external, internal labels: prescriptions *)

      g[x_] :=
      {"None" -> ({} & ),
       "Labels" -> (#[[2]] & ),
       "Momenta" -> (#[[3]] & ),
       "Symbols" -> (#[[4, 1]] & ),
       "Factors" -> (#[[4, 2]] & ),
       "Indices" -> (#[[5]] & ),
       "Positions" -> (#[[6]] & ),
       Subscript[s_] :> Composition[sf, s /. g[x]],
       f_Function :> f,
       k_ :> (Message[TopologyToGraph::invalid,
         x, k, $TopologyToGraphKeywords]; {} & )};

      (* apply to edge data *)

      ls = MapThread[
        If[#2 === {}, #1[[1]], {#1[[1]], #2}] & ,
        {ls, (OptionValue[Externals] /. g[Externals]) /@ ls}];

      ps = MapThread[
        If[#2 === {}, #1[[1]], {#1[[1]], #2}] & ,
        {ps, (OptionValue[Internals] /. g[Internals]) /@ ps}];

      (* result: external, internal edges *)
      Join[ls, ps]];

(* trap *)
TopologyToGraph[___] :=
  (Message[TopologyToGraph::usage];
   Abort[]);

(* --- NickelIndex -------------------------------------------------- *)

NickelIndex[
  gr:{{_Rule, _}..}] :=
  Module[
    {vls,vlt, lrs, gs, ni,
     pms, j, prs, tmp, min},
    (* old vertex labels (internal) *)
    vls = DeleteCases[Union @@ List @@@ First /@ gr, (in | out)[_]];
    (* new vertex labels *)
    vlt = MapIndexed[First[#2] & , vls];
    (* labeling rules *)
    lrs = Prepend[Rule @@@ Transpose[{vls, vlt}], (in | out)[_] -> "e"];
    (* apply to graph *)
    gs = {First[#] /. lrs, Last[#]} & /@ gr;
    (* subroutine: compute Nickel index *)
    ni[p_] := Module[
      {r, g, i},
      (* permutation rules *)
      r = Rule @@@ Transpose[{vlt, p}];
      (* apply permutation, sort vertices, sort edges *)
      g = Sort[{Sort[First[#] /. r], Last[#]} & /@ gs];
      (* build adjacency lists *)
      i = Function[v, Cases[g, {v -> w_, l_} -> {w, l}]] /@ vlt;
      i = If[# === {}, {{}, {}}, Transpose[#]] & /@ i;
      i = {First /@ i, Last /@ i};
      (* result: add separators *)
      i = (Join @@ Riffle[#, {{"|"}}]) & /@ i;
      Join[First[i], {":"}, Last[i]]];

    (*pms = {vlt};
    Do[
      prs = {j -> #, # -> j} & /@ vlt;
      pms = Join @@ (pms /. # & /@ prs);
      tmp = ni /@ pms;
      min = First[Sort[tmp]];
      pms = Pick[pms, # === min & /@ tmp]
      ,
      {j, vlt}];
    Return[min];*)

    First[Sort[ni /@ Permutations[vlt]]]
  ];

(* overload: no labels *)
NickelIndex[gr:{__Rule}] :=
  NickelIndex[{#, Null} & /@ gr];

(* overload: sequence *)
NickelIndex[gr__] :=
  NickelIndex[{gr}];

(* alias: Topology *)
NickelIndex[top:TopologyPattern["Graph"]] :=
  NickelIndex[TopologyToGraph[
    top, Externals -> "Labels", Internals -> "Labels"]];

(* trap *)
NickelIndex[___] :=
  (Message[NickelIndex::usage];
   Abort[]);



(* TODO: NickelIndex -> TopologyToNickelIndex *)

(* TODO:  *)







(* N.B.:

TODO: changed

- <number_of_vertices> + 1 -- external vertex label,
- 0 -- separating character.
*)

(* --- GraphTopologies ---------------------------------------------- *)

GraphTopologies::usage = "\                                                   TODO
GraphTopologies[<top>, [<set>], [<lvl>], [<n>]] tries to reconstruct \
for topology factors of <top> within kinematic setup [<set>] at most \
[<n>] graphs with number of lines given by level specifications \
('levelspec') [<lvl>].";

GraphTopologies::externals = "\
TODO";

GraphTopologies::loops = "\
Warning: The factors of topology \"`1`\" do not allow for a set of `2` \
linearly independent propagators.
Perhaps loops were integrated out or a wrong number of external \
momenta (>Externals< or >ps<) was specified for the used kinematic \
setup.";

$GraphTopologiesExternals =
{"Forward", "In", "Out", _List};

(* N.B.:
- "Forward" -- each external momentum has an incoming and an outgoing
  external leg,
- "In" -- all external momenta are attached to incoming legs, one more
  outgoing leg is assumed,
- "Out" -- all external momenta are attached to outgoing legs, one more
  incoming leg is assumed,
- <list> -- explicit assignments of external momenta for all external
  legs, e.g.
    {in[1] -> p1, in[2] -> p2, out[3] -> p1, out[4] -> p2}
  with same effect as "Forward" or
    {in[1] -> p1, in[2] -> p2, out[3] -> p1 + p2}
  which corresponds to "In".
*)

$GraphTopologiesMethods =
{"Symbol", "Flows", "1PI", "SE", "Order", "Filter", "Check"};

(* N.B.:
- "Symbol" -- constrain reconstruction by factor symbols,
- "Flows" -- discard factors without momenta flows
- "1PI" -- allow only one-particle-irreducible graphs,
- "SE" -- generate also self-energy insertions.
- "Order" -- reduce combinatorics by sorting in intermediate steps,
- "Filter" -- filter by calculating the Nickel index,
- "Check" -- check momentum conservation.
*)

Options[GraphTopologies] =
{Externals -> "In",
 Method -> {"1PI", "SE", "Order"},
 Verbosity -> False};

(* overload: plural *)
GraphTopologies[tops_?TopologyListQ, args___] :=
  Join @@ (GraphTopologies[#, args] & /@ tops);

(* main *)
GraphTopologies[
  top:TopologyPattern[], set:SetupPattern[], lv_:All, n_Integer:-1,
    opts:OptionsPattern[]] :=
  Module[
    {                                                                         (* TODO *)
       chk, m,m2r, lw, es,vb, et, mr,fs, sp,fp, mrs, prs, ord, lis, els, lgs,
       grk, gis,
       ivs, insert,grow, sort, gfs,
       split, order},

    (* -- options -- *)

    (* helper: check for correct list <l> (for Externals) *)
    chk[l_] := And @@ (MatchQ[#, (in | out)[_Integer] -> _] & /@ l) &&
      Length[l] === Length[DeleteDuplicates[First /@ l]];

    (* helper: rewrite powers of masses *)
    m2r = _.*m_^_. :> m /; MemberQ[ms /. set, m];

    lw = $CheckLevel[lv, Length[facs /. top]];

    {es, me, vb} = OptionValue[{Externals, Method, Verbosity}];
    me = $CheckMethod[me, $GraphTopologiesMethods];
    vb = vb /. VerbosityRules;

    (* check: Externals *)
    et = Externals /. Options[GraphTopologies];
    If[!MatchQ[es, Alternatives @@ $GraphTopologiesExternals] ||
         Head[es] === List && !chk[es],
       Message[GraphTopologies::externals, es, et];
       es = et];

    (* -- factors: masses and momenta flows -- *)

    mr = TopologyMasses[top, set];
    fs = TopologyMomentaFlows[top, set];

    (* flow and symbol patterns *)
    sp = Join @@ Position[First /@ (facs /. top), _?DenominatorSymbolQ];
    fp = Join @@ Position[fs, x_ /; x =!= 0];





    (* TODO -> Method: ~"Flow" "Symbol" *)
    (* neglect factors without momentum flow
    pk = # =!= 0 & /@ fs;
    {mr, fs} = Pick[#, pk] & /@ {mr, fs}; *)




    (* rewrite masses as line labels *)
    mrs = Cases[cs /. set, Rule[m[l_], s_] -> Rule[s, l]];
    (*mrs = {};*)
    mr = mr /. mrs /. m2r /. mrs;

    (* temporary propagators *)
    prs = MapThread[{#1, #2, 0, 0} & , {mr, fs}];

    (* their ordering *)
    ord = InversePermutation[Ordering[prs]];

    Report[
      vb, 2, "Factors with momenta flows: ",
      Length[fp], " of ", Length[fs], "."];

    Report[
      vb, 3, "Internal line labels and momenta flows:\n  ",
      StringJoin[Riffle[
        {ToString[#[[1]]], ": ", ToString[#[[2]]]} & /@ prs, "\n  "]]];

    (* -- legs: labels and momenta -- *)

    (* option: Externals *)
    If[Head[es] === String,
       lis = MapIndexed[in[First[#2]] -> #1 & , ps /. set];
       es = Switch[
         es,
         "Forward", Join[
           lis, lis /. in[i_] :> out[(np /. set) + i]],
         "In", Append[
           lis, out[(np /. set) + 1] -> Plus @@ (ps /. set)],
         "Out", Prepend[
           lis /. in[i_] :> out[1 + i],
           in[1] -> Plus @@ (ps /.set)]]];

    (* line labels *)
    els = Expand[Expand[#^2] /. (cs /. set)] /. mrs /. m2r /. mrs &
      /@ (Last /@ es);

    (* temporary external legs *)
    lgs = MapThread[
      {#1, #2,
       If[Head[#3] === in, #3, 0], If[Head[#3] === out, #3, 0]} & ,
      {els, Last /@ es, First /@ es}];

    (* do not count external legs *)
    lw += Length[lgs];

    Report[vb, 2, "External legs: ", Length[lgs], "."];

    Report[
      vb, 3, "External line labels and momenta flows:\n  ",
      StringJoin[Riffle[
        {ToString[#[[1]]], ": ", ToString[#[[2]]]} & /@ lgs, "\n  "]]];

    (* -- initial one-vertex graph -- *)

    (* helper: rank of propagator set <s> *)
    grk[s_] := MatrixRank[Outer[
      Coefficient[#1, #2] & , #[[2]] & /@ s, ks /. set]];

    (* sets of linearly independent propagators *)
    gis = Select[Subsets[prs, {nk /. set}], grk[#] === (nk /. set) & ];

    (* join legs *)
    gis = Join[lgs, #] & /@ gis;

    (* check: number of loops *)
    If[gis === {},
       Message[GraphTopologies::loops, name /. top, nk /. set]];

    Report[vb, 2, Length[gis], " initial graphs."];

    (* -- reconstruct graphs -- *)

    (* helper: internal vertices of graph <gr>; preserved order *)
    ivs[gr_] := DeleteCases[DeleteDuplicates[
      Join @@ (#[[{3, 4}]] & /@ gr)], (in | out)[_]];

    (* subroutine: candidate insertion <ci> in graph <gr> *)
    insert[ci_, gr_] := Module[
      {gs = gr, vs, vn, s,t, mn, cps},
      vs = ivs[gs];
      (* new vertex label *)
      vn = First[Complement[Range[Max[vs, Length[vs]] + 1], vs]];
      (* apply partitioning *)
      For[s = t = 1, s <= Length[gs], s++,
          If[gs[[s, 3]] === First[ci],
             If[FreeQ[Last[ci], t], gs[[s, 3]] = vn];
             t++];
          If[gs[[s, 4]] === First[ci],
             If[FreeQ[Last[ci], t], gs[[s, 4]] = vn];
             t++]];
      (* momentum of new propagator *)
      mn = Plus @@
        (If[#[[3]] === First[ci], #[[2]], 0] +
         If[#[[4]] === First[ci], -#[[2]], 0] & /@ gs);
      (* method: "1PI"s *)
      If[MemberQ[me, "1PI"],
         (* case: discard reducible graphs *)
         If[Intersection[Variables[mn], ks /. set] === {},
            Return[{}]]];
      (* candidate propagators; ensure momentum conservation *)
      cps = Select[prs, #[[2]] === mn || #[[2]] === -mn & ];
      (* method: "SE" *)
      cps = Fold[
        (* whether to generate self-energy insertions *)
        DeleteCases[#1, #2, {1}, If[MemberQ[me, "SE"], 1, Infinity]] & ,
        cps, {#[[1]], #[[2]], 0, 0} & /@ gs];
      (* case: nothing to insert *)
      If[cps === {},
         Return[{}]];
      (* insert vertex labels *)
      cps = Join[#[[{1, 2}]], If[
        #[[2]] === mn, {vn, First[ci]}, {First[ci], vn}]] & /@ cps;
      (* result: extended graphs *)
      Append[gs, #] & /@ cps];

    (* subroutine: grow graph <gr> by remaining insertions *)
    grow[gr_] := Module[
      {vd, ps, vs, cis},
      (* helper: vertex degree of <v> in propagator set <s> *)
      vd = Function[v, Plus @@ (Count[#[[{3, 4}]], v] & /@ gr)];
      (* helper: partition of rays for insertion <i> *)
      ps = Function[i, First[i] -> # & /@ Subsets[
        Range[Last[i]], {2, Last[i] - 2}]];
      vs = ivs[gr];
      (* candidate insertions: (vertices of higher degree) *)
      cis = Select[# -> vd[#] & /@ vs, Last[#] > 3 & ];
      (* case: nowhere to insert *)
      If[cis === {},
         Return[{}]];
      (* candidate insertions (ray partitions) *)
      cis = Join @@ (ps /@ cis);
      (* result: apply insertions *)
      Join @@ (insert[#, gr] & /@ cis)];

    (* subroutine: sort propagators and relabel vertices *)
    sort[gr_] := Module[
      {gs, vrs},
      (* sort by label and position *)
      gs = SortBy[gr, #[[{1, 2}]] & ];
      (* protect external and relabel internal vertices *)
      vrs = Join[
        {in[i_] -> in[i], out[o_] -> out[o]},
        MapIndexed[#1 -> First[#2] & , ivs[gs]]];
      (* apply relabeling *)
      Join[#[[{1, 2}]], #[[{3, 4}]] /. vrs] & /@ gs];

    (* final graphs *)
    gfs = {};

    (* initial sort *)
    gis = DeleteDuplicates[sort /@ gis];
    Report[vb, 3, "Sorted graphs: ", Length[gis], "."]

    (* check: graphs to extend or enough final graphs *)
    While[
      gis =!= {},

      Report[
        vb, 2, "Number of lines: ",
        Length[First[gis]] - Length[lgs], "."];

      (* select by line number *)
      gfs = Join[gfs, Select[
        gis, First[lw] <= Length[#] && Length[#] <= Last[lw] & ]];
      Report[vb, 3, "Final graphs: ", Length[gfs], "."];

      (* case: enough graphs *)
      If[n >= 0 && n <= Length[gfs],
         Break[]];

      (* case: out of levels *)
      If[gis =!= {} && Last[lw] === Length[First[gis]],
         Break[]];

      (* extend graphs by all possible insertions *)
      gis = Join @@ (grow /@ gis);
      Report[vb, 3, "Extended graphs: ", Length[gis], "."];



(* TODO: If[MemberQ[me, "Order"], ] *)
      (* discard identical graphs *)
      Report[vb, 3, "Sorting..."];
      gis = DeleteDuplicates[sort /@ gis];



(* TODO: If[MemberQ[me, "Filter"], ] *)
      Report[vb, 3, "Filtering..."];
      (*gis = DeleteDuplicates[gis, NickelIndex[{#[[3]] -> #[[4]], #[[1]]} & /@ #1] === NickelIndex[{#[[3]] -> #[[4]], #[[1]]} & /@ #2] & ];*)






      Report[vb, 3, "Sorted graphs: ", Length[gis], "."]];

    (* -- result -- *)

    (* cut number of graphs *)
    If[n >= 0 && n < Length[gfs],
       gfs = Take[gfs, n]];

    (* subroutine: split external and internal edges *)
    split[gr_] := Module[
      {slgs, sprs, cprs},
      slgs = Select[gr, MemberQ[#[[{3, 4}]], (in | out)[_]] & ];
      sprs = Select[gr, FreeQ[#[[{3, 4}]], (in | out)[_]] & ];
      (* complete by unused factors *)
      cprs = Fold[
        DeleteCases[#1, #2, {1}, 1] & ,
        prs, {#[[1]], #[[2]], 0, 0} & /@ sprs];
      sprs = Join[sprs, cprs];
      (* order by initial topology factors *)
      sprs = Sort[sprs][[ord]];
      {slgs, sprs}];

    gfs = split /@ gfs;

    (* subroutine: ordering function *)
    order[gr_] := Module[
      {ps, is,if, vs},
      ps = Join @@ Position[Last[gr], {_, _, 0, 0}];
      is = Intersection[ps, sp];
      if = Intersection[ps, fp];
      vs = Union[Part[#, {3, 4}] & /@ First[gr]];
      (* prefer: more lines, denominator symbols, momenta flows, external vertices *)
      {Length[pps], Length[is], Length[if], -Length[vs]}];

(* TODO: use SortTopologies[] *)

    gfs = SortBy[gfs, order];


(* TODO: If[MemberQ[me, "Check", ] *)



    (* result: integrate graphs into topology *)
    Topology[top, legs -> First[#], pros -> Last[#]] & /@ gfs];



(* TODO *)

(* overload: "Setup" *)
(*
GraphTopologies[top:TopologyPattern["Setup"], args___] /;
FreeQ[{args}, SetupPattern[]]:=
  GraphTopologies[top, setp /. top, args];
*)








(* trap *)
GraphTopologies[___] :=
  (Message[GraphTopologies::usage];
   Abort[]);



















(* needs: TopologyMasses[], TopologyMomentaFlows[] *)

(* N.B.:

- Assume for each external momentum an in-coming and an out-going leg.
- Initially all internal and external lines connect to a single vertex.

- Select for each loop momentum a propagator such that they form a
  linearly independent set.
- Try all such possibilities.

- Try to insert each propagator in all possible places, i.e.  vertices
  with degree larger than three, and check momentum conservation.
- Iterate until no more propagators or higher-degree vertices left.

*)

(* TODO:

- add dummy entries for irred. sps.

*)






GraphTopology::usage = GraphTopologies::usage;

(* shortcut: plural *)
GraphTopology[tops_?TopologyListQ, args___] :=
  First /@ (GraphTopologies[#, args] & /@ tops);

(* shortcut: singular *)
GraphTopology[top:TopologyPattern[], args___] :=
  First[GraphTopologies[top, args]];

(* trap *)
GraphTopology[___] :=
  (Message[GraphTopology::usage];
   Abort[]);

















(* --- TopologyPlot ... --------------------------------------------- *)

(* ---- helper: *)

(* N.B.:
- Returns interpolated coordinates and orientation between a set of
  points interconnected by straight lines.
- <cs>: list of real valued coordinates,
   <t>: real valued length parameter > 0, < 1.
*)

$TopologyPlotInterpolate[
  cs:{{__Real}..}, t_Real] /;
MatrixQ[cs] && Length[cs] > 1 && Length[cs[[1]]] > 0 &&
  t >= 0 && t <= 1 :=
  Module[
    {ls, u, i, o, d},
    (* calculate all distances *)
    ls = MapThread[Norm[#2 - #1] & , {Most[cs], Rest[cs]}];
    (* accumulate distances *)
    ls = FoldList[Plus, First[ls], Rest[ls]];
    (* rescale parameter *)
    u = t*Last[ls];
    (* find starting point *)
    i = 1 + LengthWhile[ls, # < u & ];
    (* offset from starting point *)
    o = u - If[i > 1, ls[[i - 1]], 0];
    (* normalized direction *)
    d = Normalize[cs[[i + 1]] - cs[[i]]];
    (* linear interpolation *)
    {cs[[i]] + o*d, d}];

(* ---- styles: *)

$TopologyStyle =
{"hb" -> ({{Black, Thick, Line[#1]}, {White, Line[#1]}} & ),
 "hp" -> {Black, Thick},
 "qt" | "Qt" -> {Black, Thick},
 Global`mh -> {Black, Thick},
 Global`mt -> {Black, Thick},
 0 -> {Black},
 Null -> {Black, Dotted},
 _ -> {Black}};

$TopologyStyle =
{"hb" -> (GraphicsGroup[{{Black, Thickness[1/20], Line[#1]}, {White, Thickness[1/60], Line[#1]}}] & ),
 "hp" -> (GraphicsGroup[{{Black, Thickness[1/20], Line[#1]}, {Lighter[Gray], Thickness[1/60], Line[#1]}}] & ),
 "qt" | "Qt" -> {Black, Thickness[1/30]},
 Global`mh -> (GraphicsGroup[{{Black, Thickness[1/20], Line[#1]}, {White, Thickness[1/60], Line[#1]}}] & ),
 Global`mt -> {Black, Thickness[1/30]},
 0 -> {Black, Thickness[1/60]},
 Null -> {Black, Dotted, Thickness[1/60]},
 _ -> {Black, Thickness[1/60]}};

$DiagramStyle =
{"gh" -> {Purple},
 "gl" -> {Red},
 "hb" -> {Black, Dashed, Thick},
 "hp" -> {Black, Thick},
 "qu" | "Qu" -> {Lighter[Blue]},
 "qd" | "Qd" -> {Darker[Blue]},
 "qt" | "Qt" -> {Blue, Thick},
 "si" -> {Gray, Dotted},
 Null -> {Black, Dotted, Thin},
 _ -> {Black}};

$IntegralStyle =
  $TopologyStyle;

(* ---- options: *)

Options[TopologyPlot] =
{

(* ----- TopologyToGraph, TopologyPlot *)

   Externals -> "Momenta",
   Internals -> "Positions",
   (* cf. $TopologyToGraphKeywords *)

   TopologyStyle -> $TopologyStyle,

(* ----- GraphPlot *)

   PlotLabel -> Automatic,  (* None *)
   FrameLabel -> None,
   (* Automatic, None, _ *)

   DirectedEdges -> False,
   (* False, True *)
   EdgeLabeling -> True,
   (* Automatic, False, True *)
   EdgeRenderingFunction -> Automatic,
   (* Automatic, None, _Function *)

   MultiedgeStyle -> 0.5,
   SelfLoopStyle -> 0.5,
   (* All, Automatic, None, _Reals *)

   VertexLabeling -> False,
   (* All, Automatic, False, True *)
   VertexRenderingFunction -> None,
   (* Automatic, None, _Function *)

   VertexCoordinateRules -> Automatic,
   DataRange -> Automatic,

   Method -> "SpringElectricalEmbedding",

   (* N.B.:
   - None other works w/ for VertexCoordinateRules.
   - All: "CircularEmbedding", "HighDimensionalEmbedding",
       "LinearEmbedding", "RandomEmbedding", "SpringEmbedding",
       "SpringElectricalEmbedding".
   *)

   PackingMethod -> "NestedGrid",

   (* N.B.:
   - Works best w/ VertexCoordinateRules.
   - All: "Layered", "LayeredTop", "LayeredLeft", "ClosestPacking",
       "ClosestPackingCenter", "NestedGrid"
   *)

(* ----- free *)

   LabelStyle -> {Background -> White},  (* {} *)
   RotateLabel -> True,

   Frame -> False,
   FrameStyle -> {},

   BaseStyle -> {PointSize[1], Thickness[1/5]},  (* {} *)
   FormatType :> TraditionalForm,
   PlotStyle -> Automatic,

   PlotRange -> All,
   PlotRangeClipping -> False,
   PlotRangePadding -> 0,  (* Automatic *)
   PlotRegion -> Automatic,

   AspectRatio -> Automatic,
   Background -> White,  (* None *)

   ImageMargins -> 0,
   ImagePadding -> All,
   ImageSize -> Small,  (* Automatic *)
   ImageSizeRaw -> Automatic,

(* ----- fixed *)

   Axes -> False,
   AxesLabel -> None,
   AxesOrigin -> Automatic,
   AxesStyle -> {},

   FrameTicks -> False,
   FrameTicksStyle -> {},

   GridLines -> None,
   GridLinesStyle -> {},

   Ticks -> False,
   TicksStyle -> {},

(* ----- unused *)

   AlignmentPoint -> Center,
   BaselinePosition -> Automatic,

   Prolog -> {},
   Epilog -> {},

   ColorOutput -> Automatic,
   DisplayFunction :> $DisplayFunction,

   ContentSelectable -> Automatic,
   CoordinatesToolOptions -> Automatic,
   PreserveImageOptions -> Automatic

   };

(* ---- main *)

TopologyPlot::usage = "\
TODO";



(* overload: plural *)
TopologyPlot[
  tops_?TopologyListQ, set:Elective[SetupPattern[]],
  opts:OptionsPattern[]] :=
  TopologyPlot[#, set, opts] & /@ tops;

















(* main *)
TopologyPlot[
  top:TopologyPattern["Graph"], opts:OptionsPattern[]] :=
  Module[
    {nm, ts,de,el,vl, gr,gs, is, cts,vts,lbs, erf,vrf, lis,los, lni,lno, lu,
     optr, vcr,vcs, ddd, of,

     bs,ls,
     dotf, crossf, labelf

   },

    nm = $CheckName[name /. top];

    (* -- options -- *)

    {bs, ls} = OptionValue[{BaseStyle, LabelStyle}];
    {bs, ls} = If[Head[#] === Rule, {#}, #] & /@ {bs, ls};

    {ts, de, el, vl} = OptionValue[
      {TopologyStyle, DirectedEdges, EdgeLabeling, VertexLabeling}];

    de = TrueQ[de];
    el = MemberQ[{Automatic, True}, el];
    vl = MemberQ[{All, Automatic, True}, vl];

    (* -- graph data -- *)

    gr = TopologyToGraph[
      top, Sequence @@ FilterRules[{opts}, Options[TopologyToGraph]]];

    gs = TopologyToGraph[
      top,
      Externals -> ({#[[6]], #[[2]]} & ),
      Internals -> ({#[[6]], #[[2]]} & )];

    (* enforce labeling *)
    gr = MapThread[If[
      Head[#1] === List, {First[#1], Join[#2[[2]], Rest[#1]]},  (* TODO: check if needed *)
      #2] & , {gr, gs}];

    is = inds /. top;

    cts = vts = lbs = {};

    (* -- pure numerators -- *)

    (* extract from graph *)
    gs = Cases[gr, {0 -> 0, _}];
    gr = Complement[gr, gs];

    (* symbolic notation *)
    gs = If[Length[#] > 3, Drop[#, 2], Last[#]] &
      /@ Select[Last /@ gs, Length[#] > 2 & ];

    If[gs =!= {},
       PrependTo[gr, {0 -> 0, {0, Null, gs}}]];

    (* sort by label positions in TopologyStyle to control overlap *)
    of = If[Length[#] > 1, Last[#], #] & @ Position[Function[t, MatchQ[Part[#, 2, 2], First[t]]] /@ ts, True] & ;  (* TODO: nicer *)
    gr = SortBy[gr, of];

    gr = Reverse[gr];

    lls = DeleteDuplicates[DeleteCases[DeleteCases[Join @@ List @@@ First /@ gr, (in | out)[_]], 0]];

    rrs = Prepend[MapIndexed[#1 -> First[#2] & , lls], e:(in | out)[_] -> e];

    gr = Prepend[Rest[#], First[#] /. rrs] & /@ gr;

    (*Print/@gr;*)

    (*gr = Reverse[gr];*)

    (* -- edge rendering function -- *)


    (* TODO: make $-defines *)

    dotf = Inset[
      Graphics[{Sequence @@ bs, Point[{0, 0}]}],
      #[[1]], {0, 0}, Scaled[1/10], #[[2]]] & ;

    crossf = Inset[
      Graphics[{Sequence @@ bs, Line[{{-1, -1}, {1, 1}}], Line[{{-1, 1}, {1, -1}}]}],
      #[[1]], {0, 0}, Scaled[1/10], #[[2]]] & ;

    labelf = {Sequence @@ ls, Inset[#2, #1]} & ;

    vcr = {};

    erf[cs_, vs_, lb_] := Module[
      {ge, st, gl, ds},

      vcr = Join[vcr, {First[vs] -> First[cs], Last[vs] -> Last[cs]}];

      (* find matching edge *)
      ge = Select[gr, vs === List @@ #[[1]] & , 1][[1]];
      (* remove from list *)
      gr = DeleteCases[gr, ge, {1}, 1];
      (* styling options *)
      st = ge[[2, 2]] /. ts;

      (* option: edge labelling *)
      If[el && Length[ge[[2]]] > 2,
         gl = If[Length[#] > 1, #, #[[1]]] & @ Drop[ge[[2]], 2];
         If[is =!= inds && FreeQ[vs, (in | out)[_]],
            If[Head[gl] === Integer,
               (* dots, crosses *)
               If[gl > 0, gl--];
               ds = $TopologyPlotInterpolate[cs, N[#]] &
                 /@ (Range[Abs[gl]]/(Abs[gl] + 1));


               (* BaseStyle *)
               ds = If[gl > 0, dotf, crossf] /@ ds;



               vts = Join[vts, ds]];
            If[MatchQ[gl, {_Integer, _Integer}],
               {gp, gl} = {First[gl], Last[gl]};
               (* dots, crosses *)
               If[gl > 0, gl--];
               ds = $TopologyPlotInterpolate[cs, N[#]] &
                 /@ (Range[Abs[gl] + 1]/(Abs[gl] + 2));
               (* labels *)
               AppendTo[lbs, labelf[Part[ds, 1, 1], gp]];
               ds = Rest[ds];


               (* BaseStyle *)
               ds = If[gl > 0, dotf, crossf] /@ ds;


               vts = Join[vts, ds]],
            AppendTo[lbs, labelf[If[vs === {0, 0}, First[cs], $TopologyPlotInterpolate[cs, 0.5][[1]]], gl]]]];

      (* case: scalar products *)
      If[vs === {0, 0},
         Return[{}]];
      (* check: function *)
      If[Head[st] === Function,
         Return[st[cs, vs, lb]]];
      (* check: no list *)
      If[Head[st] =!= List,
         st = {st}];
      (* option: directed edges *)
      Join[st, If[
        de,
        (* directed *)
        Prepend[{Arrow[cs]}, If[
          MemberQ[vs, (in | out)[_]],
          (* external *)
          Arrowheads[{0.08, #} & /@ (
                                     0.5 - (0.1*(# - 1))/2 + 0.1*Range[0, # - 1] & @
                                     First[Cases[vs, (in | out)[x_] -> x]])],
          (* internal *)
          Arrowheads[{{Medium, 4/5}}]]],
        (* not directed *)
        {Line[cs]}]]];
    (* TODO: switch for arrows only on external or internal edges *)


    (* -- vertex rendering function -- *)

    vrf[cs_, v_] := Module[
      {},
      If[v === 0, Return[{}]];
      AppendTo[vts, If[
        vl,
        Inset[Framed[v], cs],
        If[FreeQ[{in, out}, Head[v]],
           {Black, PointSize[Large], Point[cs]},
           {}]]];
      {}];

    (* -- vertex coordinate rules -- *)

    (* in-coming, out-going *)
    lis = Cases[Join @@ (List @@ #[[1]] & /@ gr), in[_]];
    los = Cases[Join @@ (List @@ #[[1]] & /@ gr), out[_]];

    (* lengths, maximum *)
    {lni, lno} = Length /@ {lis, los};
    lu = Max[lni, lno]/2;

    vcs = Join[
      MapIndexed[
        #1 -> {-lu, lu*If[
          lni === 1, 0, 1 - 2*(#2[[1]] - 1)/(lni - 1)]} & , Sort[lis]],
      MapIndexed[
        #1 -> {lu, lu*If[
          lno === 1, 0, 1 - 2*(#2[[1]] - 1)/(lno - 1)]} & , Sort[los]],
      {0 -> {0, -1}}];

    (* -- options processing -- *)

    optr = Join @@ First[Last[Reap[

      If[OptionValue[PlotLabel] === Automatic,
         Sow[{PlotLabel -> name /. top}]];

      If[OptionValue[FrameLabel] === Automatic, Sow[
        {Frame -> True,
         FrameLabel -> name /. top,
         PlotRangePadding -> Automatic}]];

      Switch[
        OptionValue[EdgeRenderingFunction],
        Automatic, Sow[{EdgeRenderingFunction -> erf}],
        None, Sow[{EdgeRenderingFunction -> Automatic}]];

      If[vl || OptionValue[VertexRenderingFunction] === Automatic,
         Sow[{VertexRenderingFunction -> vrf}]];

      Switch[
        OptionValue[VertexCoordinateRules],
        Automatic, Sow[
          {DataRange -> {{-lu, lu}, {-lu, lu}},
           VertexCoordinateRules ->
             If[Head[$TopologyPlotVertexCoordinateRules[nm]] === List,
                $TopologyPlotVertexCoordinateRules[nm],
                vcs]}],
        _List, Sow[
          {DataRange -> {{-lu, lu}, {-lu, lu}},                         (* Automatic? *)
           VertexCoordinateRules ->
             Join[OptionValue[VertexCoordinateRules], vcs]}]]

    ]]];

    (* -- result -- *)

    (* add options, filter options and defaults *)
    gp = GraphPlot[First /@ gr, Sequence @@ FilterRules[
      $AppendOptions[optr, opts, TopologyPlot], Options[GraphPlot]]];

    (* clean generated vertex coordinate rules *)
    vcr = DeleteDuplicates[vcr, First[#1] === First[#2] & ];  (* TODO: necessary still? *)

    vcr = SortBy[vcr, First[#] & ];

    (* save in global lookup table *)
    $TopologyPlotVertexCoordinateRules[nm] = vcr;

    (* combine *)
    Graphics[
      {First[gp], vts, lbs}, Sequence @@ Rest[gp],
      Sequence @@ FilterRules[
        $AppendOptions[optr, opts, TopologyPlot], Options[Graphics]]]];



(*
TODO: Plot[<expr>]: <expr> /. {BT[...] -> -Graphics-}
*)










(* trigger: no "Graph" -> TopologyGraphs[] *)
TopologyPlot[
  top:TopologyPattern[], set:SetupPattern[],
    opts:OptionsPattern[]] :=
  TopologyPlot[
    First[Quiet[
      GraphTopologies[top, set, All, -1, Sequence @@ FilterRules[
        {opts}, Options[GraphTopologies]]],
      {GraphTopologies::externals, TopoID::method}]],
    Sequence @@ FilterRules[
      DeleteCases[{opts}, HoldPattern[Method -> _]],
      Options[TopologyPlot]]];









(* TODO: is this correct? *)
TopologyPlot[top:TopologyPattern["Setup"], opts:OptionsPattern[]] :=
  TopologyPlot[top, setp /. top, opts];




(* trap *)
TopologyPlot[___] :=
  (Message[TopologyPlot::usage];
   Abort[]);



(* ################################### *)




(* TODO: $AppendToOptions[], ... *)

Options[DiagramPlot] =
  Options[TopologyPlot];

(* shortcut: DiagramPlot *)
DiagramPlot[args___] :=
  TopologyPlot[
    args,
    Externals -> "Labels", Internals -> "Labels",
    TopologyStyle -> $DiagramStyle,
    DirectedEdges -> True];



(* ############################### *)




IntegralPlot::usage = "\
TODO";

$AppendToOptions[
  IntegralPlot,
  {Externals -> "Momenta",
   Internals -> "Indices",
   TopologyStyle -> $IntegralStyle},
  TopologyPlot];

(* main, overload: <x>, <tops> -> <maps>, <tops> *)
IntegralPlot[
  x_, ks___:{}, tops_?TopologyListQ, set:Elective[SetupPattern[]],
  opts:OptionsPattern[]] :=
  Module[
    {mps, pls, rls},
    mps = ToMapping[{x}, ks];
    rls = Reverse /@ MappingToRule[mps];
    pls = IntegralPlot[mps, tops, set, opts];
    rls = MapThread[First[#1] -> #2 & , {rls, pls}];
    x /. rls];

(* main, alias: <int> -> TopologyPlot[] *)
IntegralPlot[
  int:IntegralPattern[], set:Elective[SetupPattern[]],
    opts:OptionsPattern[]] :=
  TopologyPlot[int, set, Sequence @@ FilterRules[
    $AppendOptions[opts, IntegralPlot], Options[TopologyPlot]]];

(* overload: _, <top> -> _, <tops> *)
IntegralPlot[
  maps_, top:TopologyPattern[], set:Elective[SetupPattern[]],
  opts:OptionsPattern[]] :=
  IntegralPlot[maps, {top}, set, opts];












(* main, overload: <map>, <tops> *)
IntegralPlot[
  map:MappingPattern[], tops_?TopologyListQ, set:Elective[SetupPattern[]],
    opts:OptionsPattern[]] :=
  Module[


    {pl,vc, mapr, imap, int, rmap, is, f,          vcr, optr},
    {imap, int} = MapTopologyToIntegral[map, tops];

    {pl, vc} = # /. $FlagRules & /@ OptionValue[{PlotLabel, VertexCoordinateRules}];  (* TODO: leave like this? *)

    If[imap === {},
       (* TODO: message *)
       Return[$Failed]];

    rmap = ReverseMapping[imap];

    mapr = MappingToRule[map];

    vcr = $TopologyPlotVertexCoordinateRules[Head[Last[mapr]]];

(* TODO: work with mapping routines *)
    is = Part[id /. map, id /. rmap];
    f = Part[Transpose[{id /. rmap, is}], Part[#, 6]] & ;
(*
    Print[imap];

    Print[];

    Print /@ (pros /. tops[[1]]);

    Print[];

    Print /@ (pros /. int);
*)

    optr = Join @@ First[Last[Reap[

      If[pl,
         Sow[{PlotLabel -> Last[mapr]}]];

      If[OptionValue[Internals] === "Internals",
         Sow[{Internals -> f}]];


      If[vc && Head[vcr] === List,

         (* TODO: contract *) Null;

         Sow[{VertexCoordinateRules -> vcr}]];


    ]]];

    IntegralPlot[int, set, Sequence @@ $AppendOptions[
      optr, opts, IntegralPlot]]];







(* overload: <maps>, <tops> -> <map>, <tops> *)
IntegralPlot[
  maps_?MappingListQ, tops_?TopologyListQ, set:Elective[SetupPattern[]],
  opts:OptionsPattern[]] :=
  IntegralPlot[#, tops, set, opts] & /@ maps;

(* overload: <ints> -> <int> *)
IntegralPlot[
  ints_?IntegralListQ, set:Elective[SetupPattern[]],
  opts:OptionsPattern[]] :=
  IntegralPlot[#, set, opts] & /@ ints;












(* TODO:
Show[Graphics[{Text[IntegralPlot[<x>, <tops>, <set>]]}]]
*)

(* trap *)
IntegralPlot[___] :=
  (Message[IntegralPlot::usage];
   Abort[]);

(* --- TopologySlider ... ------------------------------------------- *)

Options[TopologySlider] =
  Options[TopologyPlot];

TopologySlider::usage = "\
TODO";

(* main *)
TopologySlider[
  tops_?TopologyListQ, opts:OptionsPattern[]] :=
  DynamicModule[
    {i},
    Manipulate[TopologyPlot[tops[[i]], opts], {i, 1, Length[tops], 1}]];

(* trap *)
TopologySlider[___] :=
  (Message[TopologySlider::usage];
   Abort[]);

Options[DiagramSlider] =
  Options[TopologySlider];

(* shortcut: DiagramSlider *)
DiagramSlider[args___] :=
  TopologySlider[
    args,
    Externals -> "Labels", Internals -> "Labels",
    TopologyStyle -> $DiagramStyle,
    DirectedEdges -> True];

Options[IntegralSlider] =
  Options[TopologySlider];

(* shortcut: IntegralSlider *)
IntegralSlider[args___] :=
  TopologySlider[args, Internals -> "Indices"];








(* --- TopologyGrid ... --------------------------------------------- *)

Options[TopologyGrid] =
  Options[TopologyPlot];

TopologyGrid::usage = "\
TODO";

(* main *)
TopologyGrid[
  tops_, w_Integer:5, opts:OptionsPattern[]] :=
  GraphicsGrid[Partition[
    If[Head[#] === List, #, {#}] & @
      TopologyPlot[tops, opts], w, w, 1, {}]];

(* trap *)
TopologyGrid[___] :=
  (Message[TopologyGrid::usage];
   Abort[]);



Options[DiagramGrid] =
  Options[TopologyGrid];

(* shortcut: DiagramGrid *)
DiagramGrid[args___] :=
  TopologyGrid[
    args,
    Externals -> "Labels", Internals -> "Labels",
    TopologyStyle -> $DiagramStyle,
    DirectedEdges -> True];




Options[IntegralGrid] =
  Options[TopologyGrid];

(* main *)
IntegralGrid[
  ints__, w_Integer:5, opts:OptionsPattern[]] :=
  GraphicsGrid[Partition[
    If[Head[#] === List, #, {#}] & @
      IntegralPlot[ints, opts], w, w, 1, {}]];   (* TODO: options -> GraphicsGrid *)




TopologyShow[args___] :=
  Show[TopologyGrid[args]];

DiagramShow[args___] :=
  Show[DiagramGrid[args]];

IntegralShow[args___] :=
  Show[IntegralGrid[args]];




(* --- TopologyManipulate ------------------------------------------- *)

$TopologyManipulateGridLines =
  20;

Options[TopologyManipulate] =
{ImageSize -> Large,
 GridLines -> Automatic,
 GridLinesStyle -> Directive[Lighter[Gray, 0.8], Thin]};

$AppendToOptions[TopologyManipulate, TopologyPlot];

TopologyManipulate[
  top:TopologyPattern["Graph"], opts:OptionsPattern[]] :=
  Module[
    {f, nm, gl, plt, vcs,vvs, gls,gd, r},

    nm = $CheckName[name /. top];

    gl = OptionValue[GridLines];
    If[Head[gl] =!= Integer,
       gl = gl /. $FlagRules /. True -> $TopologyManipulateGridLines];

    plt = TopologyPlot[top, Sequence @@ FilterRules[
      $AppendOptions[opts, TopologyManipulate], Options[TopologyPlot]]];

    vcs = $TopologyPlotVertexCoordinateRules[nm];
    vvs = MapIndexed[Symbol["vv" <> ToString[First[#2]]] & , vcs];

    If[Head[gl] === Integer,
       gls = {Min[#], Max[#]} & /@ Transpose[Last /@ vcs];
       gd = Min[Abs[Last[#] - First[#]] & /@ gls]/gl;
       gl = Range @@ Append[#, gd] & /@ gls;
       vcs = First[#] -> Round[Last[#], gd] & /@ vcs];

    (* DynamicModule[] *)
    r = f[1][
      MapThread[f[2], {vvs, Last /@ vcs}],
      (* Graphics[] *)
      f[3][
        (* TopologyPlot[] *)
        {f[4][f[5][f[6][top, Sequence @@ FilterRules[
          $AppendOptions[
            {VertexCoordinateRules ->
               MapThread[First[#1] -> #2 & , {vcs, vvs}]},
            opts, TopologyManipulate], Options[TopologyPlot]]]]],
         (* Locator[] *)
         Function[x, f[7][f[4][x, f[8][f[2][
           x, If[NumberQ[gd], f[9][f[10][1], gd], f[10][1]]]]]]] /@ vvs},
        (* options *)
        Sequence @@ FilterRules[
          $AppendOptions[{GridLines -> gl}, opts, TopologyManipulate],
          Options[Graphics]]]];

    r /.
    {f[1] -> DynamicModule, f[2] -> Set, f[3] -> Graphics,
     f[4] -> Dynamic, f[5] -> First, f[6] -> TopologyPlot,
     f[7] -> Locator, f[8] -> Function, f[9] -> Round, f[10] -> Slot}];

(* --- package end -------------------------------------------------- *)

Protect["TopoID`Graph`*"];

Scan[
  SetAttributes[#, {ReadProtected}] & ,
  Select[Symbol /@ Names["TopoID`Graph`*"], Head[#] === Symbol & ]];

Unprotect[
  $TopologyPlotVertexCoordinateRules];

ClearAttributes[
  {$TopologyPlotVertexCoordinateRules},
  ReadProtected];

End[];

EndPackage[];

(* ------------------------------------------------------------------ *)



(* --- TODO:

- Check <ts>, append _ -> {Black}.

- Draw cuts (-> cts).

- MappingPlot[].

- Add Takahiro's vertex coordinate rules? Cf. "Graph-TU.m".

- Manip. -> add grid


*)
