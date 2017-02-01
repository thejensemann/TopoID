(* --- "Core.m": Main mapping Routines ------------------------------ *)

(* --- provided functions:

TODO

*)

(* --- package begin ------------------------------------------------ *)

Unprotect["TopoID`Core`*"];

ClearAll["TopoID`Core`*", "TopoID`Core`Private`*"];

BeginPackage[
  "TopoID`Core`",
  {"TopoID`Common`",
   "TopoID`System`", (*"TopoID`Graph`",*)  (* TODO *)

   "TopoID`Setup`",
   "TopoID`Topology`",
   "TopoID`Mapping`",

   "TopoID`Polynomial`",
   "TopoID`Cuts`",

   "TopoID`Object`"}];

{$MapDiagramToTopologyMethods, MapDiagramToTopology,
 $MapTopologyToTopologyMethods, MapTopologyToTopology, MinimizeTopologies,
 $MapTopologyToIndependentsMethods, MapTopologyToIndependents,

 CutTopologies, MapTopologyToCuts,

 $MapTopologyToFactorsMethods, MapTopologyToFactors,
 MapTopologyToIntegral,
 MapIntegralToIntegral, MinimizeIntegrals,
 $MapIntegralToTopologyMethods, MapIntegralToTopology};

Begin["`Private`"];

(* --- MapDiagramToTopology ----------------------------------------- *)

(* MapDiaTop[<dias>, <stp>, [opts]]
arg.s:
  <dias> -- list of diagrams producted by GetDias and
  <stp> -- list produced by TopoIDInit containing the kinematic setup
    and constraints of the problem.
opt.s:
  [Verbosity] -- level of additional output (cf. VerbosityRules) and

dep.s:
  Report, ErrorFunc, VerbosityRules.
  UF, ScalefulQ, PolyOrdering, PermuteTable.
sym.s:

output:
  output: {<diamaps>, <diatops>}

  diamaps -- mapping relations between diagrams and topologies, e.g.
  {fr -> "D1", to -> "DT1", id -> {1, 1, 3, 0}} <-- 0 for constants
  or
  {fr -> "D1", to -> 0, id -> {}} <-- for scaleless diagrams

  diatops -- topologies in canonical ordering, e.g.
  {name -> "DT1",
   dens -> {(v1+p0)^2, v1^2 + m^2, (v1-2*p0)^2},
   xpar -> {x[1], x[2], x[3]},s
   arep -> {-x[1]-x[2]-x[3], -m^2*x[1]*x[2]-m^2*x[2]^2-m^2*x[2]*x[3]}}
return:
desc.:
  extract canonic scalar topologies
notes:
- Euclidean/Minkowski-space invariant convention is used for the
  propagators (i.e. ~ 1/(m^2 - p^2) and hence no minus sign emerges).
*)

(* all but last digits *)
$MapDiagramToTopologyInherit =
  StringReplace[#, RegularExpression["\\D*(\\d*)$"] -> "$1"] & ;

NamingRules[MapDiagramToTopology] =
{Inherit ->
   ("DT" <> $MapDiagramToTopologyInherit[#] & ),
 Inherit[s_String] ->
   (s <> $MapDiagramToTopologyInherit[#] & ),

 Iterate ->
   ("DT" <> ToString[#2] & ),
 Iterate[s_String] ->
   (s <> ToString[#2] & ),
 Iterate[i_Integer] ->
   ("DT" <> ToString[i + #2] & ),
 Iterate[i_Integer, s_String] | Iterate[s_String, i_Integer] ->
   (s <> ToString[i + #2] & ),

 i_Integer ->
   (ToString[i] & ),
 s_Symbol ->
   (ToString[s] & ),
 s_String ->
   (s & ),

 l_List ->
   (If[#2 <= Length[l], l[[#2]],
       Message[NamingRules::list, l]; Abort[]] & ),

 f_Function :>
   If[Head[f["", 1]] === String, f,
      Message[NamingRules::function]; Abort[]],

 x_ :>
   (Message[NamingRules::keys, x, $NamingRulesKeys]; Abort[])};

ClearAll[$MapDiagramToTopologyMethods];

$MapDiagramToTopologyMethods =
{"Euclid", "Euclidean",       (* -> m^2 + p^2 *)
 "Minkowski", "Minkowskian",  (* -> m^2 - p^2 *)
 "Keep"};                     (* identical propagators *)

Options[MapDiagramToTopology] =
{Method -> {"Euclidean"},  (* cf. $MapDiagramToTopologyMethods *)
 Naming -> Inherit,        (* cf. NamingRules *)
 Parallel -> False,        (* cf. $FlagRules *)
 Status -> True,           (* cf. $FlagRules *)
 Verbosity -> False};      (* cf. VerbosityRules *)

MapDiagramToTopology::usage = "\
MapDiagramToTopology[<dia(s)>, <set>, [opts]] returns {<map{s}>, \
<top(s)>}.  For each diagram object in <dia(s)> a topology object is \
created in <top(s)> and returned together with a corresponding mapping \
object in <map(s)>.  This routine constructs from line masses and \
momenta propagators and applies the canonic ordering.  The used metric \
and the treatment of identical propagators can be controlled by the \
option [Method].";

MapDiagramToTopology[
  dias:{DiagramPattern[]..}, set:SetupPattern[],
    opts:OptionsPattern[]] :=
  Module[
    {ng,pl,st, nms, tmp, nm = "", f, maps,tops},

    (* -- options -- *)

    {ng, pl, st} = OptionValue[{Naming, Parallel, Status}];

    ng = ng /. NamingRules[MapDiagramToTopology];
    {pl, st} = TrueQ[# /. $FlagRules] & /@ {pl, st};

    (* -- main -- *)

    If[st, Status[
      All,
      "Map ", Length[dias], " diagrams to topologies.",
      Temporary -> False]];

    (* generate names *)
    nms = MapIndexed[ng[#1, #2[[1]]] &, name /. dias];

    tmp = Transpose[{nms, dias}];

    If[pl, SetSharedVariable[nm]];
    Status[FrontEnd, "-> Creating \"", nm, "\"...", Temporary -> True];  (* TODO: total *)

    (* helper: singular *)
    f = Function[
      nm = #[[1]];
      If[st && !pl, Status[
        Console, "-> Creating \"", nm, "\"...", Temporary -> True]];
      Quiet[MapDiagramToTopology[#[[2]], set, Naming -> #[[1]], opts]]];

    tmp = If[pl, ParallelMap, Map][f, tmp];

    (* -- result -- *)

    {maps, tops} = Transpose[tmp];

    (* clean from zero topologies *)
    tops = DeleteCases[tops, {}];

    If[st, Status[
      All, "Created ", Evaluate @ Length[maps], " mappings and ",
      Evaluate @ Length[tops], " topologies.", Temporary -> False]];

    {maps, tops}];

MapDiagramToTopology[
  dia:DiagramPattern[], set:SetupPattern[],
    opts:OptionsPattern[]] :=
  Module[
    {f, me,ng, cp, s, nm, msk, top, fs,ft, mct, map},

    f = Expand[#] //. (cs /. set) & ;

    (* -- options -- *)

    {me, ng} = OptionValue[{Method, Naming}];
    me = Flatten[{me}];
    ng = ng /. NamingRules[MapDiagramToTopology];

    (* check: method *)
    cp = Complement[me, $MapDiagramToTopologyMethods];
    If[cp =!= {},
       Message[Method::invalid, cp, $MapDiagramToTopologyMethods];
       me = Method /. Options[MapDiagramToTopology]];

    (* sign of metric *)
    s = If[MemberQ[{"Minkowski", "Minkowskian"}, me], -1, +1];

    (* create name *)
    nm = ng[name /. dia, 1];

    (* -- contract dummy lines -- *)

    msk = MapIndexed[
      If[(m[#[[1]]] //. (cs /. set)) === Null, 0, #2[[1]]] & ,
      pros /. dia];

    msk = Mapping[name /. dia, name /. dia, msk];

    top = GraphMapTopology[msk, dia];

    (* -- construct factors -- *)

    (* mass, sign, momentum, constraints *)
    fs = FixedPoint[f, m[#[[1]]]^2 + s*#[[2]]^2] & /@ (pros /. top);

    (*fs = Expand[m[#[[1]]]^2 + s*#[[2]]^2] //. (cs /. set) &
      /@ (pros /. top);*)

    (* method: duplicates or unique *)
    ft = If[FreeQ[me, "Keep"], Union[fs], Sort[fs]];

    (* non-constant *)
    ft = Select[ft, Intersection[Variables[#], ks /. set] =!= {} & ];

    (* establish notation *)
    top = Topology[top, facs -> MapIndexed[p @ #2[[1]] -> #1 & , fs]];

    (* -- mapping scheme -- *)

    (* old -> new, set constants to zero *)
    If[FreeQ[me, "Keep"],
       (* identify identical propagators *)
       mct = Flatten[Position[ft, #, 1, 1] & /@ fs /. {} -> 0];
       mct = Mapping[name /. dia, nm, mct],
       (* leave identical propagators *)
       mct = Flatten[DeleteDuplicates[Position[fs, #, 1] & /@ ft]];
       mct = ReverseMapping[Mapping[nm, name /. dia, mct], Length[fs]]
     ];

    (* -- topology -- *)

    (* apply mapping *)
    top = MapToTopology[mct, top];

    (* canonicalize *)
    {map, top} = CanonicalizeTopology[top, set];

    (* check: scaleful *)
    If[!TopologyScalefulQ[top],
       Return[{Mapping[name /. dia, 0, {}], {}}]];

    (* -- compose result -- *)

    (* mapping *)
    map = ComposeMapping[mct, map];

    (* topology *)
    top = Topology[
      top, hist -> {"MapDiagramToTopology", name /. dia, me}];

    {map, top}];

(* overload: "Setup" *)
MapDiagramToTopology[
  dia:DiagramPattern["Setup"],
    opts:OptionsPattern[]] :=
  MapDiagramToTopology[dia, setp /. dia, opts];

(* trap *)
MapDiagramToTopology[___] :=
  (Message[MapDiagramToTopology::usage];
   Abort[]);

(* TODO:
- samples?
- Beware: "Setup" overloads
- description/dependencies

- Verbosity
*)

(* --- MapTopologyToTopology ---------------------------------------- *)

$MapTopologyToTopologyMethods =
{"Mask", "Reorder"};

Options[MapTopologyToTopology] =
{Method -> {"Mask", "Reorder"}};
(* cf. $MapTopologyToTopologyMethods *)

MapTopologyToTopology::usage = "\
TODO";

MapTopologyToTopology::nomatch = "\
Warning: No match for topology factors of \"`1`\" in \"`2`\".";

MapTopologyToTopology::multiple = "\
Warning: Multiple matches for topology factors of \"`1`\" in \"`2`\" \
for different (sub-)topologies `3`.";

MapTopologyToTopology[
  src:TopologyPattern["Alpha"], trg:TopologyPattern["Alpha", "Subt"],
    n_Integer:-1, opts:OptionsPattern[]] :=
  Module[
    {me,cp, fs,ft, ps,rs, pt,rt, sel, sort, mask, cut, maps},

    (* check: Method *)
    me = Flatten[{OptionValue[Method]}];
    cp = Complement[me, $MapTopologyToTopologyMethods];
    If[cp =!= {},
       Message[Method::invalid, cp, $MapTopologyToTopologyMethods];
       me = Intersection[me, $MapTopologyToTopologyMethods]];

    (* factor symbols *)
    fs = First /@ (facs /. src);
    ft = First /@ (facs /. trg);

    (* alpha-representations *)
    {ps, rs} = {apar, arep} /. src;
    {pt, rt} = {apar, arep} /. trg;

Print["in"];

    rs = rs /. MapIndexed[#1 -> pt[[First[#2]]] & , ps];


    (* -- find matches -- *)

    (* select candidates *)
    sel = Select[subt /. trg, Length[#[[1]]] === Length[ps] & ];

    (* identical sub-topologies *)
    sel = Select[
      sel, Expand[rs - (rt /. PermuteRules[#[[1]], pt])] === {0, 0} & ];

    (* check: no match *)
    If[sel === {},
       Message[MapTopologyToTopology::nomatch,
         name /. src, name /. trg];
       Return[{}]];

    (* check: multiple matches *)
    If[Length[sel] > 1,
       Message[MapTopologyToTopology::multiple,
         name /. src, name /. trg, sel]];

    sel = Join @@ sel;

    (* -- reorder -- *)

    (* helper *)
    sort = Function[
      ss, ss[[#]] & /@ PolynomialOrderings[$TopoIDMetric @@ rs, ps]];

    (* option *)
    If[MemberQ[me, "Reorder"],
       sel = Join @@ (sort /@ sel)];

    (* -- masking -- *)

    (* helper *)
    mask = Function[ss, And @@ Not /@ MapThread[
      DenominatorSymbolQ[#1] && NumeratorSymbolQ[#2] & ,
      {fs, ft[[ss]]}]];

    (* helper: cuts *)
    cut[map_] := Module[
      {cs,ct},
      {cs, ct} = cuts /. {src, trg};
      Switch[
        {cs, ct},
        (* no cuts *)
        {cuts, cuts},
        True,
        (* source cuts *)
        {_, cuts},
        Or @@ (Intersection[Range[Length[map]], #] === # & /@ cs),  (* ??? leave ??? or just "True" ??? *)
        (* target cuts *)
        {cuts, _},
        Or @@ (Intersection[map, #] === # & /@ ct),
        (* both cuts *)
        {_, _},
        Or @@ Join @@ Outer[Complement[map[[#1]], #2] === {} & , cs, ct, 1]
      ]];

    (* option *)
    If[MemberQ[me, "Mask"],
       sel = Select[sel, mask];
       If[(cuts /. trg) =!= cuts,
          sel = Select[sel, cut]]];

    (* -- result -- *)

    (* create mappings *)
    maps = Mapping[name /. src, name /. trg, #] & /@ sel;

    (* demanded number *)
    If[n >= 0 && n < Length[maps], Take[maps, n], maps]];
(*
(* overload: plural, plural *)
MapTopologyToTopology[
  srcs_?TopologyListQ, trgs_?TopologyListQ, n_Integer:-1,
  opts:OptionsPattern[]] :=
  Union[DeleteCases[
    Join @@
      ((Status[Console, name /. #];
        MapTopologyToTopology[srcs, #, n, opts]) & /@ trgs), {}]];

(* overload: plural, singular *)
MapTopologyToTopology[
  srcs_?TopologyListQ, trg:TopologyPattern[], n_Integer:-1,
  opts:OptionsPattern[]] :=
  Union[DeleteCases[
    Join @@
      ((Status[Console, name /. #];
        MapTopologyToTopology[#, trg, n, opts]) & /@ srcs), {}]];

(* overload: singular, plural *)
MapTopologyToTopology[
  src:TopologyPattern[], trgs_?TopologyListQ, n_Integer:-1,
    opts:OptionsPattern[]] :=
  Union[DeleteCases[
    Join @@
      ((Status[Console, name /. #];
        MapTopologyToTopology[src, #, n, opts]) & /@ trgs), {}]];

(* trigger: InspectTopology[] *)
MapTopologyToTopology[
  src:TopologyPattern["Alpha"], trg:TopologyPattern["Alpha"],
    n_Integer:-1, opts:OptionsPattern[]] /;
!TopologyQ[trg, "Subt"] :=
  MapTopologyToTopology[
    src, InspectTopology[
      trg, Sequence @@ FilterRules[{opts}, Options[InspectTopology]]],
    n, opts];
*)
(* trap *)
MapTopologyToTopology[___] :=
  (Message[MapTopologyToTopology::usage];
   Abort[]);

Off[MapTopologyToTopology::nomatch];

(* TODO:
- pass options!
- Parallel?
- n in overloads
*)

(* --- MinimizeTopologies ------------------------------------------- *)

(*

- maps topologies in a list to each other

- output: {<topmaps>, <gentops>}

- calls:
  SubTops
  PermuteTable

  topmaps -- mapping of topologies in diatops on gentops, e.g.
  {fr -> "DT1", to -> "GT4", id -> {1, 3, 2}}

  gentops -- generic topologies, e.g.
  {name -> "GT1",
   dens -> {(v1+p0)^2, v1^2 + m^2, (v1-2*p0)^2},
   xpar -> {x[1], x[2], x[3]},
   arep -> {-x[1]-x[2]-x[3], -m^2*x[1]*x[2]-m^2*x[2]^2-m^2*x[2]*x[3]},
   zero -> {{1}, {2}},
   subt -> {
     {{1, 2, 3}},
     {{1, 3}, {1, 2}, {2, 3}},
     {{2}}}}

- "representatives of all unique non-zero subsets with matching no. of
  lines"

- todo: speedup by filering for propagator masses

*)

(* all but first letters *)
$MinimizeTopologiesInherit =
  StringReplace[#, RegularExpression["^\\D*(.d*)"] -> "$1"] & ;

NamingRules[MinimizeTopologies] =
{Inherit ->
   ("GT" <> $MinimizeTopologiesInherit[#] & ),
 Inherit[s_String] ->
   (s <> $MinimizeTopologiesInherit[#] & ),

 Iterate ->
   ("GT" <> ToString[#2] & ),
 Iterate[s_String] ->
   (s <> ToString[#2] & ),
 Iterate[i_Integer] ->
   ("GT" <> ToString[i + #2] & ),
 Iterate[i_Integer, s_String] | Iterate[s_String, i_Integer] ->
   (s <> ToString[i + #2] & ),

 i_Integer ->
   (ToString[i] & ),
 s_Symbol ->
   (ToString[s] & ),
 s_String ->
   (s & ),

 l_List ->
   (If[#2 <= Length[l], l[[#2]],
       Message[NamingRules::list, l]; Abort[]] & ),

 f_Function :>
   If[Head[f["", 1]] === String, f,
      Message[NamingRules::function]; Abort[]],

 x_ :>
   (Message[NamingRules::keys, x, $NamingRulesKeys]; Abort[])};

$MinimizeTopologyMethods =
{"Cuts", "Mask"};

Options[MinimizeTopologies] =
{Method -> {"Cuts", "Mask"},   (* cf. $MinimizeTopologyMethods *)
 Naming -> Inherit,    (* cf. NamingRules *)
 Parallel -> False,    (* cf. $FlagRules *)
 Status -> True,       (* cf. $FlagRules *)
 Verbosity -> False};  (* cf. VerbosityRules *)

MinimizeTopologies::usage = "\
MinimizeTopologies[<srcs>, [opts]] returns {<maps>, <trgs>}.  For each \
source topology object in <srcs> a mapping object is created in <maps> \
relating it to one target topology object in <trgs>.  This routine \
maps topologies to (sub-)topologies and thus produces a minimal set.  \
The naming of objects in the result can be controlled by the [Naming]
option.";

MinimizeTopologies[
  srcs:{TopologyPattern["Alpha"]...},
    opts:OptionsPattern[]] :=
  Module[
    {ng,st, tops, maps = {}, trgs = {}, k  = 0, nm, done,
     i = 0, src, ns = "",fs,ps,rs,ls, sb,zr, j = 0, trg, nt = "",ft,pt,lt, ss, s,
     mask,sl,  tmptop},

    (* -- options -- *)

    {ng, st} = OptionValue[{Naming, Status}];

    ng = ng /. NamingRules[MinimizeTopologies];
    st = TrueQ[st /. $FlagRules];

    (* -- main -- *)

    If[st, Status[
      All, "Minimize set of ", Length[srcs], " topologies.",
      Temporary -> False]];

    (* descending by number of factors *)
    tops = SortBy[srcs, -Length[apar /. #] & ];

    If[st, Status[
      FrontEnd, "-> Inspecting \"", ns, "\": \"", nt, "\" (", Length[maps], " done, ", Length[trgs], " found)",
      Temporary -> True]];
    (* ^-- TODO *)

    (* iterate over whole set *)
    Do[
      src = tops[[i]];

      (* case: already mapped *)
      If[(done /. src) === True,
         Continue[]];

      {ns, fs, ps, rs} = {name, facs, apar, arep} /. src;
      fs = First /@ fs;
      ls = Length[ps];

      If[st, Status[
        Console, "-> Inspecting \"", ns, "\"... (", Length[maps], " done, ", Length[trgs], " found)", Temporary -> True]];

      nm = ng[ns, ++k];

      tmptop = InspectTopology[src, All, Sequence @@ FilterRules[{opts}, Options[InspectTopology]]];  (* <- TODO: call *)
      tmptop = Topology[tmptop, name -> nm, hist -> {"MinimizeTopologies", ns}];
      {sb, zr} = {subt, zero} /. tmptop;

      (* case: new topology *)
      AppendTo[maps, Mapping[ns, nm, ls]];
      AppendTo[trgs, tmptop];

      (* iterate over remaining *)
      Do[
        trg = tops[[j]];

        (* case: already mapped *)
        If[(done /. trg) === True,
           Continue[]];

        {nt, ft, pt, rt} = {name, facs, apar, arep} /. trg;
        ft = First /@ ft;
        lt = Length[pt];

        If[st, Status[
          Console, "-> Inspecting \"", ns, "\": \"", nt, "\" (", Length[maps], " done, ", Length[trgs], " found)",
          Temporary -> True]];

        (* helper *)
        mask = Function[
          sl, If[Or @@ MapThread[
            DenominatorSymbolQ[#1] && NumeratorSymbolQ[#2] & ,
            {fs[[sl]], ft}], {}, sl]];

        (* NEW: "Mask" keyword *)
        ss = Select[sb, Length[#[[1]]] === lt & ];
        ss = mask /@ # & /@ ss;
        ss = DeleteCases[#, {}] & /@ ss;
        ss = DeleteCases[ss, {}];
        If[ss =!= {}, ss = First /@ ss];

        (*ss = #[[1]] & /@ Select[sb, Length[#[[1]]] === lt & ];*)

        (* iterate over subsets *)
        Do[

          (* case: new mapping *)
          If[Expand[rt - (rs /. PermuteRules[s, ps])] === {0, 0},
             AppendTo[maps, Mapping[nt, nm, s]];
             AppendTo[tops[[j]], done -> True];
             Break[]],

          {s, ss}],
        {j, i + 1, Length[tops]}],
      {i, 1, Length[tops]}];

    (* ascending by number of factors *)
    trgs = SortBy[trgs, Length[apar /. #] & ];
                                                                        (* TODO: sort by number of d[] in facs, then n[] *)

    If[st, Status[
      All, "Minimized ", Evaluate @ Length[maps], " to ",
      Evaluate @ Length[trgs], " topologies.", Temporary -> False]];

    {maps, trgs}];

MinimizeTopologies[___] :=
  (Message[MinimizeTopologies::usage];
   Abort[]);

(* TODO:
- samples?
- description/dependencies
- Parallel...?
- Verbosity
- "Mask"
*)

(* --- MapTopologyToIndependents ------------------------------------ *)

(*
- map generics to linearly independent sub-topologies

gentops -- generic topologies, produced by MapTopTop[]
vx -- variable scalar products, e.g. {p0*v1, v1^2}
cx -- independent constants, e.g. {p0^2, m^2}

output: {<genmaps>, <lintops>}

genmaps -- mappings from generic to irreducible topologies, e.g.
{fr -> "GT1", to -> "IT1s1", id -> {1,2,4}}

lintops -- list of linearly independent sub-topologies, e.g.
{name -> "GT1s1",
dens -> {(v1+p0)^2, v1^2 + m^2, (v1-2*p0)^2},
xpar -> {x[1], x[2], x[3]},
arep -> {-x[1]-x[2]-x[3], -m^2*x[1]*x[2]-m^2*x[2]^2-m^2*x[2]*x[3]}}

- calls:
PermuteTable
treat single topology

- basic subsets: representatives of all unique non-vanishing subsets of
gen with length rnk]
*)

(* #1: basename, #2: index, #3: subset *)
NamingRules[MapTopologyToIndependents] =
{$Iterate[o_Integer] ->
   Iterate[o],
 $Iterate[o_Integer][s_String] ->
   Iterate[s, o],
 $Iterate[o_Integer][i_Integer] ->
   Iterate[o + i],
 $Iterate[o_Integer][s_String, i_Integer] |
   $Iterate[o_Integer][i_Integer, s_String] ->
     Iterate[s, o + i],

 "Subt" ->
   (#1 <> "s" <> StringJoin[ToString /@ #3] & ),                           (* string "Inherit"? *)

 Inherit ->
   (#1 <> "s" <> ToString[#2] & ),
 Inherit[s_String] ->
   (#1 <> s <> ToString[#2] & ),

 Iterate ->
   ("LT" <> ToString[#2] & ),
 Iterate[s_String] ->
   (s <> ToString[#2] & ),
 Iterate[i_Integer] ->
   ("LT" <> ToString[i + #2] & ),
 Iterate[s_String, i_Integer] | Iterate[i_Integer, s_String] ->
   (s <> ToString[i + #2] & ),

 i_Integer ->
   (ToString[i] & ),
 s_Symbol ->
   (ToString[s] & ),
 s_String ->
   (s & ),

 l_List ->
   (If[#2 <= Length[l], l[[#2]],
       Message[NamingRules::list, l]; Abort[]] & ),

 f_Function :>
   If[Head[f["", 1, {1}]] === String, f,
      Message[NamingRules::function]; Abort[]],

 x_ :>
   (Message[NamingRules::keys, x, $NamingRulesKeys]; Abort[])};

$MapTopologyToIndependentsMethods =
{"Keep"};  (* identical (sub-)topologies *)

Options[MapTopologyToIndependents] =
{Method -> {},         (* cf. $MapTopologyToIndependentsMethods *)
 Naming -> Inherit,    (* cf. NamingRules *)
 Parallel -> False,    (* cf. $FlagRules *)
 Status -> True,       (* cf. $FlagRules *)
 Verbosity -> False};  (* cf. VerbosityRules *)

MapTopologyToIndependents::usage = "\
MapTopologyToIndependents[<top(s)>, [<set>], [opts]] returns {<maps>, \
<inds>} where <tops> is a list of input topology objects and <set> the \
setup definition.  <inds> is a list of topology objects composed only \
of linearly independent propagators each and <maps> is a list of \
mapping objects describing the decomposition of <tops> into <inds>.";

MapTopologyToIndependents::consistent = "\
Topology \"`1`\" failed the consistency check with the setup \
definition (scalar products and constants).  The following expression \
should be zero: `2`.";

(* case: plural *)
MapTopologyToIndependents[
  srcs:{TopologyPattern[]...}, set:SetupPattern[],
    opts:OptionsPattern[]] :=
  Module[
    {ng,pl,st, nm, f, tmp, maps,trgs, g, oss,nms},

    (* -- options -- *)

    {ng, pl, st} = OptionValue[{Naming, Parallel, Status}];

    {pl, st} = TrueQ[# /. $FlagRules] & /@ {pl, st};

    (* -- main -- *)

    If[st, Status[
      All,
      "Map ", Length[srcs], " topologies to linearly independents.",
      Temporary -> False]];

    nm = "";

    If[pl, SetSharedVariable[nm]];

    Status[FrontEnd, "-> Treating \"", nm, "\"...", Temporary -> True];

    (* helper: status, singular call *)
    f = Function[
      nm = name /. #;
      If[st && !pl, Status[
        Console, "-> Treating \"", nm, "\"...", Temporary -> True]];
      Quiet[MapTopologyToIndependents[#, set, opts]]];

    tmp = If[pl, ParallelMap, Map][f, srcs];

    {maps, trgs} = Transpose[tmp];

    (* -- naming -- *)

    (* helper: names from mappings, offset *)
    g[ms_, o_] := Module[
      {n = ng /. Iterate -> $Iterate[o] //.
         NamingRules[MapTopologyToIndependents]},
      MapIndexed[
        n[to /. #1, #2[[1]], id /.#1] & , ReverseMapping /@ ms]];

    (* offsets *)
    oss = Drop[FoldList[Plus, 0, Length /@ maps], -1];

    (* generate names *)
    nms = MapThread[g, {maps, oss}];

    (* -- result -- *)

    {nms, maps, trgs} = Join @@ # & /@ {nms, maps, trgs};

    (* rename mappings, topologies *)
    maps = MapThread[Mapping[fr /. #2, #1, id /. #2] & , {nms, maps}];

    trgs = MapThread[Topology[#2, name -> #1] & , {nms, trgs}];

    If[st, Status[
      All, "Created ", Evaluate @ Length[maps], " mappings and ",
      Evaluate @ Length[trgs], " topologies.", Temporary -> False]];    (* TODO: created " tops. from " tops. *)

    {maps, trgs}];

(* main: singular *)
MapTopologyToIndependents[
  src:TopologyPattern["Subt"], set:SetupPattern[],
    opts:OptionsPattern[]] :=
  Module[
    {me,ng,pl,st, cp, nm,fs, nf, vr, tm, chk, ni, sel, nms, maps, trgs},

    (* -- options -- *)

    {me, ng, pl, st} = OptionValue[{Method, Naming, Parallel, Status}];

    ng = ng /. NamingRules[MapTopologyToIndependents];
    {pl, st} = TrueQ[# /. $FlagRules] & /@ {pl, st};

    (* check: method *)
    cp = Complement[me, $MapTopologyToIndependentsMethods];
    If[cp =!= {},
       Message[Method::invalid, cp, $MapTopologyToIndependentsMethods];
       me = Method /. Options[MapTopologyToIndependents]];

    (* -- topology matrix -- *)

    {nm, fs} = {name, facs} /. src;
    nf = Length[fs];

    (* all variables: scalar products, constant, factor symbols *)
    vr = Join[Last /@ (ss /. set), xs /. set, First /@ fs];

    (* full topology matrix *)
    tm = TopologyMatrix[src, set];

    (* check: consistency *)
    chk = Expand[tm . vr - (#[[2]] - #[[1]] & /@ fs)];
    If[chk =!= (0 & /@ fs),
       Message[MapTopologyToIndependents::consistent, nm, chk];
       Return[{{}, {}}]];

    (* number of linearly independent factors *)
    tm = TopologyMatrixReduce[tm, set];

    (* reduced matrix *)
    ni = MatrixRank[tm];

    (* -- matching (sub-)topologies -- *)

    (* correct number of lines *)
    sel = Select[subt /. src, Length[#[[1]]] === ni & ];

    (* linearly independent factors *)
    sel = Select[sel, MatrixRank[tm[[#[[1]]]]] === ni & ];

    (* option: keep identical *)
    sel = If[MemberQ[me, "Keep"], Join @@ sel, First /@ sel];

    (* -- compose results -- *)

    (* create names *)
    nms = MapIndexed[ng[nm, #2[[1]], #1] & , sel];

    (* create mappings *)
    maps = MapThread[
      ReverseMapping[Mapping[#1, nm, #2], nf] & , {nms, sel}];

    (* apply mappings *)

    trgs = MapToTopology[#, src] & /@ maps;

    {maps, trgs}];

(* overload: "Setup" *)
MapTopologyToIndependents[
  src:TopologyPattern["Setup"],
    opts:OptionsPattern[]] :=
  MapTopologyToIndependents[src, setp /. src, opts];

(* trigger: InspectTopology[] *)
MapTopologyToIndependents[
  src:TopologyPattern[], set:SetupPattern[],
    opts:OptionsPattern[]] :=
  MapTopologyToIndependents[InspectTopology[src, set], set, opts];

(* trap *)
MapTopologyToIndependents[___] :=
  (Message[MapTopologyToIndependents::usage];
   Abort[]);

(* TODO:
verbosity
parallel, status in singular version?
*)

(* --- CutTopologies ------------------------------------------------ *)

Options[CutTopologies] =
  Options[Cuts];

CutTopologies::usage = "\
TODO";

CutTopologies[
  srcs:{TopologyPattern["Graph"]..},
    opts:OptionsPattern[]] :=
  Module[
    {ls, cs, maps, trgs},

    ls = Length /@ (facs /. srcs);

    Status[
      All,
      "Inspect cuts of ", Length[srcs], " topologies.",
      Temporary -> False];

    cs = Cuts[#, opts] & /@ srcs;

    maps = MapThread[Mapping[#1, If[#3 === {}, 0, #1], #2] & , {name /. srcs, ls, cs}];

    trgs = MapThread[Topology[#1, cuts -> #2, hist -> {"CutTopologies", opts}] & , {srcs, cs}];

    Status[
      All,
      "Created ", Length[maps], " mappings and ", Length[trgs], " topologies.",
      Temporary -> False];

    {maps, Select[trgs, Length[cuts /. #] > 0 & ]}];

(* trigger:  *)
(*CutTopologies[
  srcs:{TopologyPattern[]..}, set:SetupPattern[],
    opts:OptionsPattern[]] :=
  CutTopologies[GraphTopology[#, set] & /@ srcs, opts];*)  (* TODO? *)

(* trap *)
CutTopologies[___] :=
  (Message[CutTopologies::usage];
   Abort[]);

(* !!! TODO !!! *)
(* status, ... *)

(* --- MapTopologyToCuts -------------------------------------------- *)

Options[MapTopologyToCuts] =
{Method -> Automatic,
 Naming -> Inherit,
 Verbosity -> False};

MapTopologyToCuts[
  tops_?TopologyListQ, opts:OptionsPattern[]] :=
  Join @@@ Transpose[MapTopologyToCuts[#, opts] & /@ tops];

(* no Symm -> trigger or # cut lines *)












MapTopologyToCuts[
  top:TopologyPattern["Symm", "Cuts"], opts:OptionsPattern[]] :=
  Module[
    {ng,vb, idm, gls, inQ, gsy, ccs, sig,sigs, nm,nms, maps, sms, tops},

    (* options *)
    {ng, vb} = OptionValue[{Naming, Verbosity}];
    vb = vb /. VerbosityRules;
    Report[vb, 1, "MapTopologyToCuts[]."];

    (* -- find classes of cuts -- *)

    (* indices of graph lines *)
    idm = Range[Length[pros /. top]];
    gls = Union @@ Position[pros /. top, {__, 0, 0}, {1}];
    gls = Complement[idm, gls];
    (* possible global graph symmetry *)
    inQ = Complement[gls, #] === {} & ;
    gsy = Select[symm /. top, And @@ (inQ /@ #) & , 1];
    (* cut classes *)
    ccs = List /@ (cuts /. top);
    (* case: global symmetry exists *)
    If[gsy =!= {},
       gsy = First[gsy];
       Report[vb, 2, "Global graph symmetry:\n  ", gsy, "."];
       sig = Function[
         {c, s}, Join @@ (First[Position[s, #, {1}, 1]] & /@ c)];
       sigs = Function[c, Sort[sig[c, #] & /@ gsy]];
       ccs = GatherBy[cuts /. top, sigs]];
    Report[vb, 2, "Distinct cut classes:\n  ", ccs, "."];

    (* -- naming and mappings -- *)

    nm = name /. top;
    nms = MapIndexed[FromCharacterCode[64 + First[#2]] & , ccs];
    nms = nm <> "c" <> # & /@ nms;
    maps = Mapping[nm, #, idm] & /@ nms;

    (* -- dissect topology by cuts -- *)

    (* filter symmetries *)
    sms = Function[
      cc,
      (CutsSelectGroups[symm /. top, cc]
       // CutsSymmetricGroups[#, cc] &
       // SortSymmetricGroups
       // FilterSymmetricGroups)] /@ ccs;

    (* create topologies *)
    tops = MapThread[
      Topology[top, name -> #1, symm -> #2, cuts -> #3] & ,
      {nms, sms, ccs}];

    {maps, tops}];

(* N.B.:
- Cuts are determined on base of the full graph.
- Therefore, only cuts connected by a global graph symmetry can be
  considered together.
- Irreducible numerators are irrelevant in this context.
- Afterwards apply ZeroCutsTopology[] to translate cuts to zeros.
*)












(* --- MapTopologyToFactors ----------------------------------------- *)

NamingRules[MapTopologyToFactors] =
{Inherit ->
   (#1 <> "f" <> ToString[#2] & ),
 Inherit[s_String] ->
   (#1 <> s <> ToString[#2] & ),

 Iterate ->
   ("FT" <> ToString[#2] & ),
 Iterate[s_String] ->
   (s <> ToString[#2] & ),
 Iterate[i_Integer] ->
   ("FT" <> ToString[i + #2] & ),
 Iterate[i_Integer, s_String] | Iterate[s_String, i_Integer] ->
   (s <> ToString[i + #2] & ),

 i_Integer ->
   (ToString[i] & ),
 s_Symbol ->
   (ToString[s] & ),
 s_String ->
   (s & ),

 l_List ->
   (If[#2 <= Length[l], l[[#2]],
       Message[NamingRules::list, l]; Abort[]] & ),

 f_Function :>
   If[Head[f["", 1]] === String, f,
      Message[NamingRules::function]; Abort[]],

 x_ :>
   (Message[NamingRules::keys, x, $NamingRulesKeys]; Abort[])};

$MapTopologyToFactorsMethods =
{"Euclid", "Euclidean",        (* -> negative alpha-polynomials *)
 "Minkowski", "Minkowskian"};  (* -> positive alpha-polynomials *)

Options[MapTopologyToFactors] =
{Method -> "Euclidean",
 Naming -> Inherit};

MapTopologyToFactors::usage = "\
MapTopologyToFactors[<top(s)>] returns {<maps>, <facs>} where <top(s)> \
is a list of topology objects, <maps> is a list of mapping objects \
relating <top(s)> to a set of factor topologies <facs> if \
factorization is possible.";

MapTopologyToFactors::factors = "\
Warning: No factorization found for topology \"`1`\".";

MapTopologyToFactors[
  tops_?TopologyListQ, set:Elective[SetupPattern[]],
    opts:OptionsPattern[]] :=
  DeleteCases[Join @@ #, {}] & /@
  Transpose[MapTopologyToFactors[#, opts] & /@ tops];

(*
TODO: tops -> srcs

Status[
All,
"Decompose ", Length[srcs], " topologies into factors.",
Temporary -> False];

Status[
All,
"Created ", Length[maps], " mappings and ", Length[trgs], " topologies.";
Temporary -> False];
*)

MapTopologyToFactors[
  tops:{}, set:Elective[SetupPattern[]],
    opts:OptionsPattern[]] :=
  {{}, {}};                                                                 (* TODO: needed? *)

MapTopologyToFactors[
  top:TopologyPattern["Alpha"],
    opts:OptionsPattern[]] :=
  Module[
    {me,ng, cp, s, ap,ar, au,aw, f, aus,aws, ars, g, ids, aps, nms,
     maps, tops},

    (* -- options -- *)

    {me, ng} = OptionValue[{Method, Naming}];
    me = Flatten[{me}];
    ng = ng /. NamingRules[MapTopologyToFactors];

    (* check: method *)
    cp = Complement[me, $MapTopologyToFactorsMethods];
    If[cp =!= {},
       Message[Method::invalid, cp, $MapTopologyToFactorsMethods];
       me = Method /. Options[MapTopologyToFactors]];

    (* sign of metric *)
    s = If[MemberQ[{"Minkowski", "Minkowskian"}, me], -1, +1];

    {ap, ar} = {apar, arep} /. top;

    (* -- check: topology factorizes -- *)

    {au, aw} = ar;

    (* try to factor U-polynomial *)
    au = Factor[au];

    (* check: no factorization *)
    If[Head[au] =!= Times,
       Message[MapTopologyToFactors::factors, name /. top];
       Return[{{}, {}}]];

    (* -- factorize alpha-representation -- *)

    (* helper: product of U polynomials -> sub-polynomial of W *)
    f = Function[r, Select[
      PolynomialReduce[aw, r][[1, 1]],
      Intersection[Variables[#], Variables[r]] === {} & ]];

    (* U polynomials <- factors of U *)
    aus = List @@ au;

    (* ignore purely numeric factors *)
    aus = Select[aus, Intersection[Variables[#], ap] =!= {} & ];

    (* W polynomials <- factor off all but one factor of U from W *)
    aws = f[au/#] & /@ aus;

    (* alpha-representations *)
    ars = Transpose[{aus, aws}];

    (* -- create mappings -- *)

    (* helper: alpha-parameters -> mapping ID *)
    g = Flatten[Position[ap, #] & /@ Intersection[ap, Variables[#]]] & ;

    (* create IDs *)
    ids = g /@ aus;

    (* apply on alpha-parameters, alpha-representation, fix sign *)
    aps = Take[ap, Length[#]] & /@ ids;
    ars = MapThread[#1 /. PermuteRules[#2, ap] &, {ars, ids}];
    ars = If[MatchQ[#[[1, 1]], Times[-1, __]], s, -s]*# & /@ ars;

    (* create names for the factors *)
    nms = MapIndexed[ng[name /. top, #2[[1]]] & , ids];

    (* inverted mappings *)
    maps = MapThread[ReverseMapping[
      Mapping[#1, name /. top, #2], Length[ap]] & , {nms, ids}];

    (* -- compose result -- *)

    (* apply mappings on topology *)
    tops = MapToTopology[#, top] & /@ maps;

    (* insert correct alpha-representations *)
    tops = MapThread[Topology[
      #1, apar -> #2, arep -> #3] & , {tops, aps, ars}];

    {maps, tops}];

MapTopologyToFactors[___] :=
  (Message[MapTopologyToFactors::usage];
   Abort[]);

Off[MapTopologyToFactors::factors];

(* TODO:
- Verbosity?

- Naming for plural case

- Parallel...
- Status
*)

(* --- MapTopologyToIntegral ---------------------------------------- *)

MapTopologyToIntegral::usage = "\
TODO";

MapTopologyToIntegral::names = "\
Warning: Mapping source \"`1`\" and topology name \"`2`\" do not \
match.";

MapTopologyToIntegral::ninds = "\
Warning: Number of indices in mapping to \"`1`\" (`3`) incompatible \
with number of factors in \"`2`\" (`4`).";

MapTopologyToIntegral::matches = "\
Warning: Found no matches for topology \"`1`\".";

MapTopologyToIntegral::multi = "\
Warning: Multiple topologies named \"`1`\" found.";

(* overload: plural, plural *)
MapTopologyToIntegral[
  maps_?MappingListQ, tops_?TopologyListQ,
    set:Elective[SetupPattern[]]] :=
  Transpose[Join @@ (Transpose[
    MapTopologyToIntegral[maps, #, set]] & /@ tops)];

(* overload: plural, singular *)
MapTopologyToIntegral[
  maps_?MappingListQ, top:TopologyPattern[],
    set:Elective[SetupPattern[]]] :=
  Module[
    {sel},
    (* candidate mappings *)
    sel = Select[maps, (fr /. #) === (name /. top) & ];
    (* check: no match *)
    If[sel === {},
       Message[MapTopologyToIntegral::matches, name /. top];
       Return[{{}, {}}]];
    (* result: via singular version *)
    Transpose[MapTopologyToIntegral[#, top, set] & /@ sel]];

(* overload: singular, plural *)
MapTopologyToIntegral[
  map:MappingPattern[], tops_?TopologyListQ,
    set:Elective[SetupPattern[]]] :=
  Module[
    {sel},
    (* candidate topologies *)
    sel = Select[tops, (fr /. map) === (name /. #) & ];
    (* check: no match *)
    If[sel === {},
       Message[MapTopologyToIntegral::matches, fr /. map];
       Return[{{}, {}}]];
    (* check: multiple matches *)
    If[Length[sel] > 1,
       Message[MapTopologyToIntegral::multi, fr /. map]];
    (* result: via singular *)
    MapTopologyToIntegral[map, sel[[1]], set]];

(* main: singular, singular *)
MapTopologyToIntegral[
  map:MappingPattern[], top:TopologyPattern[],
    set:Elective[SetupPattern[]]] :=
  Module[
    {},
    (* check: names *)
    If[(fr /. map) =!= (name /. top),
       Message[MapTopologyToIntegral::matches, fr /. map, name /. top];
       Return[{{}, {}}]];
    (* check: number of indices *)
    If[Length[id /. map] =!= Length[facs /. top],
       Message[MapTopologyToIntegral::ninds, to /. map, name /. top,
         Length[id /. map], Length[facs /. top]];
       Return[{{}, {}}]];
    (* result: canonical form *)
    CanonicalizeIntegral[
      Integral[
        top, name -> (to /. map), inds -> (id /. map),
        hist -> {"MapTopologyToIntegral", (id /. map)}],
      set]];

(* trap *)
MapTopologyToIntegral[___] :=
  (Message[MapTopologyToIntegral::usage];
   Abort[]);

Off[MapTopologyToIntegral::matches];

(* N.B.:
- Find matching mappings and topologies.
- Store indices and delete unneeded factors via CanonicalizeIntegral[].
*)

(* --- MapIntegralToIntegral ---------------------------------------- *)

MapIntegralToIntegral::usage = "\
TODO";

MapIntegralToIntegral[
  srcs:{IntegralPattern["Alpha"]...},
    trgs:{IntegralPattern["Alpha"]...}] :=
  Union[DeleteCases[
    Join @@ (MapIntegralToIntegral[#, trgs] & /@ srcs), {}]];

MapIntegralToIntegral[
  srcs:{IntegralPattern["Alpha"]...}, trg:IntegralPattern["Alpha"]] :=
  Union[DeleteCases[MapIntegralToIntegral[#, trg] & /@ srcs, {}]];

MapIntegralToIntegral[
  src:IntegralPattern["Alpha"], trgs:{IntegralPattern["Alpha"]...}] :=
  Union[DeleteCases[MapIntegralToIntegral[src, #] & /@ trgs, {}]];

MapIntegralToIntegral[
  src:IntegralPattern["Alpha"], trg:IntegralPattern["Alpha"]] :=
  If[({inds, arep} /. src) === ({inds, arep} /. trg),
     Mapping[name /. src, name /. trg, Length[inds /. src]],
     {}];

MapIntegralToIntegral[___] :=
  (Message[MapIntegralToIntegral::usage];
   Abort[]);

(* TODO:

- Options: Status, ...

*)

(* --- MinimizeIntegrals -------------------------------------------- *)

MinimizeIntegrals::usage = "\
TODO";

MinimizeIntegrals[ints:{IntegralPattern["Alpha"]...}] :=
  Module[
    {inti, maps, into, done,
     i, int1, n1,i1,a1, l1, j, int2, n2,i2,a2, l2,                   st},

    (* group integrals by the number of factors *)
    inti = SortBy[ints, -Length[inds /. #] & ];

    (* minimal list of integrals, identification table *)
    maps = into = {};

    (* iterate over whole set *)
    Do[
      int1 = inti[[i]];

      (* case: already mapped *)
      If[(done /. int1) === True,
         Continue[]];

      {n1, i1, a1} = {name, inds, arep} /. int1;
      l1 = Length[i1];

      (* case: new integral *)
      AppendTo[into, int1];
      AppendTo[maps, Mapping[n1, n1, l1]];

      (* iterate over remaining *)
      Do[
        int2 = inti[[j]];

        (* case: already mapped *)
        If[(done /. int2) === True,
           Continue[]];

        {n2, i2, a2} = {name, inds, arep} /. int2;
        l2 = Length[i2];

        (* case: no match *)
        If[l1 =!= l2 || i1 =!= i2,
           Continue[]];

        (* case: new mapping *)
        If[a1 === a2,
           AppendTo[maps, Mapping[n2, n1, l2]];
           AppendTo[inti[[j]], done -> True]],

        {j, i + 1, Length[inti]}],

      {i, 1, Length[inti]}];


    st = True;

    If[st, Status[
      All, "Minimized ", Evaluate @ Length[maps], " to ",
      Evaluate @ Length[into], " integrals.", Temporary -> False]];

    {maps, into}];

(* N.B.:
- Integrals should be in canonical representation obtained, e.g, by
  CanonicalizeIntegral[].
*)

(* TODO:

- add history?

- Options as in MinimizeTopology[].

*)

(* --- MapIntegralToTopology ---------------------------------------- *)

Options[MapIntegralToTopology] =
{};

Options[MapIntegralToTopology] = Join @@ Options /@
{MapIntegralToTopology, MapTopologyToTopology, TopologyMomentaShifts};

MapIntegralToTopology::usage = "\
MapIntegralToTopology[<int(s)>, <top(s)>, [<set>], [<n>], [opts]] \
tries to express integral(s) <int(s)> via (linear combinations of) \
integrals in topology/topologies <top(s)> and returns at most [<n>] \
relations if [<n>] is given, otherwhise all.
The option [Method] allows control over the mapping of integral to \
topology denominators, cf. MapTopologyToTopology.";

MapIntegralToTopology::symbols = "\
Warning: Expected denominator symbol(s) in \"`1`\" for `2`.";

MapIntegralToTopology::match = "\
Warning: No match for denominators of \"`1`\" in \"`2`\".";














(* NOTE: Order must be like this! *)

(* overload: plural, plural -> singular, plural *)
MapIntegralToTopology[
  ints_?IntegralListQ, tops_?TopologyListQ, args___] :=
  Union[DeleteCases[
    Join @@ (MapIntegralToTopology[#, tops, args] & /@ ints), {}]];

(* overload: plural, singular -> singular, singular *)
MapIntegralToTopology[
  ints_?IntegralListQ, top:TopologyPattern[], args___] :=
  Union[DeleteCases[
    Join @@ (MapIntegralToTopology[#, top, args] & /@ ints), {}]];

(* overload: singular, plural -> singular, singular *)
MapIntegralToTopology[
  int:IntegralPattern[], tops_?TopologyListQ, args___] :=
  Union[DeleteCases[
    Join @@ (MapIntegralToTopology[int, #, args] & /@ tops), {}]];











(* main *)
MapIntegralToTopology[
  int:IntegralPattern[], top:TopologyPattern["Alpha", "Solve"],
    set:SetupPattern[], n_Integer:-1, opts:OptionsPattern[]] :=
  Module[
    {me,vb,
     ids,ins,ips, pk, dmap,dint, maps, res, is,ts, bs,si, map,nap, tint,dtop,
     den,num, shft, c, exp,         ID,

     os,

     tfs, tds, tns, rls},

    (* options *)
    {me, vb} = OptionValue[{Method, Verbosity}];
    me = Flatten[{me}];
    vb = vb /. VerbosityRules;

    os = FilterRules[{opts}, Options[MapTopologyToTopology]];


    Report[vb, 1, name /. int, " -> ", name /. top, "..."];

    (* -- integral: factors -- *)

    (* denominators, numerators *)
    ids = Flatten[Position[inds /. int, x_ /; x > 0]];
    ins = Flatten[Position[inds /. int, x_ /; x <= 0]];

    (* factors *)
    ips = First /@ Part[facs /. int, ids];

    (* check: symbols *)
    pk = Not /@ DenominatorSymbolQ /@ ips;
    If[Or @@ pk,
       Message[MapIntegralToTopology::symbols,
         name /. int, Pick[ips, pk]];
       If[MemberQ[me, "Mask"],
          Return[{}]]];

    (* -- integral: mapping -- *)

    (* create *)
    dmap = Mapping["d" <> (name /. int), name /. int, ids];
    dmap = ReverseMapping[dmap, Length[facs /. int]];

    (* apply *)
    dint = MapToTopology[dmap, int];
    dint = CanonicalizeTopology[dint, set];

    (*
    Print[];
    Print[dint[[1]]];
    Print[dmap];
    Print[];
    *)

    {dmap, dint} = {ComposeMapping[dmap, dint[[1]]], dint[[2]]};

    (* -- match denominators -- *)

    maps = MapTopologyToTopology[dint, top, -1, Sequence @@ os];  (* NEED ALL *)

    (*
    Print[];
    Print /@ maps;
    Print[];
    *)

    (* check: no match *)
    If[maps === {},
       Message[MapIntegralToTopology::match,
         name /. int, name /. top];
       Return[{}]];

    (* -- loop over mappings -- *)

    res = {};

    (* TODO: check conversion *)
    {is, ts} = Symbol /@ (name /. {int, top});

    (* base scale *)
    bs = First[xs /. set];
    si = Total[inds /. int];

    Do[

      If[n > 0 && Length[res] == n,
         Break[]];

      Report[vb, 2, "mapping: ", map];

      nap = ReverseMapping[map, Length[facs /. top]];

      Report[vb, 2, "reverse: ", nap];

      (* apply mapping: integral, topology *)
      tint = MapToTopology[map, dint];
      dtop = MapToTopology[nap, top];

      (*
      Print[];
      Print[inds /. dint];
      Print[inds /. tint];
      Print[];
      *)

      (* denominator: expression *)
      den = Inner[Power, First /@ (facs /. dtop), inds /. dint, Times];

      Report[vb, 2, "integral denominator: ", den];

      (*
      Show[IntegralPlot[tint]];
      *)

      If[vb > 0, c = Unique["Global`c"]];

      (* numerator: expression *)
      num = Part[#, ins] & /@ ({inds, facs} /. int);
      num = Inner[c[#2]^-#1 & , num[[1]], Last /@ num[[2]], Times];

      Report[vb, 2, "integral numerator: ", num];

      (* numerator: find momenta shifts *)
      shft = {};
      If[ins =!= {},
         shft = TopologyMomentaShifts[dint, dtop, set]];

      Report[vb, 2, "momenta shifts: ", shft];

      (* case: nothing found *)
      If[ins =!= {} && shft === {},
         Continue[]];

      (* numerator: apply shifts *)
      num = num /. c[x_] :> c[Expand[x /. shft]];

      Report[vb, 2, "applied shifts: ", num];

      (* numerator: reexpress in topology *)
      num = Expand[num /. (scps /. top) /. (cs /. set) /. c[x_] -> x];

      Report[vb, 2, "reexpressed in topology: ", num];

      (* whole expression *)
      exp = Expand[num/den];

      Report[vb, 2, "whole expression: ", exp];

      (*  *)

      tfs = First /@ (facs /. top);

      Report[vb, 2, "denominator variables: ", tfs];

      tds = id /. map;
      tns = Complement[Range[Length[tfs]], tds];

      If[vb > 0, ID = Unique["Global`ID"]];

      tdr = MapIndexed[#1 -> ID[tds[[#2[[1]]]]] & , First /@ (facs /. dtop)];
      tnr = tfs[[#]] -> ID[#] & /@ tns;
      rls = Join[tdr, tnr];

      Report[vb, 2, rls];


      exp = Expand[exp*ts[0 & /@ tfs]];
      exp = exp /. rls;
      exp = exp /. ID[i_]^p_. -> ID[i, p];
      exp = exp //. ts[l_]*ID[i_, p_] :> ts[ReplacePart[l, {i} -> l[[i]] - p]];
      exp = exp /. ts[l_List] :> bs^(si - Total[l])*ts[Sequence @@ l];

      exp = Collect[exp, ts[___], Factor];

      AppendTo[res, exp],

      {map, maps}];

    Report[vb, 1, Length[res], " relations."];

    (* result: relation rules *)
    is -> # & /@ res];





(* overload: top["Setup"] *)
MapIntegralToTopology[
  int:IntegralPattern["Setup"], top:TopologyPattern[], args___] /;
FreeQ[{args}, SetupPattern[]] :=
  MapIntegralToTopology[int, top, setp /. int, args];

(* overload: top["Setup"] *)
MapIntegralToTopology[
  int:IntegralPattern[], top:TopologyPattern["Setup"], args___] /;
FreeQ[{args}, SetupPattern[]] :=
  MapIntegralToTopology[int, top, setp /. top, n, opts];









(* trap *)
MapIntegralToTopology[___] :=
  (Message[MapIntegralToTopology::usage];
   Abort[]);

Off[MapIntegralToTopology::match];





(* N.B.:

clear integral numerators

find mapping to topology (subset of topology denominators matching
integral denominators)

apply mapping to integral

apply reverse mapping to topology

find momenta shifts between denominators of intersection

integral denominator composed of of new symbols (reversely mapped
topology) and old indices (mapped integral); integral numerator composed
of old explicit representation and indices, then the shifts are applied;
the expression is reexpressed in terms of topology factors, therefor the
topology must be complete and solved beforehand

*)

(* TODO:

- use information from subt, symm

- apply (rs /. set)

- introduce: optional argument for number of relations, also in
  overloads

- check: scalelessness?

- Union[] in the end.

- Status[]

- Shift computed, when no numerator or not mapping?

- ParallelTry?  Catch[Map[Throw[]]]

- triggers: e.g. reduce?, inspect?

- setup overloads

*)
















(* --- package end -------------------------------------------------- *)

Protect["TopoID`Topology`*"];

Scan[
  SetAttributes[#, {ReadProtected}] & ,
  Select[Symbol /@ Names["TopoID`Topology`*"], Head[#] === Symbol & ]];

End[];

EndPackage[];

(* ------------------------------------------------------------------ *)

(* --- TODO:
- Unify Naming as for MapTopologyToIndependents.
*)

(* --- XXX DUMP XXX *)

(* Verbosity -> If[pl, 0, vb + 1]; *) (* adjust output from workers *)

(*
    Report[vb, 2, "Reorder data..."];
    Report[vb, 2, "[MapDiagramsToTopologies] done."];
    Report[vb, 1, "[MapDiagramsToTopologies]:\n  ",
           "mapping ", n, " diagrams -> ", n, " topologies,\n  ",
           "Parallel -> ", pl, "."];
    Report[vb, 2, "Introduce counting index..."];
*)

(*
    Report[vb, 1, "[MapDiagramToTopology]:\n  ", "mapping \"", name /. dia, "\" -> ", nm, "."];
    Report[vb, 2, "Scalar propagators:\n  ", fs, "."];
    Report[vb, 2, "Constrained propagators:\n  ", ft, "."];
    Report[vb, 2, "Unique and non-constant:\n  ", fc, "."];
    Report[vb, 2, "Mapping scheme:\n  ", pm, "."];
    Report[vb, 2, "Inverted mapping:\n  ", ip];
    Report[vb, 2, "Remapped propagators:\n  ", fs];
    Report[vb, 3, "Alpha-representation:\n  ", ar, "."];
    Report[vb, 2, "-> Scaleless diagram!"];
    Report[vb, 2, "Canonical ordering:\n  ", pn, "."];
    Report[vb, 2, "Reordered propagators:\n  ", fc, "."];
    Report[vb, 3, "Reordered alpha-representation:\n  ", ar, "."];
    Report[vb, 2, "Reordered mapping:\n  ", id /. map, "."];
    Report[vb, 2, "-> Created mapping and topology \"", nm, "\"!"];
    Report[vb, 1, "[MapDiagramToTopology] done."];
*)

(*
Report[vb, 1, "[maptt]: ", Length[ts], " topologies..."];
Report[vb, 2, "Inspecting ", name /. tp, "..."];
Report[vb, 3, "-> Already mapped!"];
Report[vb, 3, "-> added new entry!"];
Report[vb, 4, "Inspecting ", name /. cn, "..."];
Report[vb, 5, "-> Already mapped!"];
Report[vb, 5, "Subsets with ", lc, " lines:\n  ", sc, "."];
Report[vb, 10, "\n", rc, "\n", rt, "\n", rt /. PermuteRules[p, pt]];
Report[vb, 6, "-> Found mapping:\n  ", p, "."];
*)

(*
NamingRules[MapTopologiesToIndependents] =
{Iterate ->
   (Iterate[#] &),
 Iterate[i_Integer] ->
   (Iterate[i + #] &),
 Iterate[p_String] ->
   (Iterate[p, #] &),
 Iterate[i_Integer, p_String] | Iterate[p_String, i_Integer] ->
   (Iterate[p, i + #] &),
 Iterate[p_String, s_String] ->
   (Iterate[#, p, s] &),
 Iterate[i_Integer, p_String, s_String]
 | Iterate[p_String, i_Integer, s_String]
 | Iterate[p_String, s_String, i_Integer] ->
   (Iterate[i + #, p, s] &),
 x_ ->
   (x & )};
*)

(*
Report[vb, 1, "[MapTopologyToIndependents]: ", name /. top, "..."];
Report[vb, 2, "Topology factors:\n  ", fs, "."];
Report[vb, 2, "Number of factors: ", nf, "."];
Report[vb, 2, "All variables:\n  ", vr, "."];
Report[vb, 2, "Topology matrix (full):\n  ", TableForm[tm], "."];
Report[vb, 2, "Check for consistency:\n  ", ch, "."];
Report[vb, 3, "-> Topology inconsistent!"];
Report[vb, 2, "Topology matrix (reduced):\n  ", TableForm[tn], "."];
Report[vb, 2, "Number of linearly independents: ", ni, "."];
Report[vb, 2, "Matching sub-topologies:\n  ", cln, "."];
Report[vb, 2, "Linearly independents:\n  ", cli, "."];
Report[vb, 2, "Created mappings and topologies:\n  ", nms, "."];
Report[vb, 2, "[MapTopologyToIndependents]: done."];
*)

(*
Report[vb, 1, "[MapTopologiesToIndependents]:\n  ",
"mapping ", n, " topologies -> independents,\n  ",
"Parallel -> ", pl, "."];
Report[vb, 2, "Introduce counting index..."];
Report[vb, 2, "Reorder data..."];
Report[vb, 2, "[MapTopologiesToIndependents] done."];
*)

(*
Status[FrontEnd,
"-> Mapping topologies to independents: ", i, "/", n, "...",
Temporary -> True];
Status[Console,
"Mapped topologies to independents: ",
Length[itops], " -> ",
Length[otops], ".",
Temporary -> False];
Status[FrontEnd,
"Mapped topologies to independents: ",
Evaluate @ Length[itops], " -> ",
Evaluate @ Length[otops], ".",
Temporary -> False];
*)
