(* -- "Object.m": Handle Properties of Objects ---------------------- *)

(* --- provided functions: TODO

GraphMapTopology[<map>, <top>] -> <top>
  (map) -> (legs, pros)
-- apply mapping only on graph data

LabelTopology[{<top>, <set>] -> <top>
  (set) -> (legs, pros)
-- apply mass labels to topology graph




MapToTopology[<map>, <top>] -> <top>
  (inds,facs, scps,rels, apar,arep, subt,zero,symm,cuts),
  calls GraphMapTopology[]
-- apply mapping to various entries simultaneously if given

CanonicalizeTopology[<top(s)>, [<set>]] -> {<map(s)>, <top(s)>}
  (facs, set) -> (apar, arep), calls MapToTopology[]
-- compute or reorder the alpha-representation



InspectTopology[<top(s)>, [<set>], [<lvl>], [opts]] -> <top(s)>         DONE
  (apar, arep, facs, cuts) -> (subt, zero)
  possibly calls CanonicalizeTopology[]
-- compute and insert (sub-)topologies into an object



CompleteTopology[<top(s)>, <set>, [opts]] -> {<map(s)>, <top(s)>}
  (facs, set) -> (inds, facs, pros)
-- complete the set of factors such that it can be reduced for all
   scalar products

ReduceTopology[<top(s)>, <set>] -> <top(s)>
  (facs, set) -> (scps, rels)
-- invert for scalar products and find additional relations



SymmetrizeTopology[<top(s)>, [<set>], [<level>], [opts]] -> <top(s)>    DONE
  (apar, arep, facs, cuts) -> (symm, zero)
  possibly calls CanonicalizeTopology[]
-- insert available symmetries into an object



CutsTopology[<top(s)>, [opts]] -> <top(s)>
-- insert available cut information into an object

ZeroCutsTopology[<top>] -> <top>
  (zero, cuts) -> (zero)
-- combine available information on zero subsets and cuts into a common
   notation

PermuteTopology[<top>, <ord>] -> {<map>, <top>}
  (inds,facs, subt,zero,symm,cuts, pros)
-- permute various entries simultaneously if given, does not touch
   >apar<, >arep<

SortTopology[<top>] -> {<map>, <top>}                                   TODO -> LATER
  calls PermuteTopology[]
-- change order of entries by predefined sorting

HashTopology[<top>, <set>, [opts]] -> <top>                             TODO -> LATER

PrepareTopology[<top>, <set>, [opts]] -> {<map>, <top>}                 TODO
  (name, facs, scps,rels, zero,symm,cuts, setp)
-- prepare a topology for code generation

CanonicalizeIntegral[<int>, <set>] -> {<map>, <int>}
  calls MapTopology[] and CanonicalizeTopology[]
-- compute or reorder the alpha-representation

ClearNumeratorsTopology[<top(s)>] -> {<map(s)>, <top(s)>}
-- map <top(s)> in such way that pure numerator factors are dumped

TopologyScalefulQ[<top(s)>] -> <bool(s)>
-- check whether the object <top> has a scale or not

*)

(* --- package begin ------------------------------------------------ *)

Unprotect["TopoID`Object`*"];

ClearAll["TopoID`Object`*", "TopoID`Object`Private`*"];

BeginPackage[
  "TopoID`Object`",
  {"TopoID`Common`", "TopoID`System`",  (* TODO *)

   "TopoID`Setup`",
   "TopoID`Topology`",
   "TopoID`Mapping`",

   "TopoID`Polynomial`",
   "TopoID`Cuts`",

   "TopoID`FORM`"}];

{Null,

 LabelTopology,
 MapToTopology,
 CanonicalizeTopology,

 $InspectTopologyMethods, InspectTopology,
 $SymmetrizeTopologyMethods, SymmetrizeTopology,
 TopologyCheck,

 Null};

{GraphMapTopology,
 CompleteTopology, $CompleteTopologyMethods,
 ReduceTopology,
 CutsTopology,
 ZeroCutsTopology, $ZeroCutsTopologyMethods,
 PermuteTopology,
 HashTopology, $HashTopologyMethods,
 $PrepareTopologySimplifier,
 PrepareTopology,
 CanonicalizeIntegral,
 ClearNumeratorsTopology,
 TopologyScalefulQ,
 TopologyFactorizingQ};

Begin["`Private`"];

(* --- GraphMapTopology --------------------------------------------- *)

GraphMapTopology::nomatch = "\
Warning: Mapping may be incorrect, source \"`1`\" and topology name \
\"`2`\" do not match.";

GraphMapTopology[
  map:MappingPattern[], top:TopologyPattern["Graph"]] :=
  Module[
    {im,rm,rp,cp, ls,ps, crls, crl, c, i},

    (* check: no match *)
    If[(fr /. map) =!= (name /. top),
       Message[GraphMapTopology::nomatch, fr /. map, name /. top]];

    (* -- mappings -- *)

    (* identical mapping: relative to >pros< *)
    im = Range[Length[pros /. top]];

    (* reversed mapping: relative to >facs< *)
    rm = id /. ReverseMapping[map];

    (* lines present *)
    (*rp = Intersection[im, DeleteCases[rm, 0]];*)
    rp = DeleteCases[rm, 0];                                            (* TODO: ensure all in im? *)

    (* contracted lines *)
    cp = Complement[im, rp];

    (* -- contractions rules -- *)

    {ls, ps} = {legs, pros} /. top;

    (* vertex pairs *)
    crls = Sort[Part[#, {3, 4}]] & /@ ps[[cp]];

    (* resolve ambiguities *)
    crls = Sort[crls, First[#1] > First[#2] & ];

    (* valid chain of rules *)
    crls = Rule @@ Reverse[#] & /@ crls;

    (* -- identify vertices -- *)

    While[
      crls =!= {},

      (* act one by one *)
      crl = First[crls];

      (* also on themselves *)
      crls = Rest[crls] /. crl;

      c = MapAt[
        (* protect external vertices *)
        # /. {in[i_] -> in[i], out[i_] -> out[i], crl} & ,
        (* apply on vertices only *)
        #, {{3}, {4}}] & ;

      (* for all edges *)
      {ps, ls} = c /@ # & /@ {ps, ls}];

    (* -- compile -- *)

    (* apply mapping *)
    ps = ps[[rp]];

    (* result *)
    Topology[
      top, legs -> ls, pros -> ps,
      hist -> {"GraphMapTopology", id /. map}]];

(* trap *)
GraphMapTopology[___] :=
  (Message[GraphMapTopology::usage];
   Abort[]);

(* N.B.:
- Lines may be contracted by mappings to zero (not present in the target
  at all) or by identical mappings (multiple lines represented by a
  single one in the target).
- In the latter case at least two lines are mapped onto one, the first
  of these is left out from the contractions.
*)

(* --- LabelTopology ------------------------------------------------ *)

LabelTopology::usage = "\
LabelTopology[<top(s)>, [<set>]] rewrites particle labels in graph \
data of topology (list) <top(s)> as masses specified in setup [<set>].";

(* overload: plural *)
LabelTopology[tops_?TopologyListQ, set:Elective[SetupPattern[]]] :=
  LabelTopology[#, set] & /@ tops;

LabelTopology[
  top:TopologyPattern["Graph"], set:SetupPattern[]] :=
  Module[
    {ls,ps},

    {ls, ps} = {legs, pros} /. top;

    (* apply mass wrapper to labels *)
    {ls, ps} = Map[MapAt[
      If[Head[#] === String, m[#], #] & , #, {1}] & , {ls, ps}, {2}];

    (* apply mass assignment rules *)
    {ls, ps} = {ls, ps} /. Cases[cs /. set, HoldPattern[m[_] -> _]];

    (* result *)
    Topology[
      top, hist -> {"LabelTopology"},
      legs -> ls, pros -> ps]];

(* alias *)
LabelTopology[top:TopologyPattern["Setup"]] :=
  LabelTopology[top, setp /. top];

(* trap *)
LabelTopology[___] :=
  (Message[LabelTopology::usage];
   Abort[]);

(* --- MapToTopology ------------------------------------------------ *)

MapToTopology::usage = "\
MapToTopology[<map(s)>, <top(s)>] returns the topology (list) <top(s)> \
with the mapping(s) <map(s)> applied to them.  Meaning contraction and \
permutation of lines has beed performed to all data stored in the \
topology.";

MapToTopology::matches = "\
Warning: Found no matches for topology \"`1`\".";

MapToTopology::multi = "\
Warning: Multiple topologies named \"`1`\" found.";

MapToTopology::match = "\
Mapping may be incorrect, source \"`1`\" and topology name \"`2`\" do \
not match.";

MapToTopology::length = "\
Mapping `1` has too few elements to be applied to topology \"`2`\".";

MapToTopology::canon = "\
Warning: Permutation `1` does not seem to be canonical.";

(* overload: plural, plural *)
MapToTopology[
  maps_?MappingListQ, tops_?TopologyListQ] :=
  (* result: -> plural, singular *)
  Join @@ (MapToTopology[maps, #] & /@ tops);

(* overload: plural, singular *)
MapToTopology[
  maps_?MappingListQ, top:TopologyPattern[]] :=
  Module[
    {sel},
    (* candidate mappings *)
    sel = Select[maps, (fr /. #) === (name /. top) & ];
    (* check: no matches *)
    If[sel === {},
       Message[MapToTopology::matches, name /. top];
       Return[{}]];
    (* result: -> singular, singular *)
    MapToTopology[#, top] & /@ sel];

(* overload: singular, plural *)
MapToTopology[
  map:MappingPattern[], tops_?TopologyListQ] :=
  Module[
    {sel},
    (* candidate topologies *)
    sel = Select[tops, (fr /. map) === (name /. #) & ];
    (* check: no matches *)
    If[sel === {},
       Message[MapToTopology::matches, fr /. map];
       Return[{}]];
    (* check: multiple matches *)
    If[Length[sel] > 1,
       Message[MapToTopology::multi, fr /. map]];
    (* result: -> singular, singular *)
    MapToTopology[map, First[sel]]];

(* main: singular, singular *)
MapToTopology[
  map:MappingPattern[], top:TopologyPattern[]] :=
  Module[
    {fs, nf, pam, mp,pm, p, rls, is,ap,ar,st,zr,sy,ct, toq},

    (* check: no match *)
    If[(fr /. map) =!= (name /. top),
       Message[MapToTopology::match, fr /. map, name /. top];
       Abort[]];

    (* check: length *)
    If[Length[id /. map] < Length[facs /. top],
       Message[MapToTopology::length, id /. map, name /. top];
       Abort[]];

    (* -- mappings, permutations -- *)

    fs = facs /. top;
    nf = Length[fs];
    pam = ReverseMapping[map, nf];
    {mp, pm} = id /. {map, pam};

    (* ensure range of factors *)
    pm = Select[pm, # =!= 0 && # <= nf & ];

    (* permute line indices *)
    p = mp[[#]] & /@ # & ;

    (* -- collect modifications -- *)

    rls = First[Last[Reap[

      (* >hist< *)
      Sow[hist -> {"MapToTopology", id /. map}];

      (* >inds< *)
      If[(is = inds /. top) =!= inds,
         Sow[inds -> is[[pm]]]];

      (* >facs< *)
      Sow[facs -> fs[[pm]]];

      (* >apar<, >arep< *)
      If[({ap, ar} = {apar, arep} /. top) =!= {apar, arep},
         Sow[apar -> Take[ap, Length[pm]]];
         Sow[arep -> ar /. PermuteRules[pm, ap]]];

      (* >subt< *)
      If[(st = subt /. top) =!= subt,
         If[FreeQ[st, pm],
            Message[MapToTopology::canon, pm]];
         Sow[subt -> SortGroups[p /@ SubsetIdenticalGroups[st, pm]]]];

      (* >zero< *)
      If[(zr = zero /. top) =!= zero,
         Sow[zero -> SortGroups[p @ SubsetVanishingGroup[zr, pm]]]];

      (* >symm< *)
      If[(sy = symm /. top) =!= symm,
         Sow[symm -> SortSymmetricGroups[p /@ FilterSymmetricGroups[SortSymmetricGroups[
           SubsetIdenticalGroups[sy, pm]]]]]];

      (* >cuts< *)
      If[(ct = cuts /. top) =!= cuts,
         Sow[cuts -> SortGroups[p @ SubsetCutsGroup[ct, pm]]]]

    ]]];

    (* TODO *)
    rls = Join[rls, {subt -> None, zero -> None, symm -> None, cuts -> None}];
    (* TODO *)

    (* -- result -- *)
    toq = Topology[top, rls];

    (* >legs<, >pros< *)
    If[({legs, pros} /. toq) =!= {legs, pros},
       toq = GraphMapTopology[map, toq]];

    Topology[toq, name -> to /. map]];

(* trap *)
MapToTopology[___] :=
  (Message[MapToTopology::usage];
   Abort[]);

Off[MapToTopology::canon];

(* needs: GraphMapTopology[] *)

(* N.B.:
- In general <map> is assumend to be a canonical mapping, otherwise a
  warning is printed.
- Because of this the alpha-representation may not be in unique form any
  more.
- Relations for scalar products >scps< and among factors >rels< are not
  touched at all.
*)

(* --- CanonicalizeTopology ----------------------------------------- *)

CanonicalizeTopology::usage = "\
CanonicalizeTopology[<top(s)>, [<set>]] returns the canonical mapping \
(list) and the topology (list) corresponding to the topology (list) \
<top(s)> for which the canonical alpha-representation is (re-)computed \
via the optional setup [<set>].  Already present topology data is \
reordered accordingly and not discarded.";

(* overload: plural *)
CanonicalizeTopology[tops:{}, set:Elective[SetupPattern[]]] :=
{{}, {}};

(* overload: plural *)
CanonicalizeTopology[
  tops_?TopologyListQ, set:Elective[SetupPattern[]]] :=
  Transpose[
    (Status[Console, name /. #];
     CanonicalizeTopology[#, set]) & /@ tops];

(* main: recompute *)
CanonicalizeTopology[
  top:TopologyPattern[], set:SetupPattern[]] :=
  Module[
    {ap,ar},

    (* -- alpha-representation -- *)

    {ap, ar} = {apar, arep} /. top;

    (* check: not present at all or too small *)
    If[{ap, ar} === {apar, arep} || Length[facs /. top] != Length[ap],

       (* generate alpha-parameters *)
       ap = MapIndexed[alpha @@ #2 & , facs /. top];

       (* generate U- and W-polynomials *)
       ar = AlphaRepresentation[
         Last /@ (facs /. top), ks /. set,
         Constraints -> (cs /. set),
         GeneratedParameters -> ap]];

    (* result: trigger reordering *)
    CanonicalizeTopology[Topology[top, apar -> ap, arep -> ar]]];

(* main: reorder *)
CanonicalizeTopology[
  top:TopologyPattern["Alpha"]] /;
Length[facs /. top] <= Length[apar /. top] :=
  Module[
    {ap,ar, co, map, res},

    (* -- reordering -- *)

    {ap, ar} = {apar, arep} /. top;

    (* canonical ordering of alpha-parameters *)
    co = First[PolynomialOrderings[$TopoIDMetric @@ ar, ap, 1]];

    (* reorder alpha-representation *)
    ar = ar /. PermuteRules[co, ap];

    (* -- compile -- *)

    map = ReverseMapping[Mapping[name /. top, co]];

    (* reorder remaining quantities *)
    res = MapToTopology[map, Topology[
      top, hist -> {"CanonicalizeTopology", id /. map},
      apar -> None, arep -> None]];

    (* result *)
    {map, Topology[res, apar -> ap, arep -> ar]}];

(* alias *)
CanonicalizeTopology[top:TopologyPattern["Setup"]] :=
  CanonicalizeTopology[top, setp /. top];

(* trap *)
CanonicalizeTopology[___] :=
  (Message[CanonicalizeTopology::usage];
   Abort[]);

(* needs: MapToTopology[] *)

(* N.B.:
- Order of definitions must be like this.
- MapTopology[] must be called in order to keep the correspondence
  between topology factors and alpha-parameters consistent (i.e. for
  >subt<, >zero<, etc.).
*)

(* --- InspectTopology ---------------------------------------------- *)

$InspectTopologyMethods =
{"Cuts", "Mask", "Check"};

Options[InspectTopology] =
{Method -> {"Cuts"},
 Parallel -> False,
 Verbosity -> False};
(* cf. $InspectTopologyMethods *)

InspectTopology::usage = "\
InspectTopology[<top(s)>, [<set>], [<lvl>], [opts]] returns the \
topology (list) <top(s)> with inspected sub-topology structure.
With specified setup [<set>] canonical alpha-representations are \
(re-)computed, with optional level specification [<lvl>] only \
corresponding sub-topologies are inspected.
The option [Method] expects a keyword string (list):
- \"Cuts\" -- dimiss sub-topologies not involving any cut,
- \"Mask\" -- neglect sub-topologies involving irreducible numerators,
- \"Check\" -- check the computed sub-topology structure explicitly \
before returning it.
The default is {\"Cuts\"}.";

InspectTopology::level =
  IdenticalGroups::level;

InspectTopology::cuts = "\
Warning: \"`1`\" contains no cuts as demanded via option value `2` for \
[Method].";

InspectTopology::missed = "\
Missed equivalent group(s) in \"`1`\" at position(s) `2`.";

InspectTopology::wrong = "\
Found wrong group(s) in \"`1`\" at position(s) `2`.";

(* overload: plural *)
InspectTopology[
  tops_?TopologyListQ, set:Elective[SetupPattern[]], lv_:All,
  opts:OptionsPattern[]] :=
  InspectTopology[#, set, lv, opts] & /@ tops;

(* main *)
InspectTopology[
  top:TopologyPattern["Alpha"], lv_:All,
    opts:OptionsPattern[]] :=
  Module[
    {me,pl,vb, ap,ar, nv, lw, cp, os, cts,msk, sgs,zgs, lvs,lws,
     res, cks, cws,cms},

    (* -- options -- *)

    {me, pl, vb} = OptionValue[{Method, Parallel, Verbosity}];
    me = Flatten[{me}];
    pl = pl /. $FlagRules;
    vb = vb /. VerbosityRules;
    Report[vb, 1, "InspectTopology[]."];

    {ap, ar} = {apar, arep} /. top;

    (* check: level *)
    nv = Length[ap];
    lw = lv /. If[
      Head[lv] === List,
      {All | Infinity -> nv},
      {All | Infinity -> {1, nv}, n_Integer -> {n, nv}}];
    If[!MatchQ[lw, {__Integer}] || Length[lw] > 3 ||
         !And @@ Positive /@ lw,
       Message[InspectTopology::level, lv];
       lw = {1, nv}];
    Report[vb, 2, "Level specification: ", lw, "."];

    (* check: Method *)
    cp = Complement[me, $InspectTopologyMethods];
    If[cp =!= {},
       Message[Method::invalid, cp, $InspectTopologyMethods];
       me = Complement[me, cp]];

    (* passed options *)
    os = FilterRules[{opts}, Options[IdenticalGroups]];
    PrependTo[os, Verbosity -> vb - 2];

    (* -- classification -- *)

    (* Method: "Cuts" *)
    If[MemberQ[me, "Cuts"],
       cts = cuts /. top /. cuts -> None;
       If[cts === None,
          Message[InspectTopology::cuts, name /. top, me]];
       PrependTo[os, "Cuts" -> cts];];

    (* Method: "Mask" *)
    If[MemberQ[me, "Mask"],
       msk = DenominatorSymbolQ /@ First /@ (facs /. top);
       Report[vb, 2, "Factor symbol mask: ", msk, "."];
       PrependTo[os, "Mask" -> msk]];

    (* already inspected levels *)
    {sgs, zgs} = {subt, zero} /. top /. {subt -> {}, zero -> {}};
    lvs = If[Length[lw] > 1, Range @@ lw, lw];
    If[sgs =!= {},
       lws = Union[Length /@ First /@ sgs];
       lvs = Complement[lvs, lws];
       Report[vb, 2, "Already inspected levels: ", lws, "."]];
    Report[vb, 2, "Levels to be inspected: ", lvs, "."];

    res = If[pl, ParallelMap, Map][IdenticalGroups[
      ar, ap, {#}, Sequence @@ os] & , lvs];

    (* join with known levels *)
    {sgs, zgs} = Join @@@ Transpose[Prepend[res, {sgs, zgs}]];

    (* identical groups: sort *)
    sgs = SortGroups[sgs];
    Report[vb, 2, "In total ", Length[sgs], " distinct groups."];

    (* vanishing group: find supersets, sort *)
    zgs = FilterVanishingGroup[zgs];
    Report[vb, 2, "In total ", Length[zgs], " vanishing subsets."];

    (* Method: "Check" *)
    If[MemberQ[me, "Check"],
       cks = Quiet[GroupsCheck[ar, ap, sgs]];
       If[cks =!= True,
          cws = cms = Null;
          If[MatchQ[cks, {_, _}],
             {cws, cms} = cks];
          (* check: missed groups *)
          If[cms =!= {},
             Message[InspectTopology::missed, name /. top, cms]];
          (* check: wrong groups *)
          If[cws =!= {},
             Message[InspectTopology::wrong, name /. top, cws]];
          Return[$Failed]]];

    (* result: inspected topology  *)
    Topology[
      top, hist -> {"InspectTopology", lv, opts},
      subt -> sgs, zero -> zgs]];

(* trigger: CanonicalizeTopology[] *)
InspectTopology[top:TopologyPattern[], set:SetupPattern[], lv_:All,
  opts:OptionsPattern[]] := InspectTopology[Last[
    CanonicalizeTopology[top, set]], lv, opts];

(* trigger: CanonicalizeTopology[] *)
InspectTopology[top:TopologyPattern["Setup"], lv_:All,
  opts:OptionsPattern[]] := InspectTopology[Last[
    CanonicalizeTopology[top, setp /. top]], lv, opts];

(* trap *)
InspectTopology[___] :=
  (Message[InspectTopology::usage];
   Abort[]);

Off[InspectTopology::cuts];

(* Needs:
- DenominatorSymbolQ[],
- IdenticalGroups[],
- SortGroups[], FilterVanishingGroup[],
- CanonicalizeTopology[].
*)

(* N.B.:
- Caching is performed, viz. already given present levels of groups of
  sub-topologies are assumed to be complete and correct, only remaining
  levels are inspected.
*)

(* --- CompleteTopology --------------------------------------------- TODO *)

$CompleteTopologyMethods =
{"ScalarProducts", "PlusPropagators", "MinusPropagators"};

Options[CompleteTopology] =
{Method -> {"ScalarProducts"}};  (* cf. $CompleteTopologyMethods *)

CompleteTopology::failed = "\
Cannot express all scalar products in terms of topology factors, using \
\"`1`\" as option value for [Method].  Try a different setting from `2`.";

(* overload: plural *)
CompleteTopology[
  tops_?TopologyListQ, set:Elective[SetupPattern[]],
    opts:OptionsPattern[]] :=
  Transpose[CompleteTopology[#, set, opts] & /@ tops];

CompleteTopology[
  top:TopologyPattern[], set:SetupPattern[],
    opts:OptionsPattern[]] :=
  Module[
    {a, b, f, me,cp, i, fs, sp, fa, is, ps, res},

    (* helper: massless propagator *)
    f = #2 /. a_*b_ :> a^2 + b^2 + #1*2*a*b /. (cs /. set) & ;

    (* check: method *)
    me = Flatten[{OptionValue[Method]}];
    cp = Complement[me, $CompleteTopologyMethods];
    If[cp =!= {},
       Message[Method::invalid, cp, $CompleteTopologyMethods];
       me = Method /. Options[CompleteTopology]];

    (* -- complete -- *)

    (* rename genuine propagators *)
    fs = facs /. top /. p[i_Integer] :> d[i];

    (* basis for construction *)
    sp = TopologyScalarProducts[top, set] /. s[i_Integer] :> n[i];

    (* complete set of factors *)
    fa = Which[
      (* plainly missing scalar products *)
      MemberQ[me, "ScalarProducts"], sp,
      (* simple massless propagators with "+" *)
      MemberQ[me, "PlusPropagators"], #[[1]] -> f[+1, #[[2]]] & /@ sp,
      (* simple massless propagators with "-" *)
      MemberQ[me, "MinusPropagators"], #[[1]] -> f[-1, #[[2]]] & /@ sp];

    (* -- compile -- *)

    {is, ps} = {inds, pros} /. top;

    (* >inds<: dummy indices *)
    is = If[is === inds, None, Join[is, Table[Null, {Length[fa]}]]];

    (* >pros<: dummy edge for added factors *)
    ps = If[ps === pros, None, Join[ps, {Null, #[[2]], 0, 0} & /@ fa]];

    (* result *)
    res = Topology[
      top, inds -> is, facs -> Join[fs, fa], pros -> ps,
      hist -> {"CompleteTopology", me}];

    (* check: incomplete *)
    If[!TopologyCompleteQ[res, set],
       Message[CompleteTopology::failed,
         me, $CompleteTopologyMethods];
       Return[$Failed]];

    (* mapping *)
    {Mapping[name /. top, name /. top, Length[facs /. res]],
     res}];

(* overload *)
CompleteTopology[
  top:TopologyPattern["Setup"],
    opts:OptionsPattern[]] :=
  CompleteTopology[top, setp /. top]

(* trap *)
CompleteTopology[___] :=
  (Message[CompleteTopology::usage];
   Abort[]);

(* N.B.:
- In general the alpha-representation has to be recomputed after adding
  factors to a topology (at least for comparing).
- Appending new factors to the end, thus >subt<, >zero<, >symm< and
  >cuts< (referring to positions) are left intact.
*)

(* TODO:
- "GroebnerBasis" as a Method?!
- Introduce option for a pure function acting on <fs> and <sp>?!
*)

(* --- ReduceTopology ----------------------------------------------- *)

(* overload: plural *)
ReduceTopology[
  tops_?TopologyListQ, set:Elective[SetupPattern[]]] :=
  (Status[Console, name /. #];
   ReduceTopology[#, set]) & /@ tops;

ReduceTopology[
  top:TopologyPattern[], set:SetupPattern[]] :=
  Module[
    {ss,rs},

    (* obtain inversion and relations *)
    {ss, rs} = TopologyReduce[top, set];

    (* check: fail *)
    If[ss === {},
       Return[$Failed]];

    (* result *)
    Topology[
      top, scps -> ss, rels -> rs,
      hist -> {"ReduceTopology"}]];

(* overload *)
ReduceTopology[
  top:TopologyPattern["Setup"]] :=
  ReduceTopology[top, setp /. top]

(* trap *)
ReduceTopology[___] :=
  (Message[ReduceTopology::usage];
   Abort[]);

(* --- SymmetrizeTopology ------------------------------------------- *)

$SymmetrizeTopologyMethods =
{"Cuts", "Mask", "Check"};

$SymmetrizeTopologyMethods = Union[
  $SymmetrizeTopologyMethods, $SymmetricGroupsMethods];

(* TODO:
"Symm": all canonical orderings searched
"Subt": cross-subtopology mappings allowed
"Only": keep single representations
*)

Options[SymmetrizeTopology] =
{Method -> {"Cuts", "Mask", "Symm", "Subt", "Filter", "Check"},
 Parallel -> False,
 Verbosity -> False};
(* cf. $SymmetrizeTopologyMethods *)

SymmetrizeTopology::usage = "\
SymmetrizeTopology[<top(s)>, [<set>], [<lvl>], [opts]] returns the \
topology (list) <top(s)> with inspected symmetry structure.
With specified setup [<set>] canonical alpha-representations are \
(re-)computed, with optional level specification [<lvl>] only selected \
symmetries are investigated.
The option [Method] expects a keyword string (list):
- \"Cuts\" -- dismiss symmetries not involving any cut and forbid \
transitions between different cut-structures,
- \"Mask\" -- forbid transitions between actual denominators and \
irreducible numerators,
- \"Order\" -- impose a canonic ordering on groups and within their \
subsets,
- \"Filter\" -- discard groups of subsets already contained in groups \
of supersets (implying ordered groups) and not contributing new \
symmetries.
- \"Check\" -- check the computed symmetry structure explicitly before \
returning it.
The default is {\"Cuts\", \"Mask\", \"Filter\", \"Check\"}.";

SymmetrizeTopology::cuts =
  InspectTopology::cuts;

SymmetrizeTopology::missed =
  InspectTopology::missed;

SymmetrizeTopology::wrong =
  InspectTopology::wrong;

(* overload: plural *)
SymmetrizeTopology[
  tops_?TopologyListQ, set:Elective[SetupPattern[]], lv_:All,
  opts:OptionsPattern[]] :=
  (Status[Console, name /. #];
   SymmetrizeTopology[#, set, lv, opts]) & /@ tops;

(* main *)
SymmetrizeTopology[
  top:TopologyPattern["Alpha"], lv_:All,
    opts:OptionsPattern[]] :=
  Module[
    {vb,me, ap,ar, cp, os, cts,msk, sgs,zgs, cks, cws,cms},

    (* -- options -- *)

    {me, vb} = OptionValue[{Method, Verbosity}];
    me = Flatten[{me}];
    vb = vb /. VerbosityRules;
    Report[vb, 1, "SymmetrizeTopology[]."];

    {ap, ar} = {apar, arep} /. top;

    (* check: Method *)
    cp = Complement[me, $SymmetrizeTopologyMethods];
    If[cp =!= {},
       Message[Method::invalid, cp, $SymmetrizeTopologyMethods];
       me = Complement[me, cp]];

    (* passed options *)
    os = FilterRules[{opts}, Options[SymmetricGroups]];
    os = Join[
      {Method -> Intersection[me, $SymmetricGroupsMethods],
       Verbosity -> vb - 2},
      os];

    (* -- classification -- *)

    (* Method: "Cuts" *)
    If[MemberQ[me, "Cuts"],
       cts = cuts /. top /. cuts -> None;
       If[cts === None,
          Message[SymmetrizeTopology::cuts, name /. top, me]];
       PrependTo[os, "Cuts" -> cts]];

    (* Method: "Mask" *)
    If[MemberQ[me, "Mask"],
       msk = DenominatorSymbolQ /@ First /@ (facs /. top);
       Report[vb, 2, "Factor symbol mask: ", msk, "."];
       PrependTo[os, "Mask" -> msk]];

    {sgs, zgs} = SymmetricGroups[ar, ap, lv, Sequence @@ os];

    (* Method: "Check" *)
    If[MemberQ[me, "Check"],
       cks = Quiet[GroupsCheck[ar, ap, sgs]];
       If[cks =!= True,
          cws = cms = Null;
          If[MatchQ[cks, {_, _}],
             {cws, cms} = cks];
          (* check: missed groups *)
          If[cms =!= {},
             Message[SymmetrizeTopology::missed, name /. top, cms]];
          (* check: wrong groups *)
          If[cws =!= {},
             Message[SymmetrizeTopology::wrong, name /. top, cws];
             Return[$Failed]]]];

    (* result: symmetrized topology *)
    Topology[
      top, hist -> {"SymmetrizeTopology", lv, opts},
      symm -> sgs, zero -> zgs]];

(* trigger: CanonicalizeTopology[] *)
SymmetrizeTopology[top:TopologyPattern[], set:SetupPattern[], lv_:All,
  opts:OptionsPattern[]] := SymmetrizeTopology[Last[
    CanonicalizeTopology[top, set]], lv, opts];

(* trigger: CanonicalizeTopology[] *)
SymmetrizeTopology[top:TopologyPattern["Setup"], lv_:All,
  opts:OptionsPattern[]] := SymmetrizeTopology[Last[
    CanonicalizeTopology[top, setp /. top]], lv, opts];

(* trap *)
SymmetrizeTopology[___] :=
  (Message[SymmetrizeTopology::usage];
   Abort[]);

(* Needs:
- DenominatorSymbolQ[],
- SymmetricGroups[],
- CanonicalizeTopology[].
*)

(* --- TopologyCheck ------------------------------------------------ *)

TopologyCheck::usage = "\
TopologyCheck[<top(s)>, [<set>]] applies TODO...";

TopologyCheck::missed = "\
Missed `1` group(s) in \"`2`\" at position(s) `3`.";

TopologyCheck::wrong = "\
Found wrong `1` group(s) in \"`2`\" at position(s) `3`.";

(* overload: plural *)
TopologyCheck[
  tops_?TopologyListQ, set:Elective[SetupPattern[]]] :=
  TopologyCheck[#, set] & /@ tops;

(* main *)
TopologyCheck[
  top:TopologyPattern[], set:Elective[SetupPattern[]]] :=
  Module[
    {ap,ar, igs,zgs,sgs, cts, ck, cks, cws,cms, pk},

    {ap, ar, igs, zgs, sgs, cts} = {apar, arep, subt, zero, symm, cuts} /. top;

    (* also check: {name, facs, scps, rels, ..., legs, pros, setp} *)

    (* TODO: different naming of symbols in facs *)

    (* TODO: set -> check scps, rels *)

    (* TODO: set -> Info: dependent, complete, consistent *)

    ck = First[Last[Reap[

      If[ap =!= apar && ar =!= arep,

         (* TODO: scaleful -- zero *)

         (* identical groups *)
         If[igs =!= subt,
            cks = Quiet[GroupsCheck[ar, ap, igs]];
            If[cks =!= True,
               cws = cms = Null;
               If[MatchQ[cks, {_, _}],
                  {cws, cms} = cks];
               (* account for cuts *)
               If[cts =!= cuts && cws =!= Null,
                  pk = Function[ig, Outer[
                    Intersection[#1, #2] === #2 & ,
                    ig, cts, 1]] /@ igs[[cws]];
                  pk = (Or @@ Or @@@ #) & /@ pk;
                  cws = Pick[cws, pk]];
               (* check: missed groups *)
               If[cms =!= {},
                  Message[TopologyCheck::missed,
                    "identical", name /. top, cms]];
               (* check: wrong groups *)
               If[cws =!= {},
                  Message[TopologyCheck::wrong,
                    "identical", name /. top, cws]];
               Sow[subt -> cks],
               Sow[subt -> True]]];

         (* symmetric groups *)
         If[sgs =!= symm,
            cks = Quiet[GroupsCheck[ar, ap, sgs]];
            If[cks =!= True,
             cws = cms = Null;
               If[MatchQ[cks, {_, _}],
                  {cws, cms} = cks];
               (* account for cuts *)
               If[cts =!= cuts && cws =!= Null,
                  pk = Function[sg, Outer[
                    Intersection[#1, #2] === #2 & ,
                    sg, cts, 1]] /@ sgs[[cws]];
                  pk = (Or @@ Or @@@ #) & /@ pk;
                  cws = Pick[cws, pk]];
               (* check: missed groups *)
               If[cms =!= {},
                  Message[TopologyCheck::missed,
                    "symmetric", name /. top, cms]];
               (* check: wrong groups *)
               If[cws =!= {},
                  Message[TopologyCheck::wrong,
                    "symmetric", name /. top, cws]];
               Sow[symm -> {cws, cms}],
               Sow[symm -> True]]]

         (* TODO: graph -> check cuts *)

       ]]]];

    Print[And @@ (TrueQ /@ Last /@ ck)];

    ck];

(* trap *)
TopologyCheck[___] :=
  (Message[TopologyCheck::usage];
   Abort[]);

(* TODO:

- overload "Setup"

- add check for momenta in graph

*)

(* --- CutsTopology ------------------------------------------------- *)

Options[CutsTopology] =
  Options[Cuts];

(* overload: plural *)
CutsTopology[tops_?TopologyListQ, opts:OptionsPattern[]] :=
  (Status[Console, name /. #];
   CutsTopology[#, opts]) & /@ tops;

CutsTopology[
  top:TopologyPattern["Graph"],
    opts:OptionsPattern[]] :=
  Module[
    {cs},

    (* obtain cuts *)
    cs = Cuts[top, opts];

    (* result *)
    Topology[
      top, cuts -> cs,
      hist -> {"CutsTopology", opts}]];

(* trap *)
CutsTopology[___] :=
  (Messsage[CutsTopology::usage];
   Abort[]);

(* --- ZeroCutsTopology --------------------------------------------- TODO *)

$ZeroCutsTopologyMethods =
{"Keep"};

Options[ZeroCutsTopology] =
{Method -> {"Keep"}};  (* cf. $ZeroCutsTopologyMethods *)

ZeroCutsTopology::nomatch = "\
`1` does not match the pattern for vanishing subsets of factors.";

(* overload: plural *)
ZeroCutsTopology[
  tops_?TopologyListQ,
    opts:OptionsPattern[]] :=
  ZeroCutsTopology[#, opts] & /@ tops;

ZeroCutsTopology[
  top:TopologyPattern[],
    opts:OptionsPattern[]] :=
  Module[
    {me,cp, fs, l,ls, zo,cs, i, res},

    (* check: method *)
    me = Flatten[{OptionValue[Method]}];
    cp = Complement[me, $ZeroCutsTopologyMethods];
    If[cp =!= {},
       Message[Method::invalid, cp, $ZeroCutsTopologyMethods];
       me = Method /. Options[ZeroCutsTopology]];

    fs = Range[Length[facs /. top]];

    (* temporary symbols:
    l[<i>] means factor <i> present, i.e. with power < 0 *)
    ls = l /@ fs;

    {zo, cs} = {zero, cuts} /. top /. {zero -> {}, cuts -> {}};

    (* -- combine information -- *)

    (* scaleless condition (invert notation):
    only those in a group -> none of the others *)
    zo = Complement[ls, ls[[#]]] & /@ zo;
    (*zo = Or @@ (And @@ Not /@ # & /@ zo);*)
    zo = Or @@ (Not[Or @@ #] & /@ zo);

    (* cutless condition:
    not one group with all of them *)
    cs = ls[[#]] & /@ cs;
    (*cs = And @@ (Or @@ Not /@ # & /@ cs);*)
    cs = And @@ (Not[And @@ #] & /@ cs);

    (* combine: no scale or cut *)
    zo = BooleanMinimize[zo || cs];

    (* -- change notation -- *)

    (* boolean expression -> lists of integers *)
    zo = If[Head[zo] === Or, List @@ zo, List @ zo];
    zo = If[Head[#] === And, List @@ #, List @ #] & /@ zo;
    zo = zo /. Not[l[i_Integer]] -> i;

    (* TODO: handle zo = {{True}} case *)

    (* check: disjunctive normal form *)
    If[!MatchQ[zo, {{___Integer}...}],
       Message[ZeroCutsTopology::nomatch, zo];
       Abort[]];

    (* sort *)
    zo = Sort[Sort /@ zo];

    (* invert notation again *)
    zo = Complement[fs, #] & /@ zo;

    (* -- result -- *)

    res = {zero -> zo};

    (* option: method *)
    If[FreeQ[me, "Keep"],
       AppendTo[res, cuts -> None]];

    Topology[
      top, res,
      hist -> {"ZeroCutsTopology", opts}]];

(* trap *)
ZeroCutsTopology[___] :=
  (Messsage[ZeroCutsTopology::usage];
   Abort[]);

(* TODO:

- Handle case of top. w/o >cuts< (pass it trough, as well case of no
  >zero<).
-> Still to test.

*)

(* --- PermuteTopology ---------------------------------------------- *)   (* TODO: clean, document diff. to MapToTopology, ... *)

(* MapToTop. doesn't touch symm, subt, zero, but discards them *)

PermuteTopology::noperm = "\
`1` is no valid permutation list.";

(* overload: plural, plural *)
PermuteTopology[
  tops_?TopologyListQ, pms:{___List}] /; Length[tops] === Length[pms] :=
  Transpose[MapThread[PermuteTopology, {tops, pms}]];

(* overload: plural, singular *)
PermuteTopology[
  tops_?TopologyListQ, pm:{___Integer}] /; PermutationListQ[pm] :=
  Transpose[PermuteTopology[#, pm] & /@ tops];

(* main *)
PermuteTopology[
  top:TopologyPattern[], pm:{___Integer}] /; PermutationListQ[pm] :=
  Module[
    {ip, keys, is, fs, st,sm, zo,cs, ps, vals, rls,          ap, ar},

    ip = InversePermutation[pm];

    (* all needed keys *)
    keys = {inds, facs, subt,symm, zero,cuts, pros,    apar, arep};

    (* corresponding values *)
    {is, fs, st,sm, zo,cs, ps,               ap, ar} = keys /. top;

    (* -- permute -- *)

    (* >inds< *)
    is = If[is === inds, None, is[[pm]]];

    (* >facs< *)
    fs = fs[[pm]];

    (* >subt<, >symm< *)
    st = If[st === subt, None, ip[[#]] & /@ # & /@ st];
    sm = If[sm === symm, None, ip[[#]] & /@ # & /@ sm];

    (* >zero<, >cuts< *)
    zo = If[zo === zero, None, ip[[#]] & /@ zo];
    cs = If[cs === cuts, None, ip[[#]] & /@ cs];

    (* >pros< *)
    ps = If[ps === pros, None, ps[[pm]]];




    (* >apar<, >arep< *)
    If[ap === apar || ar === arep,
       ap = ar = None,
       ar = ar /. PermuteRules[pm, ap]];


    (* -- compile -- *)

    (* replacement rules *)
    vals = {is, fs, st,sm, zo,cs, ps,          ap, ar};
    rls = MapThread[#1 -> #2 &, {keys, vals}];

    (* mapping, result *)
    {Mapping[
      name /. top, name /. top, ip],
     Topology[
       top, rls,
       hist -> {"PermuteTopology", pm}]}];

(* trap: no permutation *)
PermuteTopology[top:TopologyPattern[], pm_] :=
  (Message[PermuteTopology::noperm, pm];
   Abort[]);

(* trap *)
PermuteTopology[___] :=
  (Message[PermuteTopology::usage];
   Abort[]);

(* N.B.:
- Beware: The meaning of <pm> is reversely to >id< of a mapping.
  No, it is not!?
*)

(* --- HashTopology ------------------------------------------------- *)

$HashTopologyMethods =
{"Adler32", "CRC32", "MD2", "MD5", "SHA", "SHA256", "SHA384", "SHA512"};

Option[HashTopology] =
{Method -> {"MD5"}};

HashTopology[
  top:TopologyPattern["Alpha"], set:SetupPattern[],
    opts:OptionsPattern[]] :=
  Module[
    {me},

    me = OptionValue[Method];

    (* relevant objects *)
    res = {inds, arep} /. top;

    (* TODO *)

    Hash[res, me]];

(* overload *)
HashTopology[
  top:TopologyPattern["Setup"]] :=
  HashTopology[top, setp /. top]

(* trap *)
HashTopology[___] :=
  (Message[HashTopology::usage];
   Abort[]);

(* N.B.:
- >apar< must be replaced in >arep<.
- >facs< should be in canonical order -> iterate through them:
  * first replace variables from >ms<,
  * then replace variables from >xs< (dependent on order in setup!)
*)

(* TODO:
- Check that object was canonicalized before.
- Check option [Method].

*)

(* --- PrepareTopology ---------------------------------------------- *)

(* TODO: multivariate? *)
$PrepareTopologySimplifier[x_] :=                                            (* TODO: maybe need to use FixedPoint[...] or Fold[...]? *)
(*Collect[Factor[#1 /. (rs /. #2)], First[xs /. #2], Factor] /. (rs /. #2) & ;*)
  Factor[Collect[Factor[#1], First[xs /. #2], Function[f, f /. FORMDenominatorRule[x]]]] & ;

ClearAll[PrepareTopology];

Options[PrepareTopology] =
{Exclude -> {Global`p3 | Global`p4 -> _},        (* _ | {___} *)
 Method -> Inherit,                (* cf. MethodRules *)
 Naming -> Inherit,                (* cf. NamingRules *)
 Simplifier -> $PrepareTopologySimplifier[Global`x]};  (* _Function[_, SetupPattern[]] *)

PrepareTopology::name = "\
Warning: The topology name \"`1`\" cannot be used in FORM.";

PrepareTopology::consistent = "\
Topology \"`1`\" failed the consistency check with the setup \
definition.  One should check whether all appearing scalar products \
and constants have been properly declared via `2` and `3`.";

PrepareTopology::complete = "\
Topology \"`1`\" failed the completeness check with the setup \
definition.  The factors `2` are not sufficient in order to reexpress \
all appearing scalar products `3`.  Use CompleteTopology[] to add the \
needed additional factors to the topology.";

PrepareTopology::dependent = "\
Warning: Topology \"`1`\" contains `2` linearly dependent factor(s), \
partial fractioning relations are generated."

PrepareTopology::factorized = "\
Warning: Partial fractioning relations for \"`1`\" contain \
unfactorized denominator(s) `2`.";


(* TODO *)
(* overload: plural *)
PrepareTopology[
  tops_?TopologyListQ, set:Elective[SetupPattern[]],
  opts:OptionsPattern[]] :=
  (Status[Console, name /. #];
   PrepareTopology[#, set, opts]) & /@ tops;

PrepareTopology[
  top:TopologyPattern["Solve"], set:SetupPattern[],
    opts:OptionsPattern[]] :=
  Module[
    {nm,fs,ss,rl, nd,pf,
     denCheck, dcs},

    {ex, me, ng, sy} = OptionValue[{Exclude, Method, Naming, Simplifier}];

    {nm, fs, ss,rl} = {name, facs, scps,rels} /. top;

    (* -- check: name -- *)

    If[!StringMatchQ[nm, RegularExpression["[A-Za-z][\\w]*"]],
       Message[PrepareTopology::name,
         nm]];

    (* -- checks: consistent, complete -- *)

    (*Check[
      If[!TopologyConsistentQ[top, set],
         Message[PrepareTopology::consistent,
           nm, First /@ (ss /. set), xs /. set]];
      If[!TopologyCompleteQ[top, set],
         Message[PrepareTopology::complete,
           nm, First /@ fs, First /@ (ss /. set)]],
      Abort[]];*)                                                       (* TODO: control via option *)

    (* -- partial fractioning -- *)

    nd = Length[fs] - TopologyRank[top, set];
    If[nd > 0,
       Message[PrepareTopology::dependent,
         name /. top, nd];
       pf = PartialFractioning[rl, First /@ fs, xs /. set];
       If[pf =!= $Failed,
          AppendTo[rl, pf]]];



    (* -- integrate setup -- *)

    sset = Fold[DeleteCases[#1, #2, {0, Infinity}] & , set, ex];



    (* -- change notation -- *)

    fs = fs //. (rs /. sset) /. {x_[i_Integer] /; MemberQ[{s, n, d}, x] :>
                                   Symbol[ToString[x] <> ToString[i]],
                p[i_Integer] :> Symbol["d" <> ToString[i]]};

    fs = sy[#, sset] & /@ # & /@ fs;

    ss = ss //. (rs /. sset) /. {x_[i_Integer] /; MemberQ[{s, n, d}, x] :>
                  Symbol[ToString[x] <> ToString[i]],
                p[i_Integer] :> Symbol["d" <> ToString[i]]};

    ss = sy[#, sset] & /@ # & /@ ss;

    rl = rl //. (rs /. sset) /. {x_[i_Integer] /; MemberQ[{s, n, d}, x] :>
                                   Symbol[ToString[x] <> ToString[i]],
                                 p[i_Integer] :> Symbol["d" <> ToString[i]]};

    (* helper: check if denominators in # are free of sums *)
    denCheck = And @@ Function[a, FreeQ[Denominator[a], Plus]] /@
      (If[Head[#] === Plus, List @@ #, List @ #]
       /. FORMSymbol[x_] -> FORMSymbol) & ;

    If[Length[rl] > 1,
       rl[[-1]] = sy[#, sset] & /@ # & /@ rl[[-1]];
       dcs = denCheck /@ Last /@ rl[[-1]];
       If[!(And @@ dcs),
          Message[PrepareTopology::factorized,
            name /. top, Pick[rl[[-1]], Not /@ dcs]]]];

    rl = rl /. {x_[i_Integer] /; MemberQ[{s, n, d}, x] :> Symbol[ToString[x] <> ToString[i]],
                p[i_Integer] :> Symbol["d" <> ToString[i]]};


    sset = sset /. {x_[i_Integer] /; MemberQ[{s, n, d}, x] :>
                  Symbol[ToString[x] <> ToString[i]],
                p[i_Integer] :> Symbol["d" <> ToString[i]]};



    (* -- result -- *)

    Topology[
      top,
      facs -> fs,
      scps -> ss,
      rels -> rl,
      setp -> sset,
      hist -> {"PrepareTopology", opts}]];

(* trigger: "Solve" *)                                                  (* TODO: check *)
PrepareTopology[
  top:TopologyPattern[], set:SetupPattern[],
    opts:OptionsPattern[]] :=
  PrepareTopology[ReduceTopology[top, set], set, opts];

(* overload *)
PrepareTopology[
  top:TopologyPattern["Setup"],
    opts:OptionsPattern[]] :=
  PrepareTopology[top, setp /. top, opts];

(* trap *)
PrepareTopology[___] :=
  (Message[PrepareTopology::usage];
   Abort[]);

Off[PrepareTopology::dependent];

(* N.B.:
- Tasks:
  * perform consistency and completeness checks,
  * generate partial fractioning relations,
  * apply rules and change to FORM notation (name, symbols),
  * integrate setup (selected) information.
*)

(* TODO:
- trigger SortTop?
- trigger ZeroCuts...?
- trigger Cuts..., Symm...?
*)

(* --- CanonicalizeIntegral ----------------------------------------- TODO *)

CanonicalizeIntegral::usage = "\
CanonicalizeIntegral[<int(s)>, [<set>]] returns the canonical mapping \
(list) and the integral (list) corresponding to the integral (list) \
<int(s)> for which the canonical alpha-representation is (re-)computed \
via the optional setup [<set>].  Already present integral data is \
reordered accordingly and not discarded.";

(* overload: plural *)
CanonicalizeIntegral[
  tops_?IntegralListQ, set:Elective[SetupPattern[]]] :=
  Transpose[CanonicalizeIntegral[#, set] & /@ tops];

CanonicalizeIntegral[
  int:IntegralPattern[], set:Elective[SetupPattern[]]] :=
  Module[
    {i = 0, con, top, can,
     pos, ins, fcs},

    (* contraction mapping *)
    con = If[# === 0, 0, ++i] & /@ (inds /. int);
    con = Mapping[name /. int, name /. int, con];

    (* apply mapping *)
    top = MapToTopology[con, int];

    (* canonical form *)
    {can, top} = CanonicalizeTopology[top, set];

    (* chose minimal ordering for the indices *)
    pos = PolynomialOrderings[$TopoIDMetric @@ (arep /. top), apar /. top];
    ins = First[Sort[(inds /. top)[[#]] & /@ pos]];

    (* rename factor symbols *)
    fcs = MapThread[
      If[#1 < 0, n, d] @@ First[#2] -> Last[#2] & ,
      {ins, facs /. top}];

    (* result: combine mappings *)
    {ComposeMapping[con, can],
     Integral[top, inds -> ins, facs -> fcs]}];

(* overload *)
CanonicalizeIntegral[
  int:IntegralPattern["Setup"]] :=
  CanonicalizeIntegral[int, setp /. int];

(* trap *)
CanonicalizeIntegral[___] :=
  (Message[CanonicalizeIntegral::usage];
   Abort[]);

(* --- ClearNumeratorsTopology -------------------------------------- *)

(* overload: plural *)
ClearNumeratorsTopology[
  tops_?TopologyListQ] :=
  Transpose[ClearNumeratorsTopology /@ tops];

ClearNumeratorsTopology[
  top:TopologyPattern[]] :=
  Module[
    {f, map},
    map = Position[First /@ (facs /. top), f_?DenominatorSymbolQ];
    map = Mapping[name /. top, name /. top, Flatten[map]];
    map = ReverseMapping[map, Length[facs /. top]];
    {map, MapToTopology[map, top]}];

(* trap *)
ClearNumeratorsTopology[___] :=
  (Message[ClearNumeratorsTopology::usage];
   Abort[]);

(* --- TopologyScalefulQ -------------------------------------------- *)

(* overload: plural *)
TopologyScalefulQ[
  tops_?TopologyListQ, set:Elective[SetupPattern[]]] :=
  TopologyScalefulQ[#, set] & /@ tops;

TopologyScalefulQ[
  top:TopologyPattern["Alpha"]] :=
  Module[
    {tmp = ClearNumeratorsTopology[top][[2]]},
    ScalefulQ[$TopoIDScale @@ (arep /. tmp), apar /. tmp]];

(* trigger: CanonicalizeTopology[] *)
TopologyScalefulQ[
  top:TopologyPattern[], set:SetupPattern[]] :=
  TopologyScalefulQ[CanonicalizeTopology[top, set]];

(* trigger: CanonicalizeTopology[] *)
TopologyScalefulQ[
  top:TopologyPattern["Setup"]] :=
  TopologyScalefulQ[CanonicalizeTopology[top, setp /. top]];

(* trap *)
TopologyScalefulQ[___] :=
  (Message[TopologyScalefulQ::usage];
   Abort[]);

(* --- TopologyFactorizingQ ----------------------------------------- *)

(* overload: plural *)
TopologyFactorizingQ[
  tops_?TopologyListQ, set:Elective[SetupPattern[]]] :=
  TopologyFactorizingQ[#, set] & /@ tops;

TopologyFactorizingQ[
  top:TopologyPattern["Alpha"]] :=
  Head[Factor[First[arep /. top]]] === Times;

(* trigger: CanonicalizeTopology[] *)
TopologyFactorizingQ[
  top:TopologyPattern[], set:SetupPattern[]] :=
  TopologyFactorizingQ[CanonicalizeTopology[top, set]];

(* trigger: CanonicalizeTopology[] *)
TopologyFactorizingQ[
  top:TopologyPattern["Setup"]] :=
  TopologyFactorizingQ[CanonicalizeTopology[top, setp /. top]];

(* trap *)
TopologyFactorizingQ[___] :=
  (Message[TopologyFactorizingQ::usage];
   Abort[]);

(* --- PrepareTopology ---------------------------------------------- *)

(* TODO:
- check if rules terminate where applied
- option to rename "3l..." tops.!
*)

MethodRules[PrepareTopology] =
{                                                                       (* Key -> _Function[Topology[]] -> {___Rule} *)  (* rules applied once! *)
     Inherit -> ({x_[i_Integer] /; MemberQ[{s, n, d}, x] :>
                      Symbol[ToString[x] <> ToString[i]],
                  p[i_Integer] :> Symbol["d" <> ToString[i]]} & ),

     (*Inherit[s_String],*)

     Iterate ->
         (MapIndexed[#1 -> Symbol["d" <> ToString[#2[[1]]]] & ,
                     First /@ (facs /. #)] & ),

     (*Iterate[s_String]*)

     x_ :> (Message[PrepareTopology::method, x]; {} & )
};

PrepareTopology::invalid = "\
`1` is no valid symbol denoting a topology factor.";

PrepareTopology::method = "\
`1` is no valid method to generate symbols for topology factors.  Valid
methods are: TODO";                                                     (* TODO *)

(*PrepareTopology[itop:Topology[], iset:Setup[], opts:OptionsPattern[]] :=
    Module[{ex,me,ng,sy,vb, mset, nm,sr, rl,fs,pf, st,zr, oset, map, otop},

           (* -- options -- *)

           {ex, me, ng, sy, vb} = OptionValue[
               {Exclude, Method, Naming, Simplifier, Verbosity}];

           ex = Flatten[{ex}];

           me = me /. MethodRules[PrepareTopology];

           (* -- notation -- *)

           (* exclude entries *)
           mset = Fold[DeleteCases[#1, #2, {0, Infinity}] & , iset, ex];

           (* rename: topology, factors *)
           nm = ng[itop];
           sr = me[itop];

           (* apply renaming *)
           {fs, rl} = {facs /. itop, rl} /. sr;

           (* change notation *)
           {fs, rl} = sy[#, mset] & /@ {fs, rl};

           (* -- compile: new setup *)

           (* changed settings *)
           oset = {ss ->  (* changed notation *)
                       (ss /. mset /. sr),
                   is ->  (* changed notation *)
                       (is /. mset /. sr),
                   xs ->  (* apply transformations *)
                       (xs /. mset //. (rs /. mset)),                   (* TODO: Union[...]? *)
                   cs ->  (* discard mass assignments *)
                       DeleteCases[cs /. mset //. (rs /. mset),
                                   m[_] -> _]};

           (* create rules *)
           oset = Rule[First[#] -> _, #] & /@ oset;

           (* integrate changes *)
           oset = mset /. oset;

           (* -- compile: mapping, new topology -- *)

           otop = {name -> nm,
                   facs -> fs,
                   rels -> rl,
*)
(* TODO: integrate setup -> later in external function? *)




(* TODO: note to remind my self --

Naming -> rename topology itself, should be self consistent if also applied to mappings.

Method -> rename topology factors.

*)






















(* --- package end -------------------------------------------------- *)

Protect["TopoID`Object`*"];

Scan[
  SetAttributes[#, {ReadProtected}] & ,
  Select[Symbol /@ Names["TopoID`Object`*"], Head[#] === Symbol & ]];

End[];

EndPackage[];

(* ------------------------------------------------------------------ *)

(* --- TODO:

-

*)
