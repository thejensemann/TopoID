(* -- "Super.m": Set Operations on Topologies ----------------------- *)

(* --- provided functions:

TopologyIntersections, TopologyIntersection
--

TopologyComplement
--

*)

(* --- package begin ------------------------------------------------ *)

Unprotect["TopoID`Super`*"];

ClearAll["TopoID`Super`*", "TopoID`Super`Private`*"];

BeginPackage[
  "TopoID`Super`",
  {"TopoID`Common`",
   "TopoID`Setup`",
   "TopoID`Topology`",
   "TopoID`Mapping`",
   "TopoID`Polynomial`",
   "TopoID`Object`"}];

{TopologyIntersections, TopologyIntersection,
 TopologyIntersectionsMapping,
 TopologyIntersectionMappings, TopologyIntersectionMapping,
 TopologyComplements, TopologyComplement,
 TopologyComplementMappings, TopologyComplementMapping,

 (*TopologyUnion, TopologyUnionMapping,*)
 UnionTopology};

Begin["`Private`"];

(* --- TopologyIntersections ... ------------------------------------ *)

TopologyIntersections::usage = "\
TODO";

(* main *)
TopologyIntersections[
  src:TopologyPattern["Alpha", "Subt"],
    trg:TopologyPattern["Alpha", "Subt"]] :=
  Module[
    {sp,sr,ss, tp,tr,ts, t, sl, tq, s, sq, res, end},
    (* source, target: alpha-representation, sub-topologies *)
    {sp, sr, ss} = {apar, arep, subt} /. src;
    {tp, tr, ts} = {apar, arep, subt} /. trg;
    (* unique representatives *)
    {ss, ts} = First /@ # & /@ {ss, ts};
    (*{ss, ts} = SortBy[#, -Length[#] & ] & /@ {ss, ts};*)
    (* loop over target candidates *)
    Do[
      (* source: candidate sub-topologies *)
      sl = Select[ss, Length[#] === Length[t] & ];
      (* check: none found *)
      If[sl === {},
         Continue[]];
      (* target: contract representation *)
      tq = tr /. PermuteRules[t, tp];
      (* loop over source candidates *)
      Do[
        (* source: contract representation *)
        sq = sr /. PermuteRules[s, sp];
        (* check: match found *)
        If[Expand[tq - sq] === {0, 0},
           res = {s, t};
           Print[res];(*Goto[end]*)],
        {s, sl}],
      {t, ts}];
    (* default: nothing found *)
    Return[{{}, {}}];
    (* result: common subsets *)
    Label[end];
    res];








TopologyIntersections[
  src:TopologyPattern["Alpha"], trg:TopologyPattern["Alpha"]] :=
  Module[
    {cutf, canon, comp, aps,ars, apt,art, l, res, set, j, sbs,tbs, tmp},

    cutf = Function[{ss, cs}, Select[ss, Or @@ (Function[c, Intersection[#, c] === c] /@ cs) & ]];

    {aps, ars} = {apar, arep} /. src;
    {apt, art} = {apar, arep} /. trg;

    comp[s1_, s2_] := Module[
      {r1, r2},
      r1 = $CanonGroup[ars, aps, s1];
      r2 = $CanonGroup[art, apt, s2];
      If[r1 =!= 0 && r1[[1]] === r2[[1]],
         Sow[{r1[[2]], r2[[2]]}]];
      Null];

    l = Min[Length /@ {aps, apt}];

    res = {};

    Do[
      sbs = Subsets[Range[Length[aps]], {j}];
      If[(cuts /. src) =!= cuts, subs = cutf[sbs, cuts /. src]];
      tbs = Subsets[Range[Length[apt]], {j}];
      If[(cuts /. trg) =!= cuts, tbs = cutf[tbs, cuts /. trg]];
      tmp = Join @@ Outer[List, sbs, tbs, 1];
      res = Reap[comp[#[[1]], #[[2]]] & /@ tmp];
      If[res[[2]] =!= {}, Break[]];

      , {j, Reverse[Range[l]]}];
    If[res === Null, Return[{{}, {}}]];
    res];















(* trap *)
TopologyIntersections[___] :=
  (Message[TopologyIntersections::usage];
   Abort[]);

(*
- >subt< sorted by length, thus finds the maximum subset.
- By construction no more than one common subset. Explain! Catch?
- Triggers for "Alpha", "Subt"?
*)

(* -- *)

TopologyIntersection::usage = "\
TODO";

(* main *)
TopologyIntersection[
  src:TopologyPattern[], trg:TopologyPattern[]] :=
  First[TopologyIntersections[src, trg]];

(* trap *)
TopologyIntersection[___] :=
  (Message[TopologyIntersection::usage];
   Abort[]);

(* needs: TopologyIntersections[] *)

(* --- TopologyIntersectionsMapping ... ----------------------------- *)

TopologyIntersectionsMapping::usage = "\
TODO";

(* main *)
TopologyIntersectionsMapping[
  src:TopologyPattern[], trg:TopologyPattern[]] :=
  Module[
    {tmp, smap,tmap},
    (* common subsets *)
    tmp = TopologyIntersections[src, trg];
    (* mapping: source -> subset *)
    smap = ReverseMapping[Mapping["subset", src, tmp[[1]]], src];
    (* mapping: subset -> target *)
    tmap = Mapping["subset", trg, tmp[[2]]];
    (* result: composition *)
    ComposeMapping[smap, tmap]];

(* trap *)
TopologyIntersectionsMapping[___] :=
  (Message[TopologyIntersectionsMapping::usage];
   Abort[]);

(* needs: TopologyIntersections[] *)

(* -- *)

TopologyIntersectionMappings::usage = "\
TODO";

(* main *)
TopologyIntersectionMappings[
  src:TopologyPattern[], trg:TopologyPattern[]] :=
  Module[
    {tmp, smap,tmap},
    (* common subsets *)
    tmp = TopologyIntersections[src, trg];
    (* mapping: source -> subset *)
    smap = ReverseMapping[Mapping[src, tmp[[1]]], src];
    (* mapping: target -> subset *)
    tmap = ReverseMapping[Mapping[trg, tmp[[2]]], trg];
    (* result: both mappings *)
    {smap, tmap}];

(* trap *)
TopologyIntersectionMappings[___] :=
  (Message[TopologyIntersectionMappings::usage];
   Abort[]);

(* needs: TopologyIntersections[] *)

(* -- *)

TopologyIntersectionMapping::usage = "\
TODO";

(* main *)
TopologyIntersectionMapping[
  src:TopologyPattern[], trg:TopologyPattern[]] :=
  Module[
    {tmp},
    (* common subset *)
    tmp = TopologyIntersection[src, trg];
    (* result: source -> subset mapping *)
    ReverseMapping[Mapping[src, tmp], src]];

(* trap *)
TopologyIntersectionMapping[___] :=
  (Message[TopologyIntersectionMapping::usage];
   Abort[]);

(* needs: TopologyIntersection[] *)

(* --- TopologyComplements ... -------------------------------------- *)

TopologyComplements::usage = "\
TODO";

(* main *)
TopologyComplements[
  src:TopologyPattern["Subt"], trg:TopologyPattern["Subt"]] :=
  Module[
    {tmp, scom,tcom},
    (* common subsets *)
    tmp = TopologyIntersections[src, trg];
    (* result: complements *)
    scom = Complement[Part[subt /. src, 1, 1], tmp[[1]]];
    tcom = Complement[Part[subt /. trg, 1, 1], tmp[[2]]];
    {scom, tcom}];

(* trap *)
TopologyComplements[___] :=
  (Message[TopologyComplements::usage];
   Abort[]);

(* needs: TopologyIntersections[] *)

(* -- *)

TopologyComplement::usage = "\
TODO";

(* main *)
TopologyComplement[
  src:TopologyPattern["Subt"], trg:TopologyPattern[]] :=
  Module[
    {tmp},
    (* common subset *)
    tmp = TopologyIntersection[src, trg];
    (* result: complement *)
    Complement[Part[subt /. src, 1, 1], tmp]];

(* trap *)
TopologyComplement[___] :=
  (Message[TopologyComplement::usage];
   Abort[]);

(* needs: TopologyIntersection[] *)

(* --- TopologyComplementMappings ... ------------------------------- *)

TopologyComplementMappings::usage = "\
TODO";

TopologyComplementMappings[
  src:TopologyPattern[], trg:TopologyPattern[]] :=
  Module[
    {tmp, smap,tmap},
    (* mutually exclusive subsets *)
    tmp = TopologyComplements[src, trg];
    (* mapping: source -> subset *)
    smap = ReverseMapping[Mapping[src, tmp[[1]]], src];
    (* mapping: target -> subset *)
    tmap = ReverseMapping[Mapping[trg, tmp[[2]]], trg];
    (* result: both mappings *)
    {smap, tmap}];

TopologyComplementMappings[___] :=
  (Message[TopologyComplementMappings::usage];
   Abort[]);

(* needs: TopologyComplements[] *)

(* -- *)

TopologyComplementMapping::usage = "\
TODO";

TopologyComplementMapping[
  src:TopologyPattern[], trg:TopologyPattern[]] :=
  Module[
    {tmp},
    (* mutually exclusive subset *)
    tmp = TopologyComplement[src, trg];
    (* result: source -> subset mapping *)
    ReverseMapping[Mapping[src, tmp], src]];

TopologyComplementMapping[___] :=
  (Message[TopologyComplementMapping::usage];
   Abort[]);

(* needs: TopologyComplement[] *)

(* --- -------------------------------------------------------------- *)





(* --- UnionTopology ------------------------------------------------ *)

Options[UnionTopology] =
  Options[TopologyMomentaShifts];

UnionTopology::usage = "\
TODO";

UnionTopology::subset = "\
No subset common to \"`1`\" and \"`2`\" found.";

UnionTopology::element = "\
Warning: \"`1`\" already contains \"`2`\" as subset.";

UnionTopology::shifts = "\
No momenta shifts relating subsets of \"`1`\" and \"`2`\" found.";

(* main *)
UnionTopology[
  src:TopologyPattern["Alpha"],
    trg:TopologyPattern["Alpha"],
    set:SetupPattern[], opts:OptionsPattern[]] :=
  Module[
    {sis,tis, sst,tst, ssto,sim,usr, tsto,tim,utr, shft, end, scp, scm,
     csr, tfs,cfs, i, is, cf, cfe},

Print["Searching subset..."];

    {sis, tis} = TopologyIntersections[src, trg];

    (* check: no common subset *)
    If[sis === {{}, {}},
       Message[UnionTopology::subset, name /. src, name /. trg];
       Return[$Failed]];

    (* check: source contains target *)
    If[Length[sis] === Length[facs /. src],
       Message[UnionTopology::element, name /. src, name /. trg];
       Return[src]];

    (* check: target contains source *)
    If[Length[tis] === Length[facs /. trg],
       Message[UnionTopology::element, name /. trg, name /. src];
       Return[trg]];

    (*sst = Select[subt /. src, #[[1]] === sis & , 1][[1]];*)
    (*tst = Select[subt /. trg, #[[1]] === tis & , 1][[1]];*)

Print[{sis, tis}];

Print["Groups 1..."];

    sst = IdenticalGroups[arep /. src, apar /. src, All(*{Length[sis]}*)][[1]];
Print[sst];
    sst = Select[sst, Or @@ (Function[s, Intersection[s, sis] === sis] /@ #) & , 1][[1]];

Print["Groups 2..."];

    tst = IdenticalGroups[arep /. trg, apar /. trg, All(*{Length[tis]}*)][[1]];
    tst = Select[tst, MemberQ[#, tis] & , 1][[1]];

    While[
      sst =!= {},

      ssto = First[sst];
      sst = Rest[sst];

      sim = ReverseMapping[Mapping[src, ssto], src];
      usr = MapToTopology[sim, src];

      While[
        tst =!= {},

        tsto = First[tst];
        tst = Rest[tst];

        tim = ReverseMapping[Mapping[trg, tsto], trg];
        utr = MapToTopology[tim, trg];
Print["Shifts..."];
        shft = TopologyMomentaShifts[usr, utr, set, opts];

        If[shft =!= {},
           Goto[end]]
      ]];

    Message[UnionTopology::shifts, name /. src, name /. trg];
    Return[$Failed];

    Label[end];

    scp = Complement[Range[Length[apar /. src]](*Part[subt /. src, 1, 1]*), ssto];
    scm = ReverseMapping[Mapping[src, scp], src];

    csr = MapToTopology[scm, src];

    tfs = First /@ (facs /. trg);
    cfs = First /@ (facs /. csr);

    is = Cases[Union[tfs, cfs], (p | d)[i_] -> i];
    is = Complement[Range[Length[tfs] + Length[cfs]], is];

    cfs = Reap[Do[
      If[MemberQ[tfs, cf],
         Sow[cf /. x_[_] -> x[First[is]]];
         is = Rest[is],
         Sow[cf]],
      {cf, cfs}]];
    cfs = First[Last[cfs]];

    cfe = Expand[Last /@ (facs /. csr) /. shft] /. (cs /. set);

    Topology[
      trg, facs -> Join[facs /. trg, MapThread[Rule, {cfs, cfe}]],
      hist -> {"UnionTopology", opts}]];

(* trap *)
UnionTopology[___] :=
  (Message[UnionTopology::usage];
   Abort[]);












Options[UnionTopology] =
{};

(* TODO: Method, ... same keys *)
Options[UnionTopology] = Union[
  Options[UnionTopology],
  Options[TopologyMomentaShifts]];


UnionTopology[
  "0",
  src:TopologyPattern["Alpha"], trg:TopologyPattern["Alpha"],
  set:SetupPattern[], opts:OptionsPattern[]] :=
  Module[
    {},
    Null
  ];


(* N.B.:

- find greatest common subset of <src> and <trg>
- find momentum shift between subsets, else pick next (smaller) subset
- apply shift to complement of <src> and add result to <trg>

*)



























(* --- SuperTopology ----------------------------------------------- *)

Options[SuperTopology] =
{};

(* TODO: Method, ... same keys *)
Options[SuperTopology] = Union[
  Options[SuperTopology],
  Options[InspectTopology],
  Options[TopologyMomentaShifts]];

SuperTopology::usage = "\
TODO";

(* main *)
SuperTopology[
  tops_?TopologyListQ[#, "Alpha"] & , set:SetupPattern[],
  opts:OptionsPattern[]] :=
  Module[
    {},
    Null
  ];

(* trap *)
SuperTopology[___] :=
  (Message[SuperTopology::usage];
   Abort[]);

(* --- package end -------------------------------------------------- *)

Protect["TopoID`Set`*"];

Scan[
  SetAttributes[#, {ReadProtected}] & ,
  Select[Symbol /@ Names["TopoID`Set`*"], Head[#] === Symbol & ]];

End[];

EndPackage[];

(* ------------------------------------------------------------------ *)



(* --- TODO:

- Triggers for "Alpha", "Subt"?

- dive into smaller intersection if no shift is found?

*)
