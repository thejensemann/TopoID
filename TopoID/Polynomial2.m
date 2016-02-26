(* -- "Polynomial2.m": Related to Polynomials ------------------------ *)

(* --- provided functions:



*)

(* --- package begin ------------------------------------------------ *)

Unprotect["TopoID`Polynomial2`*"];

ClearAll["TopoID`Polynomial2`*", "TopoID`Polynomial2`Private`*"];

BeginPackage[
  "TopoID`Polynomial2`",
  {"TopoID`Common`", "TopoID`System`",  (* TODO *)

   "TopoID`Topology`",
   "TopoID`Setup`",

   "TopoID`Polynomial`"}];

{c, c1, c2}

{$TopologyToCrossingRulesMethods, TopologyToCrossingRules};

Begin["`Private`"];







(* --- TopologyToCrossingRules ---------------------------------------------- *)

$TopologyToCrossingRulesMethods =
{"Cuts", "Mask", "Check"};  (* TODO: implement those *)

$TopologyToCrossingRulesMethods = Union[
  $TopologyToCrossingRulesMethods, $SymmetricGroupsMethods];

Options[TopologyToCrossingRules] =
{Method -> {"Subt", "Only", "Order", (*"Filter",*) "Check"},
 Parallel -> False,
 Verbosity -> False};
(* Cf. $TopologyToCrossingRulesMethods *)

TopologyToCrossingRules::usage = "\
TopologyToCrossingRules[<top>, <set>, [<lvl>], [opts]] ... TODO";



(* main *)
TopologyToCrossingRules[
  top:TopologyPattern[], set:SetupPattern[], lv_:All,
    opts:OptionsPattern[]] :=
  Module[
    {ts, as,ps, ar,ap, c, term,wrap, symms, f, trans, g, h, xxp, xxr},

    ts = Symbol[name /. top];
    as = MapIndexed[Symbol["a" <> ToString[First[#2]]] & , facs /. top];
    ps = Pattern[#, _] & /@ as;

    {ar, ap} = {arep, apar} /. top;

    (* helper: term from coefficient rule # *)
    term = c[Last[#]]*Inner[Power, ap, First[#], Times] & ;
    (* helper: wrap all coefficients in # in coeff[] *)
    wrap = Plus @@ (term /@ CoefficientRules[#, ap]) & ;

    symms = First[SymmetricGroups[ar, ap, lv, opts]];  (* TODO: lv, opts *)

    (* wrap invariants in W polynomials *)
    symms = ({First[ar], wrap[Last[ar]]} /. PermuteRules[First[#], ap]) -> # & /@ symms;

    (* compare U and W (modulo invariants) *)
    symms = GatherBy[symms, {First[#] /. c[_] -> 1, Length[Part[#, 2, 1]]} & ];
    (* N.B.: mutually exclusive structures cannot appear after this *)

    (* need at least two representatives *)
    symms = Select[symms, Length[#] > 1 & ];

    (* discard U polynomials *)
    f = Part[#, 1, 2] -> Last[#] & ;
    symms = f /@ # & /@ symms;



    (* helper: permutation;  #1: e.g. {{a1, a2, 0, 0}, {a4}}, #2: e.g. 3 *)  (*perform permutation for single index*)
    xxp = {ReplacePart[#1[[1]], #2 -> First[#1[[2]]]], Rest[#1[[2]]]} & ;

    (* helper: replacement;  #1: e.g. {1, 2, 4}, #2: e.g. {1, 2, 3} *)  (*permutation and replacement for all indices*)
    xxr = First[Fold[xxp, {Table[0, {Length[as]}], as[[#1]]}, #2]] & ;


    h[src_, trg_] := Module[
      {tmp, rls1,rls2, rels1,rels2, sol1,sol2, vrs1,vrs2, lhs,rhs},
      tmp = (First[src] /. c -> c1) + (First[trg] /. c -> c2);
      tmp = DeleteCases[DeleteDuplicates[Flatten[CoefficientList[tmp, ap]]], 0];
      (*tmp = Select[tmp, (# /. {c1 -> Identity, c2 -> Identity}) =!= 0 & ];*)
      rls1 = # -> c1[#] & /@ (xs /. set);
      rls2 = # -> c2[#] & /@ (xs /. set);
      tmp = tmp /. {c1[x_] :> (x /. rls1), c2[x_] :> (x /. rls2)};
      rels1 = MapIndexed[#1 == c[First[#2]] & , tmp /. c2[_] -> 0];
      rels2 = MapIndexed[#1 == c[First[#2]] & , tmp /. c1[_] -> 0];
      sol1 = Solve[rels1, Variables[First /@ rels1]];
      If[sol1 === {}, Return[{}]];
      sol2 = Solve[rels2, Variables[Last /@ rels2]];
      If[sol2 === {}, Return[{}]];
      tmp = First[sol1] /. First[sol2] /. {c1 -> Identity, c2 -> Identity};
      tmp = DeleteCases[tmp, HoldPattern[x_ -> x_]];
      (*vrs1 = Select[xs /. set, !FreeQ[First[src], #] & ];*)
      vrs1 = Select[xs /. set, !FreeQ[Last /@ (facs /. top), #] & ];
      vrs1 = vrs1 /. x_^_ -> x;
      vrs2 = Select[xs /. set, !FreeQ[First[trg], #] & ];
      vrs2 = vrs2 /. x_^_ -> x;
      If[
        Expand[(First[src] /. tmp) - First[trg]] =!= 0,
        Print["!!!ALARM!!!"];
        Return[{}]
      ];
      lhs = MapIndexed[If[MemberQ[Part[src, 2, 1], First[#2]], #1, 0] & , ps];
      rhs = xxr[Part[src, 2, 1], Part[trg, 2, 1]];
      ts[lhs, Pattern[#, _] & /@ vrs1] -> ts[rhs, vrs2 /. tmp]
    ];

    g = Function[{o, oo}, h[o, #] & /@ oo];

    (* in each group: mapping from each element to all others *)
    trans = Join @@ (Function[tr, Join @@ (g[#, Complement[tr, {#}]] & /@ tr)] /@ symms);

    trans = DeleteCases[trans, {}];

    Return[trans];




    (*s = Function[rs, If[# =!= {}, First[#], #] & @ Solve[# == 0 & /@ rs, Last /@ Join[rls2, rls1]]];*)
    (*s = Function[rs, If[# =!= {}, First[#], #] & @ Solve[# == 0 & /@ rs, Cases[Variables[rs], c2[_]]]];*)
    s = Function[rs, {Solve[# == 0 & /@ rs, Cases[Variables[rs], c2[_]]],
                      Solve[# == 0 & /@ rs, Cases[Variables[rs], c1[_]]]}];

    trans = s /@ # & /@ trans;

    Return[trans];

    cln = Function[tr, Select[tr, !MatchQ[# /. {c1 | c2 -> Identity}, HoldPattern[x_ -> x_]] & ]];

    trans = cln /@ # & /@ trans;

    cln = If[!FreeQ[#, HoldPattern[_ -> 0]], $Failed, #] & ;

    trans = cln /@ # & /@ trans;

    cln = If[# =!= $Failed, {Last /@ rls1, Last /@ rls2} /. #, #] & ;

    trans = cln /@ # & /@ trans;

    trans];



(* case: "Symm" or better "Subt"? *)


(* trap *)
TopologyToCrossingRules[___] :=
  (Message[TopologyToCrossingRules::usage];
   Abort[]);

(* --- package end -------------------------------------------------- *)

Protect["TopoID`Polynomial2`*"];

Scan[
  SetAttributes[#, {ReadProtected}] & , Select[
    Symbol /@ Names["TopoID`Polynomial2`*"], Head[#] === Symbol & ]];

End[];

EndPackage[];

(* ------------------------------------------------------------------ *)



(* --- TODO:

-

*)
