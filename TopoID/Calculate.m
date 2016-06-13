(* -- "Calculate.m": Auxiliary Routines ----------------------------- *)

(* --- provided functions:

TopologyOrder[<src>, <trg>, <set>, [opts]] -> -1 | 0 | 1
-- ordering function on topologies

SortTopologies[<tops>, [<set>], [opts]] -> <tops>
-- sort a list of topologies


TopologyToRules[<top(s)>] -> <rules>
-- obtain rules for manipulations involving topologies


LookUp[<int(s)>, [<key(s)>], [opts]] -> <rules>
-- get reduction rules for integrals from table files

TAP[<key(s), [opts]] -> <rules>
-- dynamic reduction replacement rule acting directly


TODO:

- LookUps[]: fixed point iteration of LookUp for all RHS integrals and
  subsequent "soft" reduction via LaportaEncode[]

*)

(* --- package begin ------------------------------------------------ *)

Unprotect["TopoID`Calculate`*"];

ClearAll["TopoID`Calculate`*", "TopoID`Calculate`Private`*"];

BeginPackage[
  "TopoID`Calculate`",
  {"TopoID`Common`", "TopoID`System`",  (* TODO *)

   "TopoID`Setup`",
   "TopoID`Topology`",
   "TopoID`Mapping`",

   "TopoID`Core`",

   "TopoID`Cuts`",  (* TODO: needed? *)
   "TopoID`FORM`",

   "TopoID`Object`"}];



{$TopologyOrderMethods, TopologyOrder, SortTopologies};                 (* -> DONE? *)




{$TopologySymbolPatterns,TopologySymbolQ};

{TopologyIntegralPattern,TopologyIntegralQ};

{TopologyIntegralComplexity,
 TopologyIntegralSort, TopologyIntegralList, TopologyIntegralRules, TopologyMaxIntegral};








(* ###################### *)

(* TODO: legacy version? *)
(*TopologyToRules[args___] :=
  TopologyRules[args];*)

{TopologyToRules, TopologyCalculate};

(* ##################### *)

{$TopologySimplifyRules, TopologySimplifyRules, TopologySimplify};

{$TopologyFactors,$TopologyFunction, $TopologyExpand,$TopologyReduce,
 $TopologyShifts, $TopologyRulesOrdering,
 $TopologyRulesMethods,TopologyRules, SetTopologyRules};






(*

- $TopologyNamePatterns?

- something like subset -> list (like ReverseMapping)


*)





{                                             (* -> DONE? *)

 $TopoIDToFactors, $TopoIDExpand, $TopoIDInvert, $TopoIDToFunction,
 $TopoIDRules,
 $D, $N, (*$TopoIDTransform,*)
 TopologyToSimplify,
 TopoIDSimplify};

{TopologyToKinematics};

(* DEQs *)
{TopologyDerivatives, SetTopologyDerivatives};

(* ################### *)


{$LookUpMethods, $LookUpCommands, $LookUp,LookUp,
 LookUps, LookUpClear, TAP};                                            (* DONE *)

{IntegralRelations};


(* REVISIT *)
{LaportaLimits, LaportaInit, $LaportaScan, LaportaScan, LaportaLimit};
{TopologyIndex, TopologyLimits, TopologyLimit,
 $LaportaEncode, LaportaEncode, $LaportaDecode, LaportaDecode};
{LaportaHull, LaportaFill, $LaportaSeed, LaportaSeed,
 LaportaSector, LaportaSectors};



{TopologyIBP, TopologyIBPs,
 (*$TopologyIBPRule,*) TopologyIBPRule, TopologyIBPRules};              (* DONE *)

Begin["`Private`"];








(* --- TopologyOrder ------------------------------------------------ *)

$TopologyOrderMethods =
{"Externals", "Internals", "Masses", "Constants",
 "Cuts", "Factors", "Alpha", "Indices"};

Options[TopologyOrder] =
{Method -> $TopologyOrderMethods};

TopologyOrder::usage = "\
TopologyOrder[<src>, <trg>, [<set>], [opts]] is an ordering function \
acting on two topologies <src> and <trg>, optionally defined also by \
information from the setup [<set>].
The option [Method] can be used to specify the desired order via a \
list of keywords, for possible values cf. $TopologyOrderMethods.";

(* main *)
TopologyOrder[
  src:TopologyPattern[], trg:TopologyPattern[], set:SetupPattern[],
    opts:OptionsPattern[]] :=
  Module[
    {me, cp, f, m, res},
    (* check: Method *)
    me = OptionValue[Method];
    me = Flatten[{me}];
    cp = Complement[me, $TopologyOrderMethods];
    If[cp =!= {},
       Message[Method::invalid, cp, $TopologyOrderMethods];
       me = DeleteCases[me, Alternatives @@ cp]];
    (* helper: check factors of topology #1 for setup key #2 *)
    f = If[
      set =!= Null,
      Intersection[Variables[Last /@ (facs /. #1)], #2 /. set],
      {}] & ;
    (* gather demanded information *)
    res = Function[m, Switch[
      m,
      "Externals",
      f[#, ps],
      "Internals",
      f[#, ks],
      "Masses",
      f[#, ms],
      "Constants",
      f[#, xs],
      "Cuts",
      Length /@ (cuts /. # /. cuts -> {}),
      (*"Zero",
      Length /@ ,*)
      (*"Symm",
      ,*)
      "Factors",
      GatherBy[First /@ (facs /. #), DenominatorSymbolQ],
      "Alpha",
      Expand[$TopoIDMetric @@ (arep /. # /. arep -> {})],
      "Indices",
      inds /. # /. inds -> {}] & /@ {src, trg}] /@ me;
    (* result: apply default if undecided *)
    res = DeleteCases[Order @@@ res, 0];
    If[res === {},
       Order[src, trg],
       First[res]]];

(* trap *)
TopologyOrder[___] :=
  (Message[TopologyOrder::usage];
   Abort[]);

(* N.B.:
- Cuts: max. nr. of props., min. nr. of cuts.
- Symmetries: max. length of repr., nrs. of reprs., nr. of symms.
- Zero: max. lenght of subt., nr. of subt.
- TopologyRank[]?
- Numerators, Denominators.
- Vertices, Edges.
- Use NestWhile?
*)

(* TODO:
- Move to "Core.m"?
*)

(* --- SortTopologies ----------------------------------------------- *)

SortTopologies::usage = "\
SortTopologies[<tops>, [<set>], [opts]] returns the list of topologies \
<tops> sorted in a definitive order, optionally defined also by \
information from the setup [<set>].
The option [Method] can be used to specify the desired order via a \
list of keywords, for possible values cf. $TopologyOrderMethods.";

Options[SortTopologies] =
  Options[TopologySort];

(* main *)
SortTopologies[
  tops_?TopologyListQ, set:Elective[SetupPattern[]],
  opts:OptionsPattern[]] :=
    Sort[tops, TopologyOrder[#1, #2, set, opts] === 1 & ];

(* trap *)
SortTopologies[___] :=
  (Message[SortTopologies::usage];
   Abort[]);

(* Needs:
- TopologyOrder[].
*)











(* --- $TopologySymbolPatterns, TopologySymbolQ --------------------- *)

$TopologySymbolPatterns::usage = "\
$TopologySymbolPatterns contains a list of strings or stringpatterns \
used to match symbols denoting topologies.";

$TopologySymbolPatterns =
{"LO*", "NLO*", "NNLO*", "N3LO*",  (* gg -> h, gg -> hh *)
 "A", "B", "C"};                   (* gg -> gh *)
(* String, StringPattern, RegularExpression *)

(* -- *)

TopologySymbolQ::usage = "\
TopologySymbolQ[<x>] returns True if <x> matches a topology symbol, \
cf. $TopologySymbolPatterns, and False otherwise.";

TopologySymbolQ[x_] :=
  Head[x] === Symbol &&
  Or @@ (StringMatchQ[ToString[x], #] & /@ $TopologySymbolPatterns);

(* trap *)
TopologySymbolQ[___] :=
  (Message[TopologySymbolQ::usage];
   Abort[]);

(* --- TopologyIntegralPattern, TopologyIntegralQ ------------------- *)

TopologyIntegralPattern::usage = "\
TopologyIntegralPattern[] represents a pattern for integrals within \
topologies, cf. $TopologySymbolPatterns.";

TopologyIntegralPattern[] :=
  _?TopologySymbolQ[__Integer];

(* TODO: TopologyIntegralPattern[<keys>] *)

(* trap *)
TopologyIntegralPattern[___] :=
  (Message[TopologyIntegralPattern::usage];
   Abort[]);

(* -- *)

TopologyIntegralQ::usage = "\
TopologyIntegralQ[<x>] returns True if <x> denotes an integral within \
a topology, cf. $TopologySymbolPatterns, and False otherwise.";

TopologyIntegralQ[x_] :=
  MatchQ[x, TopologyIntegralPattern[]];

(* TODO: TopologyIntegralQ[<x>, <keys>] *)

(* trap *)
TopologyIntegralQ[___] :=
  (Message[TopologyIntegralQ::usage];
   Abort[]);

(* --- ...Complexity, ...Sort, ...List, ...MaxIntegral -------------- *)

TopologyIntegralComplexity::usage = "\
TopologyIntegralComplexity[<x>] returns a list of properties that can \
be used to define an order on topology integrals <x>, cf. \
TopologyIntegralPattern[].
If <x> is an expression containing multiple topology integrals only \
the most complicated one is considered.";

(* main: topology integral *)
TopologyIntegralComplexity[i_?TopologyIntegralQ] :=
{Count[i, _?Positive],           (* number of lines *)
 (*Plus @@ (Abs /@ i),*)         (* sum of absolute powers *)
 Plus @@ Cases[i, _?Positive],   (* sum of denominator powers *)
 -Plus @@ Cases[i, _?Negative],  (* sum of numerator powers *)
 Head[i],                        (* topology name *)
 List @@ i};                     (* List of indices *)

(* overload: expression *)
TopologyIntegralComplexity[x_] :=
  TopologyIntegralComplexity[TopologyMaxIntegral[x]];

(* trap *)
TopologyIntegralComplexity[___] :=
  (Message[TopologyIntegralComplexity::usage];
   Abort[]);

(* -- *)

TopologyIntegralSort::usage = "\
TopologyIntegralSort[<list>] returns the list of expressions <list> \
sorted by integral complexity, cf. TopologyIntegralComplexity[].";

TopologyIntegralSort[l_List] :=
  SortBy[l, TopologyIntegralComplexity];

(* trap *)
TopologyIntegralSort[___] :=
  (Message[TopologyIntegralSort::usage];
   Abort[]);

(* -- *)

TopologyIntegralList::usage = "\
TopologyIntegralList[<x>] gives a list of integrals sorted by their \
respective complexity occuring in <x>, cf. \
TopologyIntegralComplexity[].";

TopologyIntegralList[x_] :=
  TopologyIntegralSort[Union[
    Cases[x, TopologyIntegralPattern[], {0, Infinity}]]];

(* TODO: TopologyIntegralList[<x>, <keys>] *)

(* trap *)
TopologyIntegralList[___] :=
  (Message[TopologyIntegralList::usage];
   Abort[]);

(* -- *)

TopologyIntegralRules::usage = "\
TopologyIntegralRules[<x>] gives for <x> replacement lists with \
topology symbols as left-hand sides and integrals sorted by their \
respective complexity as right-hand sides.";

TopologyIntegralRules[x_] :=
  Part[#, 1, 0] -> # & /@ GatherBy[TopologyIntegralList[x], Head];

(* trap *)
TopologyIntegralRules[___] :=
  (Message[TopologyIntegralRules::usage];
   Abort[]);

(* -- *)

TopologyMaxIntegral::usage = "\
TopologyMaxIntegral[<x>] returns the most complicated integral in <x> \
according to TopologyIntegralComplexity[].";

TopologyMaxIntegral[x_] :=
  Last[TopologyIntegralList[x]];

(* TODO: TopologyMaxIntegral[<x>, <keys>] *)

(* trap *)
TopologyMaxIntegral[___] :=
  (Message[TopologyMaxIntegral::usage];
   Abort[]);













(* --- TopologyToRules ---------------------------------------------- *)

(* default of Sort[] *)
$TopologyToRulesOrdering = OrderedQ[{#1, #2}] & ;

(* #3 is the ordering of factor symbols *)
$TopologyToRulesOrdering = Function[
  {a, b, o},
  If[# === {}, False, #[[1, 1]] > #[[1, 2]]] & @ Select[
    Transpose[List @@ # & /@ {a, b}][[o]], #[[1]] =!= #[[2]] & , 1]];

TopologyToRules::usage = "\
TopologyToRules[<top>] returns a list of replacement rules according \
to the vanishing and symmetric subsets of factors for the topology \
object <top>.
For example, obtained rules can be used in the following ways:
  trs = TopologyToRules[<top>];
- exp /. trs,
- exp //. trs,
- Union[ReplaceList[exp, trs]],
- FixedPoint[Fold[ReplaceAll, #, trs] & , exp].
Note that the last version is also used in TopologyCalculate[<x>, \
<rules>].";

TopologyToRules::symbol = "\
The topology name \"`1`\" cannot be converted into a valid symbol.";

(* overload: plural *)
TopologyToRules[tops_?TopologyListQ] := Join @@
  ((Status[Console, name /. #];
    TopologyToRules[#]) & /@ tops);

(* main *)
TopologyToRules[
  top:TopologyPattern[]] :=
  Module[
    {ts, or,no, um,is,ps, v,f, c,p,r,s,n,g,h, zrs,srs},

    (* generate symbol name *)
    ts = Quiet[Check[Symbol[name /. top], Null]];

    (* check: symbol name *)
    If[ts === Null,
       Message[TopologyToRules::symbol];
       Abort[]];

    or = Ordering[First /@ (facs /. top)];
    no = Length[or];

    um = Range[no];
    is = Symbol["a" <> #] & /@ ToString /@ um;
    ps = Pattern[#, _] & /@ is;

    (* -- zeros -- *)

    (* helper: vanishing;  #: e.g. {a1} *)
    v = Pattern[#, _] /; # <= 0 & @ #[[1]] & ;

    (* #: e.g. {1, 2, 4} *)
    f = ts @@ MapAt[v, ps, List /@ Complement[um, #]] -> 0 & ;

    (* -- symmetries -- *)

    (* helper: condition;  #: e.g. {1, 2, 4} *)  (*set indices of non-present lines to zero*)
    c = ts @@ ReplacePart[ps, # -> 0 & /@ Complement[um, #]] & ;

    (* helper: permutation;  #1: e.g. {{a1, a2, 0, 0}, {a4}}, #2: e.g. 3 *)  (*perform permutation for single index*)
    p = {ReplacePart[#1[[1]], #2 -> First[#1[[2]]]], Rest[#1[[2]]]} & ;

    (* helper: replacement;  #1: e.g. {1, 2, 4}, #2: e.g. {1, 2, 3} *)  (*permutation and replacement for all indices*)
    r = ts @@ First[Fold[p, {Table[0, {no}], is[[#1]]}, #2]] & ;

    (* helper: ordering *)
    s = Function[o, $TopologyToRulesOrdering[#1, #2, o] & ] @ or;

    (* helper: notation *)
    n = Function[o, RuleDelayed[#1, First[Sort[#2, o]]] & ] @ s;

    (* combine all *)

    (* #1: e.g. {1, 2, 4}, #2: e.g. {{1, 2, 4}, {1, 2, 3}} *)  (*replacement from one representation to all others*)
    g = n @@ {c[#1], Function[s, r[#1, s]] /@ #2} & ;

    (* ss: e.g. {{1, 2, 4}, {1, 2, 3}} *)  (*apply on group of symmetric representations*)
    h = Function[ss, g[#, ss] & /@ ss];

    (* -- apply -- *)

    {zrs, srs} = {zero, symm} /. top;

    zrs = If[zrs === zero, {}, f /@ zrs];
    srs = If[srs === symm, {}, h /@ srs];

    (* -- result -- *)

    Join[zrs, Join @@ srs]];

(* trap *)
TopologyToRules[___] :=
  (Message[TopologyToRules::usage];
   Abort[]);

(* TODO:
- add options to select data, kind of symmetries...
*)


(* --- TopologyCalculate -------------------------------------------- *)

TopologyCalculate::usage = "\
TopologyCalculate[<x>, <rules>] applies to topology integrals \
appearing in expression <x> replacements <rules> excessively.";

TopologyCalculate[
  x_, rs_List] :=  (* TODO: check <rs> *)
  Module[
    {lhs, rhs},
    lhs = TopologyIntegralList[x];
    rhs = FixedPoint[Fold[ReplaceAll, #, rs] & , lhs];
    x /. MapThread[Rule, {lhs, rhs}]];

(* trap *)
TopologyCalculate[___] :=
  (Message[TopologyCalculate::usage];
   Abort[]);









(* --- TopologySimplify[Rules] -------------------------------------- *)  (* DONE *)

$TopologySimplifyRules::usage = "\
List of rules applied by TopologySimplify[] by default.";

$TopologySimplifyRules =
{};

(* -- *)

TopologySimplifyRules::usage = "\
TopologySimplifyRules[<x>, [<key(s)>], [<rules>]] gives rules to \
simplify specific integrals from topologies in the expression <x> as \
far as possible using the rules in $TopologySimplifyRules.
The set of considered topologies can be restricted with [<key(s)>] \
which should be symbols, strings or string patterns.  The applied \
rules can also be specified directly with [<rules>].";

(* main *)
TopologySimplifyRules[
  x_, ks_:{}, rs_:{}] :=
  Module[
    {lhs, tss, rls, rhs},
    (* check: keys *)
    lhs = Variables[Cases[x, _Symbol[__Integer], {0, Infinity}]];
    (* select integrals *)
    lhs = Select[lhs, $CheckKeys[ks]];
    (* topology symbols *)
    tss = Union[Head /@ lhs];
    (* check: rules *)
    rls = If[rs === {}, $TopologySimplifyRules, rs];
    (* select relevant rules *)
    rls = Select[rls, Function[rl, Or @@ (!FreeQ[rl, #] & /@ tss)]];
    (* replace repeatedly *)
    rhs = FixedPoint[Fold[ReplaceAll, #, rls] & , lhs];
    (* result: simplification rules *)
    MapThread[Rule, {lhs, rhs}]];

(* trap *)
TopologySimplifyRules[___] :=
  (Message[TopologySimplifyRules::usage];
   Abort[]);

(* -- *)

TopologySimplify::usage = "\
TopologySimplify[<x>, [<key(s)>], [<rules>]] simplifies the expression \
<x> containing specific integrals from topologies as far as possible \
using the rules in $TopologySimplifyRules.
The set of considered topologies can be restricted with [<key(s)>] \
which should be symbols, strings or string patterns.  The applied \
rules can also be specified directly with [<rules>].";

(* main: shortcut to TopologySimplifyRules *)
TopologySimplify[x_, ks_:{}, rs_:{}] :=
  x /. TopologySimplifyRules[x, ks, rs];

(* trap *)
TopologySimplify[___] :=
  (Message[TopologySimplify::usage];
   Abort[]);

(* --- TopologyRules ------------------------------------------------ *)  (* ... OPEN ... *)

(* default of Sort[] *)
$TopologyRulesOrdering =
  OrderedQ[{#1, #2}] & ;

(* #3: ordering of factor symbols *)                                   (* TODO? *)
$TopologyRulesOrdering = Function[
  {a, b, o},
  If[# === {}, False, #[[1, 1]] > #[[1, 2]]] & @ Select[
    Transpose[List @@ # & /@ {a, b}][[o]], #[[1]] =!= #[[2]] & , 1]];




TopologyRules::usage = "\                                                  TODO
TopologyRules[<top>] returns a list of replacement rules according \
to the vanishing and symmetric subsets of factors for the topology \
object <top>.
For example, obtained rules can be used in the following ways:
  trs = TopologyToRules[<top>];
- exp /. trs,
- exp //. trs,
- Union[ReplaceList[exp, trs]],
- FixedPoint[Fold[ReplaceAll, #, trs] & , exp].";



$TopologyRulesMethods =
{"Zero", "Subt", "Symm", "Shifts", "Constants"};

Options[TopologyRules] =
{Method -> {"Zero", "Symm"}};
(* TODO: superseed with Options[SymmetrizeTopology] *)

(* overload: plural *)
TopologyRules[tops_?TopologyListQ] :=
  Join @@ ((Status[Console, name /. #]; TopologyRules[#]) & /@ tops);










(* main *)
TopologyRules[
  top:TopologyPattern[], set:SetupPattern[],  (* TODO: set *)
    opts:OptionsPattern[]] :=
  Module[
    {ts, me, os,
     fs,ft, fas, frs, is,ps, x,p,
     dy1,dy2,dy3,dy4,dy5,dy6,
     xd,fad,td,fd,fds,
     rds, i,

     or,no, um, v,f, c,r,s,g,h, zrs,srs},

    ts = $CheckName[name /. top];
    me = $CheckMethod[OptionValue[Method], $TopologyRulesMethods];

    (*set = setp /. top;*)

    os = FilterRules[{opts}, Options[SymmetrizeTopology]];

    fs = First /@ (facs /. top);
    ft = Last /@ (facs /. top);
    fas = Alternatives @@ fs;
    frs = MapThread[#1 -> n[First[#1], #2] & , {fs, ft}];

    is = MapIndexed[Symbol["a" <> ToString[First[#2]]] & , fs];
    ps = Pattern[#, _] & /@ is;

    x = Symbol["x"];
    p = Pattern[#, _] & @ x;

    $TopologyFactors[ts] =
      ts @@ ps -> ts[Inner[Power, fs, -is, Times]];

    $TopologyFunction[ts] =
      ts[p] :> Times[dy1 /. dy2 -> 1, dy3 @@ (-Exponent[dy1, #] & /@ dy4)];
    $TopologyFunction[ts] = $TopologyFunction[ts] /.
    {dy1 -> x, dy2 -> fas, dy3 -> ts, dy4 -> fs};

    $TopologyExpand[ts] =
      ts[p] :> dy1[Numerator[dy2 /. dy3]/Denominator[dy2]];
    $TopologyExpand[ts] = $TopologyExpand[ts] /.
    {dy1 -> ts, dy2 -> x, dy3 -> frs};

    $TopologyReduce[ts] = {};
    If[(scps /. top) =!= scps,
       rds = Join[
         scps /. top,
         DeleteCases[cs /. set, HoldPattern[m[_] -> _]]];
       $TopologyReduce[ts] =
         ts[p] :> dy1[dy2 /. dy3];
       $TopologyReduce[ts] = $TopologyReduce[ts] /.
       {dy1 -> ts, dy2 -> x, dy3 -> rds}];

    $TopologyShifts[ts] = Identity;
    If[MemberQ[me, "Shifts"],
       i = Symbol["i"];
       $TopologyShifts[ts] =
         (#1
          (* rewrite as propagator factors *)
          /. $TopologyFactors[dy1]
          (* express numerator in momenta and masses *)
          /. $TopologyExpand[dy1]
          (* apply transformation rules *)
          /. #2
          (* expand to scalar products *)
          /. n[dy2, dy3] :> n[dy4, Expand[dy5]]
          (* reexpress as propagator factors *)
          /. $TopologyReduce[dy1]
          (* remove wrapper *)
          /. n[_, dy3] -> dy5
          (* expand and cancel powers of factors *)
          /. dy1[dy3] :> dy1[Expand[dy5]]
          (* distribute topology wrapper *)
          /. dy1[dy6] :> dy1 /@ dy5
          (* topology function notation *)
          /. $TopologyFunction[dy1]) & ;
       $TopologyShifts[ts] = $TopologyShifts[ts] /.
       {dy1 -> ts, dy2 -> Pattern[#, _] & @ i, dy3 -> p,
        dy4 -> i, dy5 -> x, dy6 -> Pattern[#, _Plus] & @ x}];

    Return[];














    or = Ordering[First /@ (facs /. top)];
    no = Length[or];

    um = Range[no];
    is = Symbol["a" <> #] & /@ ToString /@ um;
    ps = Pattern[#, _] & /@ is;

    (* -- zeros -- *)

    (* helper: vanishing;  #: e.g. {a1} *)
    v = Pattern[#, _] /; # <= 0 & @ #[[1]] & ;

    (* #: e.g. {1, 2, 4} *)
    f = ts @@ MapAt[v, ps, List /@ Complement[um, #]] -> 0 & ;

    (* -- symmetries -- *)

    (* helper: condition;  #: e.g. {1, 2, 4} *)  (*set indices of non-present lines to zero*)
    c = ts @@ ReplacePart[ps, # -> 0 & /@ Complement[um, #]] & ;

    (* helper: permutation;  #1: e.g. {{a1, a2, 0, 0}, {a4}}, #2: e.g. 3 *)  (*perform permutation for single index*)
    p = {ReplacePart[#1[[1]], #2 -> First[#1[[2]]]], Rest[#1[[2]]]} & ;

    (* helper: replacement;  #1: e.g. {1, 2, 4}, #2: e.g. {1, 2, 3} *)  (*permutation and replacement for all indices*)
    r = ts @@ First[Fold[p, {Table[0, {no}], is[[#1]]}, #2]] & ;

    (* helper: ordering *)
    s = Function[o, $TopologyRulesOrdering[#1, #2, o] & ] @ or;

    (* helper: notation *)
    n = Function[o, RuleDelayed[#1, First[Sort[#2, o]]] & ] @ s;

    (* combine all *)

    (* #1: e.g. {1, 2, 4}, #2: e.g. {{1, 2, 4}, {1, 2, 3}} *)  (*replacement from one representation to all others*)
    g = n @@ {c[#1], Function[s, r[#1, s]] /@ #2} & ;

    (* ss: e.g. {{1, 2, 4}, {1, 2, 3}} *)  (*apply on group of symmetric representations*)
    h = Function[ss, g[#, ss] & /@ ss];

    (* -- apply -- *)

    {zrs, srs} = {zero, symm} /. top;

    zrs = If[zrs === zero, {}, f /@ zrs];
    srs = If[srs === symm, {}, h /@ srs];

    (* -- result -- *)

    Join[zrs, Join @@ srs]];


(* TODO: overloads for no setup, "Solve" *)






(* trap *)
TopologyRules[___] :=
  (Message[TopologyRules::usage];
   Abort[]);

(* N.B.:

- Already existent subtopologies and symmetries are used by default.

- SymmetrizeTopology[] is called otherwise.

- Method -> {"Zero", "Symm"} with pre-symmetrized topology corresponds
  to the old scheme.

- Without any entries in Method only global rules for notation change
  are set.

- $TopologyFactors[<top>] wrap product in topology symbol to prevent
  confusion with index vector notation.

*)

(*

"Zero" -- vanishing subtopologies,

"Subt" -- subtopology mapping,
"Symm" -- subtoplogy symmetrization,
"Subt" + "Symm" -- both simultaneously,

"Shifts" -- " include momenta shifts in numerator,
"Constants" -- " include crossing or invariant transformation.

*)

(* TODO:

change of philosophy?
topology should be prepared? solved, setup, symmetries, mappings...
only calculate shifts, transformations, rules here

*)



(* want:

for sector {1,3,5,...}

A[a1_,a2_ /; a2 <= 0,a3_,a4_ /; a4 <= 0, 0, a6_, ...] :>
A[a3,0,a1,0,0,a6,...][p[1] + p[2] - mh^2, p[1] - p[3], ...] /;
  or
A[a3,0,a1,0,0,a6,...][0, p[1] - p[3], ...] /;
<< most complicated integral of the expanded sum simpler than LHS >>

sector: some lines are restricted to be non-positive

transformed form (and application) for all factors in second argument
list or only for the numerators?

cases:
  no additional numerators (lines out of the subset)?
  -yes-> only permutation [but is this then the minimal form? handled by
   lower symmetry?]
  -no--> exists shift for additionals?
    -yes-> apply
    -no--> do nothing

*)







(* --- TopologyToSimplify ------------------------------------------- *)

TopologyToSimplify[
  top:TopologyPattern["Symm", "Solve"], set:SetupPattern[]] :=
  Module[
    {nm, ts, is,js, fs, xs, xt,xu,xv, tt,ft,
     f,g,h, sm, res},

    Print["calculating shifts"];

    (* find shifts, rewrite rules *)

    (**)

    h[r1_, r2_] := Module[
      {m1,m2, t1,t2, sh, pm},
      (*m1 = Mapping[nm <> "s1", nm, r1];
      m2 = Mapping[nm <> "s2", nm, r2];
      m1 = ReverseMapping[m1, Length[fs]];
      m2 = ReverseMapping[m2, Length[fs]];
      t1 = MapToTopology[m1, top];
      t2 = MapToTopology[m2, top];*)

      t1 = Topology[name -> (nm <> "s1"), facs -> (facs /. top)[[r1]]];
      t2 = Topology[name -> (nm <> "s2"), facs -> (facs /. top)[[r2]]];
      sh = TopologyMomentaShifts[t1, t2, set];
      WriteString["stdout", "."];

      pm = MapThread[Rule, {fs[[r1]], fs[[r2]]}];
      {Complement[fs, fs[[r1]]],
       DeleteCases[pm, HoldPattern[x_ -> x_]],
       sh};

      {ts @@ js,
       And @@ (# <= 0 & /@ Complement[is, is[[r1]]]),
       ts @@ (is),
       If[sh =!= {}, Expand[(Last /@ (facs /. top)) /. sh] /. (scps /. top) /. (cs /. set), {}]}
    ];

    sm = symm /. top;

    sm = sm[[{6,7,8}]];  (* TODO: TEMP *)

    g = Function[{r, rs}, h[r, #] & /@ rs];
    f = Function[rs, Join @@ (g[#, Complement[rs, {#}]] & /@ rs)];
    (*f = Function[rs, h[First[rs], #] & /@ Rest[rs]];*)

    $TopoIDRules[ts] = Join @@ (f /@ sm);

    (* -- *)

  ];





(*
- rearrange denominators
- shift scalar products

- rexpress in top.: to factors, [shift,] expand, restrict, invert, to function

- compute transformations from each representation to all others?
-> better from first to all others; inverse is obvious?!                       [?]

- condition for subtopology                                                    [?]

- how to handle case when no shift found but also no numerator present?        [?]

- condition for applying a shift?                                              [?]

- no filtering of symmetries in case?

*)





























(* TODO:
- merge with TopologyToRules[]
*)





(* --- TopologyToKinematics ----------------------------------------- *)

TopologyToKinematics::usage = "\
TopologyToKinematics[<top>, <set>] returns a rule to rewrite integrals \
from topology <top> in such a way that kinematic dependence is made \
explicit.";

(* TODO: plural <tops> *)

(* main *)
TopologyToKinematics[
  top:TopologyPattern[], set:SetupPattern[]] :=
  Module[
    {ts, fs, as,ps, vs},
    ts = Symbol[name /. top];
    fs = Last /@ (facs /. top);
    as = MapIndexed[Symbol["a" <> ToString[First[#2]]] & , fs];
    ps = Pattern[#, _] & /@ as;
    (*vs = Intersection[xs /. set, Variables[fs]];
    vs = SortBy[vs, Position[xs /. set, #] & ];*)
    vs = Select[xs /. set, !FreeQ[fs, #] & ];
    vs = vs /. x_^_ -> x;
    ts @@ ps -> ts @@ {as, vs}];

(* TODO: alias "Alpha" *)

(* TODO: alias "Setup" *)

(* trap *)
TopologyToKinematics[___] :=
  (Message[TopologyToKinematics::usage];
   Abort);

(* --- TopologyFromKinematics --------------------------------------- *)

(* TODO *)



















(* --- TopologyDerivatives ------------------------------------------ *)

TopologyDerivatives::usage = "\
TopologyDerivatives[<top>, <set>] returns rules for partial \
derivatives of topology <top> in invariants from kinematic setup \
<set>.";

(* overload: plural *)
TopologyDerivatives[
  tops_?TopologyListQ, set:Elective[SetupPattern[]]] :=
  Join @@ (TopologyDerivatives[#, set] & /@ tops);





(* main *)                                                              (* TODO *)
TopologyDerivatives[
  top:TopologyPattern["Solve"], set:SetupPattern[]] :=
  Module[
    {ts, vrs,v,rev, fs,fss, as,bs, tp, vqs,vls, vs, d,dqs,dls,ds, div, r},

    ts = Symbol[name /. top];  (* TODO: check *)

    vrs = # -> v[#] & /@ Complement[xs /. set, (ms /. set)^2];
    rev = First[Solve[
      Equal @@@ Select[cs /. set /. vrs, !FreeQ[#, v] & ],
      Last /@ vrs]] /. v -> Identity;

    fs = facs /. top /. rev;
    fss = First /@ fs;
    as = MapIndexed[Symbol["a" <> ToString[First[#2]]] & , fs];
    bs = Pattern[#, _] & /@ as;
    tp = Inner[Power, fss, -as, Times];

    (* quantities appearing only quadraticly: masses *)
    vqs = Variables[ms /. set];
    (* quantities appearing also linearly: external momenta *)
    vls = ps /. set;

    vs = Join[vqs, vls];

    dqs = Join @@ Outer[
      d[First[#1], #2^2] -> Coefficient[Last[#1], #2^2] & , fs, vqs];

    dls = Join @@ Outer[
      d[First[#1], #2] -> Coefficient[Last[#1], #2] +
        2*#2*Coefficient[Last[#1], #2^2] & , fs, vls];

    dls = Join @@ Outer[
      Append[First[#1], #2] -> Expand[#2*Last[#1]] /. (scps /. top) /. (cs /. set) & ,
      dls, vls];

    ds = Join[dqs, dls];

    div[m_] := Module[
      {r},
      r = Plus @@ MapThread[-#2*d[#1, m^2]/#1*tp & , {fss, as}];
      r = If[Head[#] === Plus, ts /@ #, ts @ #] & @ Expand[r /. ds];
      r = r //. ts[xx_] :> (xx /. (# -> 1 & /@ fss))*(ts @@ (-Exponent[xx, #] & /@ fss));
      r = Collect[r, ts[__], Factor];
      d[ts @@ bs, m] -> 2*m*r];

    div[qi_, qj_] := Module[
      {r},
      r = Plus @@ MapThread[-#2*d[#1, qi, qj]/#1*tp &, {fss, as}];
      r = ts /@ Expand[r /. ds];
      r = r /. ts[xx_] :> (xx /. (# -> 1 & /@ fss))*(ts @@ (-Exponent[xx, #] & /@ fss));
      r = Collect[r, ts[__], Factor];
      d[ts @@ bs, qi, qj] -> r];

    r = Join[
      div /@ vqs,
      Join @@ Outer[div, vls, vls]];

    RuleDelayed @@@ r /. d -> Composition[Hold, D]];










(* alias: no <set> *)
TopologyDerivatives[top:TopologyPattern["Setup"]] :=
  TopologyDerivatives[top, setp /. top];

(* trigger: no "Solve" -> ReduceTopology[] *)
TopologyDerivatives[top:TopologyPattern[], set:SetupPattern[]] :=
  TopologyDerivatives[ReduceTopology[top, set], set];

(* trap *)
TopologyDerivatives[___] :=
  (Message[TopologyDerivatives::usage];
   Abort);

(* N.B.:
- f(m^2): df/dm = 2 m df/dm^2,
- f(p^2): p_mu df/dp_mu = p_mu dp^2/dp_mu df/dp^2 = 2 p^2 df/dp^2.
*)

(* --- SetTopologyDerivatives --------------------------------------- *)

SetTopologyDerivatives::usage = "\
SetTopologyDerivatives[<top>, <set>] globally defines partial \
derivatives of topology <top> in invariants from kinematic setup \
<set>, viz. upvalues are set for the output of TopologyDerivatives[].";

(* overload: plural *)
SetTopologyDerivatives[
  tops_?TopologyListQ, set:Elective[SetupPattern[]]] :=
  SetTopologyDerivatives[#, set] & /@ tops;

(* main *)
SetTopologyDerivatives[
  top:TopologyPattern[], set:Elective[SetupPattern[]]] :=
  Module[
    {d,f, tds},
    tds = f @@@ ReleaseHold[TopologyDerivatives[top, set] /. D -> d];
    tds = Hold /@ tds /. d -> D /. f -> UpSetDelayed;
    ReleaseHold[tds]];                                                  (* TODO: return values -> True? *)


(* trap *)
SetTopologyDerivatives[___] :=
  (Message[SetTopologyDerivatives::usage];
   Abort);




































(* --- LookUp ------------------------------------------------------- *)

$LookUpMethods =
{"Cache", "Grep", "KLink"};

$LookUpCommands =
  Complement[$LookUpMethods, {"Cache"}];

Options[LookUp] =
{Method -> {"Cache", "KLink"},
 Path -> "."};

LookUp::usage = "\
LookUp[<int(s)>, [<key(s)>], [opts]] looks up given integral(s) from \
the generic expression <ints(s)> in the corresponding topology \
reduction table(s) and returns a corresponding list of rules.
These topologies may optionally be selected by the key(s) [<key(s)>] \
which should be symbols, strings or string patterns.
Via the option [Method], one can select the way the look-up is \
performed.  With \"Cache\" results of look-ups are cached, cached \
entries can be deleted again via LookUpClear[].  \"Grep\" issues a \
system call of 'grep' on \".mat\" Mathematica text files, \"KLink\" a \
MathLink call of A.V. Smirnov's KLink on \".kch\" Kyoto Cabinet \
files.  The location of the table can be adjusted by the [Path] \
option, else it is searched for on the $Path and the first match is \
taken.  The full file name can be given or just the path to the \
directory with files corresponding to the topology names.
Possible invokations:
- LookUp[\"TTA\", {{1, 1, 1, 1, 1, 1, 1}, {2, 1, 1, 1, 1, 1, 1}}], \
LookUp[\"TTA\", {1, 1, 1, 1, 1, 1, 1}],
- LookUp[TTA[1, 1, 1, 1, 1, 1, 1] + TXN[2, 1, 1, 1, 1, 1, 1]], \
LookUp[TTA[1, 1, 1, 1, 1, 1, 1]],
- LookUp[..., TTA], LookUp[..., \"TTA\"],
- LookUp[..., {TTA, TTB}], LookUp[..., TTA, TTB],
- LookUp[..., \"TT*\"], LookUp[..., RegularExpression[\"TT.*\"]].";

LookUp::file = "\
Could not find table file for \"`1`\" in \"`2`\" or on $Path.";

LookUp::call = "\
Error occured during call of external command \"`1`\".";

LookUp::entry = "\
Warning: Requested entry/entries not in the table for \"`1`\".";

LookUp::conversion = "\
Error in the conversion of integral expression(s) for \"`1`\".";

LookUp::complete = "\
Warning: Table for \"`1`\" is incomplete or query contains master \
integrals, requested `2` but found `3` entries.";

LookUp::consistent = "\
Table for \"`1`\" seems to be inconsistent, requested `2` but found \
`3` entries.";

LookUp::command = "\
Warning: Option value(s) for [Method] in `1` contains no valid look-up \
command.
Possible choices are from `2` (as strings).";

LookUp::keys = "\
Warning: Invalid selection key(s) `1` will be ignored.";

(* case: "Grep" *)
$LookUp[
  "Grep", t_String, iss:{{__Integer}..}, opts:OptionsPattern[]] :=
  Module[
    {pt,pr, fn, ess, res, ni,nr},
    pt = Path /. {opts} /. Options[LookUp] /. Path -> ".";
    (* try to expand Path *)
    pr = Quiet[Check[AbsoluteFileName[pt], Null]];
    (* check: Path name *)
    If[pr === Null,
       Message[TopoID::path, pt],
       pt = pr];
    (* find file in Path or on $Path *)
    fn = DeleteCases[
      FindFile /@ {FileNameJoin[{pt, t <> ".mat"}], t <> ".mat", pt},
      $Failed];
    (* check: file exists *)
    If[fn === {},
       Message[LookUp::file, t, pt];
       Return[{}]];
    fn = First[fn];
    (* escaped integral patterns *)
    ess = StringJoin[
      " -e '^", t, "\\[", Riffle[ToString /@ #, "\\,"], "\\]'"] &
        /@ Union[iss];
    (* system call of 'grep' *)
    res = Quiet[Check[
      ReadList[StringJoin["!grep -E", ess, " ", fn], String],
      Null]];
    (* check: system call *)
    If[res === Null,
       Message[LookUp::call, "grep"];
       Return[{$Failed}]];
    (* check: entry in table *)
    If[res === {},
       Message[LookUp::entry, t];
       Return[{}]];
    (* split at and strip from commas *)
    res = StringSplit[res, RegularExpression["\,$"]];
    (* conversion to expression *)
    res = Quiet[Check[ToExpression[Flatten[res]], Null]];
    (* check: conversion *)
    If[res === Null,
       Message[LookUp::conversion, t];
       Return[{$Failed}]];
    (* check: numbers *)
    {ni, nr} = Length /@ {iss, res};
    If[ni > nr,
       Message[LookUp::complete, t, ni, nr]];
    If[ni < nr,
       Message[LookUp::consistent, t, ni, nr];
       Return[{$Failed}]];
    (* result *)
    res];

(* case: "KLink" *)
$LookUp[
  "KLink", t_String, iss:{{__Integer}..}, opts:OptionsPattern[]] :=
  Module[
    {pt,pr, fn, ln, ck, lu, res, ni,nr},
    pt = Path /. {opts} /. Options[LookUp] /. Path -> ".";
    (* try to expand Path *)
    pr = Quiet[Check[AbsoluteFileName[pt], Null]];
    (* check: Path name *)
    If[pr === Null,
       Message[TopoID::path, pt],
       pt = pr];
    (* find file in Path or on $Path *)
    fn = DeleteCases[
      FindFile /@ {FileNameJoin[{pt, t <> ".kch"}], t <> ".kch", pt},
      $Failed];
    (* check: file exists *)
    If[fn === {},
       Message[LookUp::file, t, pt];
       Return[{}]];
    (* remove extension *)
    fn = StringReplace[First[fn], RegularExpression["\\.kch$"] -> ""];
    (* open MathLink *)
    ln = Catch[Quiet[
      Scan[
        Function[l, If[l =!= $Failed, Throw[l]]] @
          Block[{WriteString, Print}, Install[#]] & ,
        {"KLink", "KLink64"}];
      Null]];
    (* check: MathLink call *)
    If[ln === Null,
       Message[LookUp::call, "MathLink"];
       Return[{$Failed}]];
    (* open KLink *)
    ck = Quiet[Global`QRead[fn]];
    (* check: KLink call *)
    If[ck === False,
       Message[LookUp::call, "KLink"];
       Return[{$Failed}]];
    (* helper: look-up for single integral *)
    lu[is_] := Module[
      {k, v},
      k = StringJoin[t, "[", Riffle[ToString /@ is, ","], "]"];
      v = Quiet[Check[Global`QSafeGet[fn, k], Null]];
      (* check: call, entry *)
      If[MemberQ[{Null, False}, v],
         Return[v]];
      (* conversion to expression *)
      v = Quiet[Check[ToExpression[v], Null]];
      (* check: conversion *)
      If[v === Null, Return[$Failed]];
      (* result *)
      Symbol[t] @@ is -> v];
    (* apply to each integral *)
    res = lu /@ iss;
    (* clean up *)
    Quiet[
      Global`QClose[fn];
      Uninstall[ln]];
    (* check: KLink call *)
    If[MemberQ[res, Null],
       Message[LookUp::call, "KLink"];
       res = DeleteCases[res, Null];
       If[res === {},
          Return[{$Failed}]]];
    (* check: entry in table *)
    If[MemberQ[res, False],
       Message[LookUp::entry, t];
       res = DeleteCases[res, False]];
    (* check: conversion *)
    If[MemberQ[res, $Failed],
       Message[LookUp::conversion, t];
       res = DeleteCases[res, $Failed];
       If[res === {},
          Return[{$Failed}]]];
    (* check: numbers *)
    {ni, nr} = Length /@ {iss, res};
    If[ni > nr,
       Message[LookUp::complete, t, ni, nr]];
    If[ni < nr,
       Message[LookUp::consistent, t, ni, nr];
       Return[{$Failed}]];
    (* result *)
    res];

(* main: select Method *)
LookUp[
  t_String, iss:{{__Integer}..}, opts:OptionsPattern[]] :=
  Module[
    {me, ch, cp, old,new, res, pt, pf, lu, req},
    (* check: Method, "Cache" *)
    me = Flatten[{OptionValue[Method]}];
    cp = Complement[me, $LookUpMethods];
    If[cp =!= {},
       Message[Method::invalid, cp, $LookUpMethods];
       me = Complement[me, cp]];
    If[Intersection[me, $LookUpCommands] === {},
       Message[LookUp::command, me, $LookUpCommands];
       PrependTo[me, "Grep"]];
    (* separate old and new requests *)
    {old, new} = {{}, iss};
    res = {};
    If[MemberQ[me, "Cache"],
       old = First /@ DownValues[LookUp] /. LookUp -> List;
       old = Last /@ Select[First /@ old, First[#] === t & ];
       new = Complement[iss, old];
       old = Complement[iss, new];
       res = LookUp[t, #] & /@ old];
    (* helper: expand path names *)
    pf = First[DeleteCases[{FileNames[#], {#}}, {}]] & ;
    (* check: Path *)
    pt = Path /. {opts} /. Options[LookUp] /. Path -> ".";
    If[Head[pt] =!= List, pt = {pt}];
    pt = Union @@ (pf /@ pt);
    (* helper: use single location *)
    lu = $LookUp[
      First[Intersection[me, $LookUpCommands]],                      (* TODO: what's the standard, First[] or Last[]? *)
      t, #1, Path -> #2, opts, Sequence @@ Options[Lookup]] & ;
    (* pass new requests to specific routine for each location *)
    req = {};
    If[new =!= {},
       req = DeleteDuplicates[Join @@ (lu[new, #] & /@ pt)]];
    (* cache new requests *)
    If[MemberQ[me, "Cache"], Scan[
      Set[LookUp[t, List @@ First[#]], #] & ,
      Reverse[DeleteCases[req, $Failed]]]];
    (* results: join old and new request *)
    Join[res, req]];

(* overload: singular *)
LookUp[t_String, is:{___Integer}, opts:OptionsPattern[]] :=
  LookUp[t, {is}, opts];

(* overload: expression, selection keys *)                                (* TODO: use $CheckKeys *)
LookUp[
  x_, ks___:{}, opts:OptionsPattern[]] :=
  Module[
    {sl,cp, f, iss, ts, res},
    (* check: keys *)
    sl = Select[Flatten[{ks}], MemberQ[
      {Symbol, String, StringExpression, RegularExpression},
      Head[#]] & ];
    cp = Complement[Flatten[{ks}], sl];
    If[cp =!= {},
       Message[LookUp::keys, cp]];
    (* symbols -> strings *)
    sl = If[Head[#] === Symbol, ToString[#], #] & /@ sl;
    (* helper: matching function *)
    f = Or @@ (Function[k, StringMatchQ[#, k]] /@ sl) & ;
    (* all integrals *)
    iss = Variables[Cases[x, _Symbol[___Integer], {0, Infinity}]];
    (* select heads by keys *)
    If[sl =!= {},
      iss = Select[iss, f[ToString[Head[#]]] & ]];
    (* occuring topologies *)
    ts = Union[Head /@ iss];
    (* call for each topology *)
    res = LookUp[
      ToString[#], Cases[iss, #[is___Integer] -> {is}], opts] & /@ ts;
    (* result: remove failed lookups *)
    res = DeleteCases[res, $Failed | {}];
    Join @@ res];

(* trap *)
LookUp[___] :=
  (Message[LookUp::usage];
   Abort[]);

(*Off[LookUp::entry];*)
(*Off[LookUp::complete];*)

(* N.B.:
- Case "KLink" is an adapted version of Takahiro's code.
*)

(* --- LookUps ------------------------------------------------------ *)

LookUps::usage = "\
LookUps[] returns for each topology a list of cached results from \
calls of LookUp[].";

(* main *)
LookUps[] :=
  Module[
    {res},
    res = First /@ DownValues[LookUp] /. LookUp -> List;
    res = Cases[First /@ res, {_String, {__Integer}}];
    res = GatherBy[res, First];
    res = {Part[#, 1, 1], Last /@ #} & /@ res];

(* trap *)
LookUps[___] :=
  (Message[LookUps::usage];
   Abort[]);

(* TODO:
- ks___:{} -- selection keys (see below)
*)

(* --- LookUpClear -------------------------------------------------- *)

LookUpClear::usage = "\
LookUpClear[[<key(s)>]] simply deletes all cached results of LookUp[].
Only matching results are deleted in case key(s) [<key(s)>] have been \
specified as symbols, strings or string patterns.
Possible invokations:
- LookUpClear[TTA], LookUpClear[\"TTA\"],
- LookUpClear[{TTA, TTB}], LookUpClear[TTA, TTB],
- LookUpClear[\"TT*\"], LookUpClear[RegularExpression[\"TT.*\"]].";

LookUpClear::keys =
  LookUp::keys;

(* main *)
LookUpClear[ks___:{}] :=
  Module[
    {sl, cp, f, res},
    (* check: keys *)
    sl = Select[Flatten[{ks}], MemberQ[
      {Symbol, String, StringExpression, RegularExpression},
      Head[#]] & ];
    cp = Complement[Flatten[{ks}], sl];
    If[cp =!= {},
       Message[LookUpClear::keys, cp]];
    (* symbols -> strings *)
    sl = If[Head[#] === Symbol, ToString[#], #] & /@ sl;
    (* helper: matching function *)
    f = Or @@ (Function[k, StringMatchQ[#, k]] /@ sl) & ;
    (* all cached results *)
    res = First /@ DownValues[LookUp] /. LookUp -> List;
    res = Cases[First /@ res, {_String, {__Integer}}];
    (* select by topology *)
    If[sl =!= {},
       res = Select[res, f[First[#]] & ]];
    (* clear matching results *)
    Scan[Unset[LookUp[Sequence @@ #]] & , res];
    Null];

(* --- TAP ---------------------------------------------------------- *)

Options[TAP] =
  Options[LookUp];

TAP::usage = "\
TAP[[<keys>], [opts]] is a dynamic replacement rule for reduction of \
integrals in an expression <exp> as an alias to LookUp[<exp>, [opts]].";

(* main *)
TAP[ks___:{}, opts:OptionsPattern[]] :=
{i:_Symbol[___Integer] :> Part[
  LookUp[i, ks, opts] /. $Failed | {} -> {i -> i}, 1, 2]};

(* trap *)
TAP[___] :=
  (Message[TAP:usage];
   Abort[]);




(* --- adopted from "minimize.m" in the "results/math/" directory on "/x1/" --- *)














(*

- IntegralRelations[
    <ints> | <expr>,
    <tops>,
    <setup>,
    [opts: those of ToMapping[], MapIntegralToTopology[], LookUp[],
     Method -> {"Ordered", ...}]
  ]

  returns (a) replacement rules for integral and (b) possibly additional
  relations among integrals (indirect reductions; could be useful as
  checks).  With "Ordered" in Method the order of topologies in <tops>
  is used to find relations stepwise, otherwise all possible relations
  are attempted to be found in one step.

  TODO:

  - Replace LookUp[] by generic function passed via options

  - Rules option to pass additional information (e.g. for updates)

  - optionally return steps

  - optionally extend given set of relations

  - add keywords to Method such that several steps may be selected

  - filter and inherit options properly

  - move to TopoID/Calculate.m

  - EliminationOrder -> {<list of topology names>} | All | Full -> use all topologies given, regardless of appearing integrals

*)


(* TODO: use inheriting of options *)
Options[IntegralRelations] =
{Path -> ".",
 Method -> {"Rules", "Minimize", "KLink"}};

IntegralRelations[
  x_, tops_?TopologyListQ, setup:SetupPattern[],
  opts:OptionsPattern[]] :=
  Module[
    {ints, utops, truls, iruls, imaps, tmp, top, ggg, vars,eqns,sols, me, optr, res1, res2},

    optr = FilterRules[{opts}, Options[LookUp]];
    optr = optr /. {Rule[Method, m_] :> Rule[Method, Intersection[m, $LookUpMethods]]};

    me = OptionValue[Method];

    Print["Extract integrals..."];
    ints[0] = TopologyIntegralList[x];
    Print["-> ", Length[ints[0]], "."];

    utops[0] = ToString /@ Union[Head /@ ints[0]];
    utops[1] = Select[tops, MemberQ[utops[0], name /. #] & ];

    Print["Construct topology rules..."];
    truls = TopologyToRules[utops[1]];

    (* TODO: make optional *)
    If[MemberQ[me, "Rules"],
       Print["Apply topology rules..."];
       ints[1] = TopologyCalculate[ints[0], truls];
       ,
       ints[1] = ints[0];
     ];
    iruls[1] = MapThread[Rule, {ints[0], ints[1]}];
    ints[2] = Union[ints[1]];
    Print["-> ", Length[ints[2]], "."];

    Print["Create internal representations..."];
    imaps[2] = ToMapping[ints[2]];
    iruls[2] = Reverse /@ MappingToRule[imaps[2]];
    {tmp, ints[2]} = MapTopologyToIntegral[imaps[2], utops[1]];

    (* TODO: make optional *)
    If[MemberQ[me, "Minimize"],
       Print["Identify integrals..."];
       {imaps[3], ints[3]} = MinimizeIntegrals[ints[2]];
       iruls[3] = MappingToSymbolRule[imaps[3]];
       ,
       ints[3] = ints[2];
       iruls[3] = {};
     ];
    Print["-> ", Length[ints[3]], "."];

    Print["Find representations..."];
    tmp = ints[3];
    iruls[4] = {};
    Do[
      (* all integrals already mapped *)
      If[tmp === {},
         Break[]];

      (* find integral representations *)
      Print[name /. top, ": ", Length[tmp], " integrals left."];
      iruls[4, name /. top] = MapIntegralToTopology[tmp, top, setup, Verbosity -> 0];  (* TODO: inherit *)

      (* apply topology rules, reduction and identifications *)
      Print["Rules, reduce, identify, factor..."];
      iruls[5, name /. top] = TopologyCalculate[iruls[4, name /. top], truls];
      iruls[5, name /. top] = iruls[5, name /. top] /. Quiet[LookUp[iruls[5, name /. top], Sequence @@ optr]] /. Global`Acc -> Identity /. (rs /. setup);
      iruls[5, name /. top] = TopologyCalculate[iruls[5, name /. top], truls];

      (* apply identifications obtained up so far *)
      iruls[6, name /. top] = iruls[5, name /. top] /. iruls[2] /. iruls[3] /. iruls[4];
      iruls[6, name /. top] = First[#] -> Collect[Last[#], TopologyIntegralPattern[], Factor] & /@ iruls[6, name /. top];
      iruls[6, name /. top] = DeleteDuplicates[iruls[6, name /. top]];

      (* mapped integrals *)
      iruls[7, name /. top] = Cases[iruls[6, name /. top], HoldPattern[i_ -> i_]];
      ints[7, name /. top] = ToString /@ First /@ iruls[7, name /. top];
      Print[Length[ints[7, name /. top]], " master identifications."];
      (* remove mapped integrals *)
      tmp = Select[tmp, FreeQ[ints[7, name /. top], name /. #] & ];

      (* auxiliary and related integrals *)
      iruls[8, name /. top] = Complement[iruls[6, name /. top], iruls[7, name /. top]];
      Print["Solve..."];  (* TODO: use RowReduce[] instead? *)
      eqns = Equal @@@ iruls[8, name /. top];
      vars = Join[Union[Last /@ iruls[3]], TopologyIntegralList[iruls[8, name /. top]]];  (* eliminate "masters" first, then "auxiliaries" *)
      sols = Quiet[Solve[eqns, vars]];
      sols = If[sols === {}, sols, First[sols]];
      sols = First[#] -> Collect[Last[#], TopologyIntegralPattern[], Factor] & /@ sols;

      (* check *)
      chck = Cases[sols, HoldPattern[_ -> 0]];
      If[chck =!= {},
         Print["Warning: ", First /@ chck, " seem to be zero."]];

      (* split by master/auxiliary *)
      ints[8, name /. top] = ToString /@ Select[First /@ sols, MemberQ[Union[Last /@ iruls[3]], #] & ];
      Print[Length[ints[8, name /. top]], " master relations."];
      Print[Length[sols] - Length[ints[8, name /. top]], " auxiliary relations."];

      (* remove mapped integrals *)
      tmp = Select[tmp, FreeQ[ints[8, name /. top], name /. #] & ];

      (* add to mapping rules *)
      iruls[4] = Join[iruls[4], sols];

      , {top, utops[1]}];


    (* TODO: final report *)


    (* TODO: combine all the rules *)
    res1 = First[#] -> ((((Last[#] /. iruls[2]) /. iruls[3]) /. iruls[4]) /. (Reverse /@ iruls[2])) & /@ iruls[1];
    res2 = Cases[iruls[4], r_Rule /; TopologyIntegralQ[First[r]]] /. (Reverse /@ iruls[2]);

    (* TODO: correct numbers *)
    Print["Found ", Length[res1], " relations among master integrals and ", Length[res2], " auxiliary reductions."];

    Join[res1, res2]];



(* ------------------------------------------------------------------ *)









(* --- LaportaInit -------------------------------------------------- *)

LaportaInit::usage = "\
LaportaInit[<top>] returns shell script code setting the \
initialization limits for the topology <top> according to its \
definition.";

(* plural *)
LaportaInit[tops_?TopologyListQ] :=
  LaportaInit /@ tops;

(* main: "Cuts" *)
LaportaInit[
  top:TopologyPattern["Cuts"]] :=
  Module[
    {fs, nf, cis, is, i, mns,mxs, mn,mx, sps,sns, cms},
    (* factors, their number, cuts *)
    fs = First /@ (facs /. top);
    nf = Length[fs];
    cis = cuts /. top;
    (* no negative indices on cut lines *)
    is = Intersection @@ cis;
    mns = Table[If[MemberQ[is, i], 1, -1], {i, nf}];
    (* no positive indices on pure numerators *)
    mxs = If[NumeratorSymbolQ[#], 0, 2] & /@ fs;
    (* at least as many lines as smallest cut *)
    mn = Min[Length /@ cis];
    (* at most as many lines as actual denominators *)
    mx = Count[fs, _?DenominatorSymbolQ];
    (* on each line allow an additional dot, cross *)
    sps = Table[If[i < mn || i > mx, 0, i + 1], {i, 0, nf}];
    sns = Table[If[i < mn || i > mx, 0, 1], {i, 0, nf}];
    (* comments *)
    cms =
    {StringJoin["factor symbols: ", Riffle[ToString /@ fs, ","]],
     StringJoin["cuts: ", Riffle[
       {"(", Riffle[ToString /@ #, ","], ")"} & /@ cis, ", "]]};
    (* result *)
    LaportaLimit[
      name /. top, "init", Sequence @@ cms, mns, mxs, sps, sns]];

(* main: "Subt" *)
LaportaInit[
  top:TopologyPattern["Subt"]] :=
  Module[
    {fs, nf, st, mns,mxs, snvs, mn,mx, i, sps,sns, cm},
    (* factors, their number, sub-topologies *)
    fs = First /@ (facs /. top);
    nf = Length[fs];
    st = subt /. top;
    (* allow negative indices everywhere *)
    mns = Table[-1, {nf}];
    (* no positive indices on pure numerators *)
    mxs = If[NumeratorSymbolQ[#], 0, 2] & /@ fs;
    (* group of smallest (non-vanishing) subsets *)
    snvs = Select[
      Reverse[st], And @@ (DenominatorSymbolQ /@ First[#]) & , 1];
    (* at least as many lines as length of smallest subset *)
    mn = Length[Part[snvs, 1, 1]];
    (* at most as many lines as actual denominators *)
    mx = Count[fs, _?DenominatorSymbolQ];
    (* on each line allow an additional dot, cross *)
    sps = Table[If[i < mn || i > mx, 0, i + 1], {i, 0, nf}];
    sns = Table[If[i < mn || i > mx, 0, 1], {i, 0, nf}];
    (* comment *)
    cm = StringJoin["factor symbols: ", Riffle[ToString /@ fs, ","]];
    (* result *)
    LaportaLimit[name /. top, "init", cm, mns, mxs, sps, sns]];

(* trap *)
LaportaInit[___] :=
  (Message[LaportaInit::usage];
   Abort[]);

(* Needs:
- LaportaLimit[].
*)

(* --- LaportaScan -------------------------------------------------- *)

LaportaScan::usage = "\
LaportaScan[<int(s)>] returns shell script code setting appropriate \
limits for the topologies appearing in the integral list <int(s)> in \
order to reduce them.";

(* atomic: same topology *)
$LaportaScan[
  ints_List] :=
  Module[
    {scan, tn, ln, intr,intt, mnxs, spns, msng},
    (* helper: number of lines, sums of denominators, numerators *)
    scan = Count[#, i_ /; i > 0] ->
      Plus @@@ {Select[#, Positive], Select[#, Negative]} & ;
    (* notation *)
    tn = If[# === List, "<top>", ToString[#]] & @ Head[First[ints]];
    ln = Length[First[ints]];
    intr = List @@@ ints;
    intt = Transpose[intr];
    (* range minima, maxima *)
    mnxs = {Min[#, 0] & /@ intt, Max[#, 0] & /@ intt};
    (* for each line number: maximum sums of denominators, numerators *)
    spns = GatherBy[scan /@ intr, First];
    spns = Part[#, 1, 1] -> Last /@ # & /@ spns;
    spns = MapAt[{Max[First /@ #], -Min[Last /@ #]} & , #, 2] & /@ spns;
    (* account for missing line numbers *)
    msng = Complement[Range[0, ln], First /@ spns];
    spns = Join[spns, # -> {0, 0} & /@ msng];
    spns = Transpose[Last /@ SortBy[spns, First]];
    (* result *)
    LaportaLimit[tn, "scan", Sequence @@ mnxs, Sequence @@ spns]];

(* main: different topology *)
LaportaScan[ints_List] :=
 $LaportaScan /@ GatherBy[ints, Head];

(* trap *)
LaportaScan[___] :=
  (Message[LaportaScan::usage];
   Abort[]);

(* Needs:
- LaportaLimit[].
*)

(* TODO:

- min, max: at least 0

- Check the input, aliases.

*)

(* --- LaportaEncode ------------------------------------------------ *)

Options[LaportaEncode] =
{TopologyIndex -> 0,
 Min -> -5,
 Max -> +5};

LaportaEncode::usage = "\
LaportaEncode[<int(s)>, [opts]] returns a single integer encoding all \
indices of the integral(s) <int(s)>.
The options [Min] and [Max] allow setting the technical limits as \
integers or lists of integers, [TopologyIndex] can be used to \
distinguish between different integral families.
The global look-up tables TopologyIndex and TopologyLimits can be used \
for storing these options.
Possible invokations:
- LaportaEncode[{1, 1, 1, 1, 1, 1, 1}],
- LaportaEncode[{{1, 1, 1, 1, 1, 1, 1}, {1, 1, 1, 1, 1, 1, 0}}],
- LaportaEncode[TTA[1, 1, 1, 1, 1, 1, 1]],
- LaportaEncode[{TTA[1, 1, 1, 1, 1, 1, 1], TTA[1, 1, 1, 1, 1, 1, 0]}].";

LaportaEncode::length = "\
Length of index list `1` incompatible with lengths of lists specified \
via the options [Min] `2` or [Max] `3`.";

LaportaEncode::range = "\
Indices at position(s) `1` of `2` out of the range specified via \
options [Min] and [Max].";

LaportaEncode::index = "\
Option value of [TopologyIndex] `1` not an integer.";

LaportaEncode::minmax = "\
Option values of [Min] `1` or [Max] `2` not specified as integers or \
list of integers.";

(* main: atomic *)
$LaportaEncode[
  ti_Integer, is:{__Integer}, mn:{__Integer}, mx:{__Integer}] :=
  Module[
    {ni, kn,kx, ck, rs,os, ds,dn,dx, ns,nn,nx, lds,ldn,ldx, lns,lnn,lnx,
     sds,sdn,sdx, sns,snn,snx, cs},
    (* check: length *)
    ni = Length[is];
    If[Length[mn] != ni || Length[mx] != ni,
       Message[LaportaEncode::length, ni, mn, mx];
       Abort[]];
    {kn, kx} = {mn - 1, mx + 1};
    (* check: range *)
    ck = MapThread[#1 < #2 || #1 > #3 || #2 > #3 - 2 & , {is, kn, kx}];
    If[Or @@ ck,
       Message[LaportaEncode::range, Join @@ Position[ck, True], is];
       Abort[]];
    {rs, os} = {kx - kn + 1, is - kn};
    (* denominators *)
    {ds, dn, dx} = Cases[#, k_?Positive] & /@ {is, kn, kx};
    (* numerators *)
    {ns, nn, nx} = Cases[#, k_?Negative] & /@ {is, kn, kx};
    (* numbers of positive, negative indices *)
    {lds,ldn,ldx, lns,lnn,lnx} = Length /@ {ds,dn,dx, ns,nn,nx};
    (* sums of positive, negative indices *)
    {sds,sdn,sdx, sns,snn,snx} = Plus @@@ {ds,dn,dx, -ns,-nn,-nx};
    (* result: encoded indices *)
    rs = Join[{ldx, sdx, snn} - {ldn, sdn, snx} + 1, rs];
    os = Join[{ti}, {lds, sds, sns} - {ldn, sdn, snx}, os];
    cs = Reverse[FoldList[Times, 1, Reverse[rs]]];
    Dot[cs, os]];

(* overload: plural *)
LaportaEncode[iss_?MatrixQ, opts:OptionsPattern[]] :=
  LaportaEncode[#, opts] & /@ iss;

(* main: frontend *)
LaportaEncode[
  is:{__Integer}, opts:OptionsPattern[]] :=
  Module[
    {ti,mn,mx},
    {ti, mn, mx} = OptionValue[{TopologyIndex, Min, Max}];
    (* check: TopologyIndex *)
    If[Head[ti] =!= Integer,
       Message[LaportaEncode::index, ti];
       Abort[]];
    (* check: Min, Max *)
    If[IntegerQ[mn],
       mn = mn & /@ is];
    If[IntegerQ[mx],
       mx = mx & /@ is];
    If[!MatchQ[mn, {__Integer}] || !MatchQ[mx, {__Integer}],
       Message[LaportaEncode::minmax, mn, mx];
       Abort[]];
    (* result *)
    $LaportaEncode[ti, is, mn, mx]];

(* shortcut: plural *)
LaportaEncode[ints:{_Symbol[__Integer]..}, opts:OptionsPattern[]] :=
  LaportaEncode[#, opts] & /@ ints;

(* shortcut: singular *)
LaportaEncode[
  int:_Symbol[__Integer], opts:OptionsPattern[]] :=
  Module[
    {os, tis, tls},
    os = {};
    (* topology index *)
    tis = TopologyIndex[Head[int]];
    If[Head[tis] === Integer,
       AppendTo[os, TopologyIndex -> tis]];
    (* topology limits *)
    tls = TopologyLimits[Head[int]];
    If[MatchQ[tls, {{__Integer}, {__Integer}}],
       os = Join[os, {Min -> First[tls], Max -> Last[tls]}]];
    LaportaEncode[List @@ int, opts, Sequence @@ os]];

(* trap *)
LaportaEncode[___] :=
  (Message[LaportaEncode::usage];
   Abort[]);

(* Needs:
- TopologyIndex[], TopologyLimits[].
*)

(* N.B:
- Try to get topology index and limits from global look-up tables
  TopologyIndex, TopologyLimits.
*)

(* --- LaportaDecode ------------------------------------------------ *)

Options[LaportaDecode] =
{Min -> Table[-5, {20}],
 Max -> Table[+5, {20}]};

LaportaDecode::usage = "\
LaportaDecode[<int(s)>, [opts]] decodes the single integer(s) <int(s)> \
back to integral indices.
Therefore the options [Min] and [Max] must set the technical limits as \
lists of integers.
The global look-up tables TopologyIndex and TopologyLimits are \
considered in the end for attaching topology symbols and checking the \
used limits.
Possible invokations:
- LaportaDecode[712357768284, Min -> Table[-5, {7}], Max -> Table[5, \
{7}]],
- LaportaDecode[{712357768284, 831077962449}, Min -> Table[-5, {7}], \
Max -> Table[5, {7}]].";

LaportaDecode::length = "\
Incompatible lengths of lists specified via the options [Min] `1` or \
[Max] `2`.";

LaportaDecode::range = "\
Range not specified properly via options [Min] and [Max] at \
position(s) `1`.";

LaportaDecode::index = "\
Recovered index did not pass consistency check.";

LaportaDecode::minmax = "\
Option values of [Min] `1` or [Max] `2` not specified as list of \
integers with equal lengths.";

LaportaDecode::indices = "\
Warning: Multiple topologies with topology index \"`1`\" found.";

LaportaDecode::limits = "\
Warning: Technical limits of `1` do not match the option values [Min], \
[Max].";

(* main *)
$LaportaDecode[
  i_Integer, mn:{__Integer}, mx:{__Integer}] :=
  Module[
    {f, kn,kx, ck, cs, qs, j, is, ds,dn,dx, ns,nn,nx, lds,ldn,ldx,
     lns,lnn,lnx, sds,sdn,sdx, sns,snn,snx},
    f = Function[{x, y}, Most[Reverse[FoldList[
      With[{k = Mod[First[#1], #2]}, {(First[#1] - k) / #2, k}] & ,
      {x, 0}, Reverse[y]]]]];
    (* check: length *)
    If[Length[mn] != Length[mx],
       Message[LaportaEncode::length, mn, mx];
       Abort[]];
    {kn, kx} = {mn - 1, mx + 1};
    (* check: range *)
    ck = MapThread[#1 > #2 - 2 & , {kn, kx}];
    If[Or @@ ck,
       Message[LaportaDecode::range, Join @@ Position[ck, True]];
       Abort[]];
    (* recover indices *)
    cs = kx - kn + 1;
    qs = f[i, cs];
    j = Part[qs, 1, 1];
    qs = Last /@ qs;
    is = kn + qs;
    (* numbers, sums of positive, negative indices *)
    {ds, dn, dx} = Cases[#, k_ /; k > 0] & /@ {is, kn, kx};
    {ns, nn, nx} = Cases[#, k_ /; k < 0] & /@ {is, kn, kx};
    {lds,ldn,ldx, lns,lnn,lnx} = Length /@ {ds,dn,dx, ns,nn,nx};
    {sds,sdn,sdx, sns,snn,snx} = Total /@ {ds,dn,dx, -ns,-nn,-nx};
    (* check: index *)
    cs = {ldx, sdx, snn} - {ldn, sdn, snx} + 1;
    qs = f[j, cs];
    j = Part[qs, 1, 1];
    qs = Last /@ qs;
    If[{lds, sds, sns} =!=
       {ldn + qs[[1]], sdn + qs[[2]], snx + qs[[3]]},
       Message[LaportaDecode::index];
       Abort[]];
    (* result: topology index, integral *)
    {j, is}];

(* overload: plural *)
LaportaDecode[is:{__Integer}, opts:OptionsPattern[]] :=
  LaportaDecode[#, opts] & /@ is;

(* main: frontend *)
LaportaDecode[
  i_Integer, opts:OptionsPattern[]] :=
  Module[
    {mn,mx, res, tss, tls},
    {mn, mx} = OptionValue[{Min, Max}];
    (* check: Min, Max *)
    If[!MatchQ[mn, {__Integer}] || !MatchQ[mx, {__Integer}] ||
         Length[mn] != Length[mx],
       Message[LaportaDecode::minmax, mn, mx];
       Abort[]];
    (* decode *)
    res = $LaportaDecode[i, mn, mx];
    (* topology index *)
    tss = Select[DownValues[TopologyIndex], Last[#] === First[res] & ];
    tss = Part[#, 1, 1] & /@ (First /@ tss);
    If[Length[tss] > 1,
       Message[LaportaDecode::indices, First[res]];
       tss = {}];
    If[tss =!= {},
       tss = First[tss]];
    (* topology limits *)
    If[tss =!= {},
       tls = TopologyLimits[tss];
       If[MatchQ[tls, {{__Integer}, {__Integer}}] &&
            (First[tls] =!= mn || Last[tls] =!= mx),
          Message[LaportaDecode::limits, tss]]];
    (* result *)
    res = If[
      tss =!= {},
      tss @@ Last[res],
      Last[res]];
    res];

(* trap *)
LaportaDecode[___] :=
  (Message[LaportaDecode::usage];
   Abort[]);

(* Needs:
- TopologyIndex[], TopologyLimits[].
*)

(* N.B:
- Try to find topology symbol from topology index in global look-up
  table TopologyIndex and check TopologyLimits.
*)

(* --- LaportaHull -------------------------------------------------- *)

LaportaHull::usage = "\
LaportaHull[<ints>, [<top>]] builds the hull of seed integrals <ints>, \
that is a complete classification into distinct sectors of topology \
<top>.
Possible invokations:
- LaportaHull[{{1, 1, 1, 1, 1, 1, 1}, ...}, <top>],
- LaportaHull[{TTA[1, 1, 1, 1, 1, 1, 1], ...}, <top>].";

Options[LaportaHull] =
{Verbosity -> False};

(* main *)  (* TODO: use <top>-less version *)
LaportaHull[
  ints_List, top:TopologyPattern["Subt"], opts:OptionsPattern[]] /;
(MatrixQ[ints] && Length[First[ints]] === Length[facs /. top] ||
 Head[First[ints]] === Symbol[name /. top]) :=
  Module[
    {subu, sector,check,pick, vb, res, lns},

    subu = subt /. top;
    subu = Sort /@ # & /@ subu;
    subu = Sort[Sort /@ subu];

    sector = Function[is, Join @@ Position[is, i_Integer /; i > 0]];
    (*check = Function[{s, t}, Or @@ (Sort[#] === s & /@ t)];*)
    pick = Function[
      s, If[# =!= {}, Part[#, 1, 1], {}] &
        @ Select[subu, MemberQ[#, s] & , 1]];

    vb = OptionValue[Verbosity] /. VerbosityRules;

    Report[vb, 1, "Initial number of integrals: ", Length[ints], "."];

    (* assign and group by sectors, sort by complexity *)
    res = GatherBy[sector[#] -> # & /@ ints, First];
    res = Part[#, 1, 1] -> Last /@ # & /@ res;
    res = SortBy[res, First];

    lns = Length /@ Last /@ res;
    Report[vb, 1, "Number of naive sectors: ", Length[res], "."];
    Report[vb, 2, "Integrals in sectors:\n  ", lns, "."];

    (* use distinct sectors from topology *)
    res = GatherBy[pick[First[#]] -> Last[#] & /@ res, First];
    res = Part[#, 1, 1] -> Union @@ (Last /@ #) & /@ res;
    res = SortBy[res, First];

    (* discard vanishing sectors *)
    res = DeleteCases[res, HoldPattern[{} -> _]];
    (*res = Select[res, And @@ (Function[z, Complement[First[#], z] =!= {}] /@ (zero /. top)) & ];*)

    (* integrals within sectors and sectors by complexity *)
    res = First[#] -> SortBy[Last[#], LaportaEncode] & /@ res;  (* LaportaEnocde[] options *)
    res = SortBy[res, LaportaEncode[Part[#, -1, 1]] & ];

    lns = Length /@ Last /@ res;
    Report[vb, 1, "Number of distinct sectors: ", Length[res], "."];
    Report[vb, 2, "Integrals in sectors:\n  ", lns, "."];

    Report[vb, 1, "Final number of integrals: ", Total[lns], "."];

    res];

(* main: no <top> *)
LaportaHull[
  ints_List, opts:OptionsPattern[]] /; MatrixQ[List @@@ ints] :=
  Module[
    {sector,check,pick, vb, res, lns},

    sector = Function[is, Join @@ Position[is, i_Integer /; i > 0]];

    vb = OptionValue[Verbosity] /. VerbosityRules;

    Report[vb, 1, "Initial number of integrals: ", Length[ints], "."];

    (* assign and group by sectors, sort by complexity *)
    res = GatherBy[sector[#] -> # & /@ ints, First];
    res = Part[#, 1, 1] -> Last /@ # & /@ res;
    res = SortBy[res, First];

    lns = Length /@ Last /@ res;
    Report[vb, 1, "Number of naive sectors: ", Length[res], "."];
    Report[vb, 2, "Integrals in sectors:\n  ", lns, "."];

    res];

(* trap *)
LaportaHull[___] :=
  (Message[LaportaHull::usage];
   Abort[]);

(* N.B.:
- Order of definitions must be like this.
*)

(* --- LaportaFill -------------------------------------------------- *)

LaportaFill::usage = "\
LaportaFill[<ints>] fills a list of integrals <ints> with all \n
integrals in simpler sectors.";

(* main: no <top> *)
LaportaFill[
  ints_List] /; MatrixQ[List @@@ ints] :=
  Module[
    {c = 1, f, g},
    (* add each subsector *)
    f = ReplaceList[#, t_Symbol[a___, b_, c___] -> t[a, 0, c]] & ;
    g = Function[i, Status[Console, c++]; FixedPoint[Union @@ (f /@ #) & , {i}]];
    Union @@ (g /@ ints)];

(* trap *)
LaportaFill[___] :=
  (Message[LaportaFill::usage];
   Abort[]);

(* N.B.:
- Alternative, but more time consuming:
  FixedPoint[Union @@ (f /@ #) & , ints]];
*)

(* --- LaportaSeed -------------------------------------------------- *)

LaportaSeed::usage = "\
LaportaSeed[<hull>] fills up a hull of seed integrals <hull> returned \
by LaportaHull[] completely.";

Options[LaportaSeed] =
{Verbosity -> False};

Options[LaportaSeed] =
  Join @@ Options /@ {LaportaSeed, LaportaEncode};

(* atomic: single sector *)
$LaportaSeed[
  ints_List] :=
  Module[
    {intr,intt, mns,mxs,sps,sns, mask, c, itrs, seed},
    (* notation *)
    intr = List @@@ ints;
    intt = Transpose[intr];
    (* range minima, maxima *)
    mns = Min /@ intt;
    mxs = Max /@ intt;
    (* maximum sums of positive, negative indices *)
    sps = Max[Plus @@ Select[#, Positive] & /@ intr];
    sns = Min[Plus @@ Select[#, Negative] & /@ intr];
    (* helper: only acceptable seeds pass *)
    mask = Plus @@ Select[#, Positive] <= sps &&
      Plus @@ Select[#, Negative] >= sns & ;
    (* construct iterators *)
    itrs = MapIndexed[
      Prepend[#1, c[#2[[1]]]] & , Transpose[{mns, mxs}]];
    itrs = {c @@ c /@ Range[Length[itrs]], Sequence @@ itrs};
    (* generate table, apply mask, notation *)
    seed = Select[Flatten[Table @@ itrs], mask];
    seed /. c -> Head[First[ints]]];

(* main: all sectors *)
LaportaSeed[
  secs:{__Rule}, opts:OptionsPattern[]] :=
  Module[
    {os, sort, vb, res, lns, c},

    os = FilterRules[{opts}, Options[LaportaEncode]];
    sort = LaportaEncode[#, Sequence @@ os] & ;

    vb = OptionValue[Verbosity] /. VerbosityRules;

    Report[vb, 1, "Generating seeds..."];
    res = MapAt[$LaportaSeed, #, 2] & /@ secs;

    Report[vb, 1, "Sorting seeds..."];
    res = MapAt[SortBy[#, sort] & , #, 2] & /@ res;
    res = SortBy[res, sort[Part[#, 2, 1]] & ];

    lns = Length /@ Last /@ res;
    Report[vb, 1, "In total ", Plus @@ lns, " seeds."];
    Report[vb, 2, "Seeds in sectors:\n  ", lns, "."];

    (* delete duplicates, re-grouping *)
    res = MapAt[LaportaSector @@ # & , #, 1] & /@ res;
    res = c[Sequence[First[#], Sequence @@ Last[#]]] & /@ res;
    res = DeleteDuplicates[res /. c -> Sequence];
    res = Split[res, Head[#2] =!= LaportaSector & ];
    res = Select[res, Length[#] > 1 & ];
    res = LaportaSector @@ Prepend[Rest[#], List @@ First[#]] & /@ res;

    lns = Length /@ res - 1;
    Report[vb, 1, "Without duplicates ", Plus @@ lns, " seeds."];
    Report[vb, 2, "Seeds in sectors:\n  ", lns, "."];

    LaportaSectors @@ res];

(* trap *)
LaportaSeed[___] :=
  (Message[LaportaSeed::usage];
   Abort[]);

(* Needs:
- LaportaEncode[], LaportaSectors[].
*)

(* N.B.:
- Populate sectors by filling everything inbetween simplest integral
  (sector itself) and most complicated on.
- The maximum sums of positive, negative indices are used to further
  constrain seeds.
- Topology rules are not generated and applied, this is done in the
  implementation of the Laporta algorithm anyway.
*)

(* TODO:

- Input checks.

- Return LaportaSectors[] (?) by LaportaHull[].

*)

(* --- TopologyIBP -------------------------------------------------- *)

TopologyIBP::usage = "\
TopologyIBP[<top>, <set>, <qi>, <qj>, [opts]] returns the \
integration-by-parts relation (IBP) for topology <top> and setup <set> \
for derivative with respect to <qi> and afterwards contraction with \
<qj>.
The options \"Epsilon\" and \"Indices\" can be used to supplement \
corresponding symbols.";

Options[TopologyIBP] =
{"Epsilon" -> ep,
 "Indices" -> a};

TopologyIBP::symbol =
  TopoID::name;  (* TODO *)

TopologyIBP::derive = "\
\"`1`\" contains no internal momentum to build a derivative.";

TopologyIBP::linear = "\
\"`1`\" is not linear in external and internal momenta.";

(* main *)
TopologyIBP[
  top:TopologyPattern["Solve"], set:SetupPattern[], qi_, qj_,
    opts:OptionsPattern[]] :=
  Module[
    {chk, drv, ep,is, ts, fs, fss,fes, tp, cps, d, ibp, ds, bs, si},

    (* helper: check linearity of <x> *)
    chk = Function[
      x, Min[Exponent[x, #, Min] & /@ (vs /. set)] >= 0 &&
        Max[Exponent[x, #, Max] & /@ (vs /. set)] <= 1];

    (* helper: derive <x> with respect to <i> and contract with <j> *)
    drv[x_, i_, j_] := Module[
      {sr, s, c, l, c1,c2, d},
      sr = {};
      (* find substitution in case of sum *)
      If[Head[i] === Plus,
         c = {#, Coefficient[i, #, 1]} & /@ (ks /. set);
         c = First[Select[c, Last[#] =!= 0 & , 1]];
         sr = First[c] -> s/Last[c] - First[c],
         s = i];
      (* write sum as list, substitute *)
      l = If[Head[x] === Plus, List @@ x, List @ x] /. sr;
      (* linear, quadratic terms *)
      c1 = j*Coefficient[#, s, 1] & /@ l;
      c2 = 2*j*s*Coefficient[#, s, 2] & /@ l;
      (* combine terms, substitute back *)
      d = Plus @@ Join[c1, c2] /. s -> i;
      (* result: reexpress, simplify, notation *)
      Expand[d /. (scps /. top) /. (cs /. set) /. (rs /. set)]];

    (* -- arguments, options -- *)

    (* generate name symbol *)
    ts = Quiet[Check[Symbol[name /. top], Null]];

    (* check: symbol name *)
    If[ts === Null,
       Message[TopologyIBP::symbol];
       Abort[]];

    (* check: internal momentum *)
    If[Intersection[ks /. set, Variables[qi]] === {},
       Message[TopologyIBP::derive, qi];
       Abort[]];

    (* check: linear momenta *)
    Scan[
      If[!chk[#],
         Message[TopologyIBP::linear, #];
         Abort[]] & , {qi, qj}];

    {ep, is} = OptionValue[{"Epsilon", "Indices"}];

    (* -- main: calculate derivative -- *)

    (* factor symbols, expressions *)
    fs = facs /. top;
    {fss, fes} = {First /@ fs, Last /@ fs};

    (* topology as product of factors *)
    tp = Inner[Power, fss, is /@ fss, Times];

    (* complementary factors for one factor *)
    cps = Complement[fss, {#}] & /@ fss;
    cps = Inner[Power, #, is /@ #, Times] & /@ cps;

    (* application of product rule *)
    ibp = MapThread[is[#1]*d[#1]*#1^(is[#1] - 1)*#2 & , {fss, cps}];

    (* possible contribution *)
    If[qi === qj, PrependTo[ibp, (4 - 2*ep)*tp]];

    (* precompute derivatives *)
    ds = d[First[#]] -> drv[Last[#], qi, qj] & /@ fs;

    (* write as sum, insert derivatives *)
    ibp = (Plus @@ ibp) /. ds;

    (* -- result: change notation -- *)

    (* initialize notation *)
    ibp = Expand[(ts @@ ({is[#], 0} & /@ fss))*ibp];

    (* power counting for factor symbols *)
    ibp = ibp //.
      s_^p_*ts[a___, {is[s_], q_}, b___] -> ts[a, {is[s], q + p}, b];

    (* change notation *)
    ibp = ibp //.
      ts[a___, {_, p_}, b___] -> ts[a, -p, b] /. is[s_] -> -is[s];

    (* base scale, sum of indices *)
    bs = First[xs /. set];
    si = Total[is /@ fss];

    (* correct mass dimension *)
    ibp = ibp /. ts[a__] :> bs^(si - Total[{a}])*ts[a];

    (* rename indices *)
    ibp = ibp /. MapIndexed[is[#1] -> is[First[#2]] & , fss];

    (* collect *)
    Collect[ibp, ts[__], Factor]];

(* trigger: "Solve" *)
TopologyIBP[top:TopologyPattern[], set:SetupPattern[], qi_, qj_,
  opts:OptionsPattern[]] :=
  TopologyIBP[ReduceTopology[top, set], set, qi, qj, opts];

(* trap *)
TopologyIBP[___] :=
  (Message[TopologyIBP::usage];
   Abort[]);

(* --- TopologyIBPs ------------------------------------------------- *)

TopologyIBPs::usage = "\
TopologyIBPs[<top>, <set>, [opts]] returns the integration-by-parts \
relations (IBPs) for topology <top> and setup <set>.
In addition to options of TopologyIBP[], [Method] can be used to \
specify the set of generated IBPs:
- \"All\" -- all Nk*(Nk + Np) IBPs (naive approach),
- \"Lee\" -- only Nk + Np + 1 IBPs (according to Lee).
Np and Nk are the numbers of external and internal momenta.";

$TopologyIBPsMethods =
{"All", "Lee"};

Options[TopologyIBPs] =
{Method -> {"All"}};

Options[TopologyIBPs] =
  Join @@ (Options /@ {TopologyIBPs, TopologyIBP});

(* main *)
TopologyIBPs[
  top:TopologyPattern[], set:SetupPattern[],
    opts:OptionsPattern[]] :=
  Module[
    {me, cp},
    (* check: Method *)
    me = Flatten[{OptionValue[Method]}];
    cp = Complement[me, $TopologyIBPsMethods];
    If[cp =!= {},
       Message[Method::invalid, cp, $TopologyIBPsMethods];
       me = Complement[me, cp]];
    (* result: select IBPs *)
    Which[
      (* "Lee" *)
      MemberQ[me, "Lee"],
      Join[
        MapThread[
          TopologyIBP[top, set, #1, #2] & ,
          {ks /. set, Prepend[Most[ks /. set], Last[ks /. set]]}],
        TopologyIBP[top, set, First[ks /. set], #] & /@ (ps /. set),
        {Total[TopologyIBP[top, set, #, #] & /@ (ks /. set)]}],
      (* "All", default *)
      True,
      Join @@ Outer[
        TopologyIBP[top, set, #1, #2] & ,
        ks /. set, vs /. set]]];

(* trap *)
TopologyIBPs[___] :=
  (Message[TopologyIBPs::usage];
   Abort[]);

(* --- TopologyIBPRule ---------------------------------------------- *)

TopologyIBPRule::usage = "\
TopologyIBPRule[<top>, <set>, <qi>, <qj>, [opts]] returns the \
integration-by-parts relation (IBP) for topology <top> and setup <set> \
for derivative with respect to <qi> and afterwards contraction with \
<qj> in rule form.  This rule can be applied directly to seed \
integrals.
The same options as for TopologyIBP[] are available.";

Options[TopologyIBPRule] =
  Options[TopologyIBP];

(* atomic: build rule *)
$TopologyIBPRule[ts_Symbol, iss_List, ibp_] := Module[
  {as, lhs,rhs},
  as = MapIndexed[Symbol["a" <> ToString[First[#2]]] & , iss];
  lhs = ts @@ (Pattern[#, Blank[]] & /@ as);
  rhs = ibp /. MapThread[#1 -> #2 & , {iss, as}];
  lhs -> rhs];

(* main *)
TopologyIBPRule[
  top:TopologyPattern[], set:SetupPattern[], qi_, qj_,
    opts:OptionsPattern[]] :=
  Module[
    {ibp, iss},
    ibp = TopologyIBP[top, set, qi, qj, opts];
    iss = MapIndexed[OptionValue["Indices"][First[#2]] & , facs /. top];
    $TopologyIBPRule[Symbol[name /. top], iss, ibp]];

(* trap *)
TopologyIBPRule[___] :=
  (Message[TopologyIBPRule::usage];
   Abort[]);

(* --- TopologyIBPRules --------------------------------------------- *)

TopologyIBPRules::usage = "\
TopologyIBPRules[<top>, <set>, [opts]] returns the \
integration-by-parts relations (IBPs) for topology <top> and setup \
<set> in rule form.  These rules can be applied directly to seed \
integrals.
The same options as for TopologyIBPs[] are available.";

Options[TopologyIBPRules] =
  Options[TopologyIBPs];

(* main *)
TopologyIBPRules[
  top:TopologyPattern[], set:SetupPattern[],
    opts:OptionsPattern[]] :=
  Module[
    {ibps, iss},
    ibps = TopologyIBPs[top, set, opts];
    iss = MapIndexed[OptionValue["Indices"][First[#2]] & , facs /. top];
    $TopologyIBPRule[Symbol[name /. top], iss, #] & /@ ibps];

(* trap *)
TopologyIBPRules[___] :=
  (Message[TopologyIBPRules::usage];
   Abort[]);

(* --- LaportaLimit ------------------------------------------------- *)

LaportaLimit::usage = "\
LaportaLimit[<top>, <lims>] is a wrapper function introduced to store \
initialization limits for the Laporta algorithm.
Appropriately formatted output can be obtained, e.g., by invoking \
ToString[].";

Format[LaportaLimit[
  tn_String, tp_String, cm___String, ls:Repeated[{__Integer}, 4]]] :=
  StringJoin[
    "#+\n",
    "# define topology\n",
    "TOP=", tn, "\n",
    "# " <> # <> "\n" & /@ {cm},
    "#-\n",
    "\n",
    "#+\n",
    "# Laporta limits (", tp, ") generated by TopoID\n",
    "LIM=\"", Riffle[
      {"\\\"", Riffle[ToString /@ #, ","], "\\\""} & /@ {ls}, " "],
    "\"\n",
    "#-\n"];

Format[LaportaLimit[
  tn_String, tp_String, cm___String]] :=
  StringJoin[
    "# define topology\n",
    "TOP=", tn, "\n",
    "# " <> # <> "\n" & /@ {cm}];

Format[LaportaLimit[
  ls:Repeated[{__Integer}, 4]]] :=
  StringJoin[
    "append \"", Riffle[
      {"\\\"", Riffle[ToString /@ #, ","], "\\\""} & /@ {ls}, " "],
    "\"\n"];

(* --- TopologyLimit ------------------------------------------------ *)

TopologyLimit::usage = "\
TopologyLimit[<lims>] is a wrapper function introduced to store \
technical limits for the Laporta algorithm.
Appropriately formatted output can be obtained, e.g., by invoking \
ToString[].";

Format[TopologyLimit[
  mn:{__Integer}, mx:{__Integer}]] :=
  StringJoin[
    "#+\n",
    "EMN=\"", Riffle[ToString /@ mn, ","], "\"  # technical min\n",
    "EMX=\"", Riffle[ToString /@ mx, ","], "\"  # technical max\n",
    "#-\n"];

(* --- LaportaSector(s) --------------------------------------------- *)

LaportaSector::usage = LaportaSectors::usage = "\
LaportaSector(s)[<sec(s)>] are wrapper functions introduced to store \
seed integrals for the Laporta algorithm.
Appropriately formatted output can be obtained, e.g., by invoking \
ToString[].";

Format[
  LaportaSector[sect:{__Integer}, ints___]] :=
  Module[
    {fn, fc, fe},
    fn = StringJoin[Riffle[ToString /@ Sort[sect], "-"]];
    fc = StringJoin["sector (", Riffle[ToString /@ sect, ","], ")"];
    fe = "  + " <> # <> "\n" & /@ FORMForm /@ {ints};
    ToFORMCodeString[FORMFold[fn, FORMComment[fc], fe]]];

Format[LaportaSectors[secs___LaportaSector]] :=
  StringJoin[Riffle[
    ToString /@ DeleteCases[{secs}, LaportaSector[{}, ___]], "\n"]];

(* --- package end -------------------------------------------------- *)

Protect["TopoID`Calculate`*"];

Scan[
  SetAttributes[#, {ReadProtected}] & ,
  Select[Symbol /@ Names["TopoID`Calculate`*"], Head[#] === Symbol & ]];


Unprotect[
$TopologySimplifyRules,
$TopologyFactors,$TopologyFunction, $TopologyExpand,$TopologyReduce,
$TopologyShifts, $TopologyRulesOrdering,


  LookUp, LaportaLimits, TopologyIndex, TopologyLimits];

ClearAttributes[
  {
$TopologySimplifyRules,
$TopologyFactors,$TopologyFunction, $TopologyExpand,$TopologyReduce,
$TopologyShifts, $TopologyRulesOrdering,

LookUp, LaportaLimits, TopologyIndex, TopologyLimits},
  ReadProtected];

End[];

EndPackage[];

(* ------------------------------------------------------------------ *)



(* --- TODO:

- LookUp[]: other extensions for other DBs.  Use directly cmdline tools of tc.

- TopologyIBP[]: check results for arbitrary linear combinations!

- TopologyIBPs[]: use (which) line momenta as basis?

- TopologyToRules[]: a1_ -> a1_Integer?

*)
