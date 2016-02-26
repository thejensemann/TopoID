(* -- "Topology.m": Data Type Patterns, Atomic Operations ----------- *)

(* --- provided functions:

FactorPattern[], GroupPattern[], VertexPattern[], EdgePattern[] -> <pat>
-- object factor, (sub-)group, vertex symbol, edge data patterns

DenominatorSymbolQ[<x>], NumeratorSymbolQ[<x>] -> <bool>
-- factor type query functions


TopologyPattern[[<r(s)>]] -> <pat>
-- topology data pattern

TopologyQ[<top>, [<r(s)>]], TopologyListQ[<tops>, [<r(s)>]] -> <bool>
-- corresponding query functions

Topology[[<top>], [<r(s)>]] -> <top>
-- corresponding constructor


DiagramPattern[[<r(s)>]] -> <pat>
-- diagram data pattern

DiagramQ[<dia>, [<r(s)>]], DiagramListQ[<dias>, [<r(s)>]] -> <bool>
-- corresponding query functions

Diagram[[<dia>], [<r(s)>]] -> <dia>
-- corresponding constructor


IntegralPattern[[<r(s)>]] -> <pat>
-- integral data pattern

IntegralQ[<int>, [<r(s)>]], IntegralListQ[<ints>, [<r(s)>]]
-- corresponding query functions

Integral[[<r(s)>], [<r(s)>]] -> <int>
-- corresponding constructor


TopologyMatrix[<top>, [<set>]] -> <mat>
-- compute factor coefficient matrix of constants and momenta

TopologyConsistentQ[<top(s)>, [<set>]] -> <bool(s)>
-- check if all constants were declared in <set>

TopologyMatrixReduce[<mat>, [<set>]] -> <mat>
-- discard columns for coefficients of constants from matrix

TopologyMatrixReduced[<top>, [<set>]] -> <mat>
-- compute without coefficients of constants from the start

TopologyRank[<top>, [<set>]] -> <int>
-- rank of reduced coefficient matrix

TopologyCompleteQ[<top(s)>, [<set>]] -> <bool(s)>
-- check if all scalar products can be expressed in terms of factors

TopologyDependentQ[<top(s)>, [<set>]] -> <bool(s)>
-- check for linearly dependent factors


TopologySolve[<top>, [<set>], [opts]] -> <rules>
-- express all scalar products uniquely via topology factors

TopologyReduce[<top>, [<set>], [opts]] -> {<rules>, <list>}
-- express scalar products via topology factors, find linear relations

TopologyScalarProducts[<top>, [<set>], [opts]] -> <rules>
-- find irreducible scalar products in <top>


TopologyMasses[<top>, [<set>]] -> <list>
-- extract squared masses from topology factors

TopologyMomentaFlows[<top>, [<set>], [opts]] -> <list>
-- reconstruct momenta flows in topology factors up to signs

TopologyMomentaShifts[<src>, <trg>, [<set>], [opts]] -> <rules>
-- find momenta shifts from source to target

*)

(* --- package begin ------------------------------------------------ *)

Unprotect["TopoID`Topology`*"];

ClearAll["TopoID`Topology`*", "TopoID`Topology`Private`*"];

BeginPackage[
  "TopoID`Topology`",
  {"TopoID`Common`", "TopoID`System`",  (* TODO *)
   "TopoID`Setup`"}];

{p,d,n, in,out,
 FactorPattern, GroupPattern, VertexPattern, EdgePattern,
 DenominatorSymbolQ, NumeratorSymbolQ};

{$TopologyKeywords,
 name, hash, hist, inds, facs, scps,rels, apar,arep, subt,zero,
 symm, cuts, legs,pros, setp,
 TopologyPattern,
 TopologyQ, TopologyListQ,
 $TopologyKeys, Topology};

{DiagramPattern, DiagramQ, DiaDiagramListQ, Diagram};

{IntegralPattern, IntegralQ, IntegralListQ, Integral};

{TopologyMatrix, TopologyConsistentQ,
 TopologyMatrixReduce, TopologyMatrixReduced,
 TopologyRank, TopologyCompleteQ, TopologyDependentQ};

{TopologySolve, TopologyReduce, TopologyScalarProducts};

{TopologyMasses, TopologyMomentaFlows,
 $TopologyMomentaShiftsMethods, TopologyMomentaShifts};

Begin["`Private`"];

(* --- wrappers ----------------------------------------------------- *)

p::usage = d::usage = n::usage = "\
p[<int>], d[<int>] and n[<int>] wrapping integers are used in the data \
type pattern FactorPattern[] to distinguish the origin of topology \
factors:
- p[<int>] means the factor really stems from a propagator in a diagram,
- d[<int>] is a denominator which may have been added artificially but \
that must be considered for negative indices,
- n[<int>] denotes factors exclusively appearing as numerators, \
generally not just mere scalar products wrapped in s[<int>].";

in::usage = out::usage = "\
in[<int>] and out[<int>] wrapping integers are used in the data type \
pattern VertexPattern[] to label vertices of incoming or outgoing \
external edges.";

(* --- atomics ------------------------------------------------------ *)

FactorPattern::usage = "\
FactorPattern[] gives a data type pattern describing a topology or \
integral factor denoted by rules assigning a symbol to an expression.
Used in the >facs< entry in TopologyPattern[] and IntegralPattern[].
Examples:
- p[1] -> mh^2 - s - 2*p1*v1 - 2*p2*v1 + v1^2,
- p[2] -> v1^2.";

FactorPattern[] :=
  (p | d | n | s)[_Integer] | _Symbol -> _;

(* -- *)

GroupPattern::usage = "\
GroupPattern[] gives a data type pattern representing an equivalence \
class of (sub-)topologies given by a list of extended permutations \
that may act on a collection of lines.
A zero denotes the contraction of the corresponding line.  The \
definition allows also for empty sub-topologies or empty groups.
Used in the >subt<, >zero<, >symm< and >cuts< entries of \
TopologyPattern[] and IntegralPattern[].  Objects of this type are \
generated by the functions IdenticalGroups[], SymmetricGroups[] or \
Cuts[].
Example:
- {{1, 4, 3}, {1, 2, 3}}.";

GroupPattern[] :=
{{__Integer}...};

(* -- *)

VertexPattern::usage = "\
VertexPattern[] gives a data type pattern for vertex numbers which may \
be integers, in[] or out[] wrapping integers (present for external \
legs).";

VertexPattern[] :=
  in[_Integer] | out[_Integer] | _Integer;

(* -- *)

EdgePattern::usage = "\
EdgePattern[] gives a data type pattern for an edge keeping \
information about the type of particle, its momentum and the connected \
vertices.
An external edge should contain at least one in[] or out[] wrapper in \
one of its vertices.
It constitutes a part of the graph data in DiagramPattern[], \
TopologyPattern[] or IntegralPattern[] objects.
Example (Higgs boson):
- {\"hb\", k1, 1, 2}.";

EdgePattern[] :=
{_,                 (* particle label *)
 _,                 (* given momentum *)
 VertexPattern[],   (* begin vertex *)
 VertexPattern[]};  (* end vertex *)

(* Needs:
- p[], d[], n[], s[].
- in[], out[].
*)

(* --- DenominatorSymbolQ, NumeratorSymbolQ ------------------------- *)

DenominatorSymbolQ::usage = "\
DenominatorSymbolQ[<x>] checks whether <x> matches a symbol reserved \
for denominators.";

(* main *)
DenominatorSymbolQ[x_] :=
  MatchQ[x, (p | d)[_Integer]] ||
  StringMatchQ[ToString[x], RegularExpression["(?i)[pd]\\d+"]];

(* trap *)
DenominatorSymbolQ[___] :=
  (Message[DenominatorSymbolQ::usage];
   Abort[]);

(* -- *)

NumeratorSymbolQ::usage = "\
NumeratorSymbolQ[<x>] checks whether <x> matches a symbol reserved for \
numerators.";

(* main *)
NumeratorSymbolQ[x_] :=
  MatchQ[x, (n | s)[_Integer]] ||
  StringMatchQ[ToString[x], RegularExpression["(?i)[ns]\\d+"]];

(* trap *)
NumeratorSymbolQ[___] :=
  (Message[NumeratorSymbolQ::usage];
   Abort[]);

(* Needs:
- p[], d[], n[], s[].
*)

(* --- TopologyPattern ---------------------------------------------- *)

$TopologyKeywords =
{"Hash", "Inds", "Solve", "Alpha", "Subt", "Zero", "Symm", "Cuts",
 "Graph", "Setup"};

TopologyPattern::usage = "\
TopologyPattern[[<r(s)>]] gives a data type pattern for topologies \
holding name (>name<), mapping history (>hist<) and a collection of \
factors (>facs<).
The fields for a unique identifier (>hash<), indices (>inds<), \
inverted factors and relations (>scps<, >rels<), alpha-representation \
(>apar<, >arep<), (zero) (sub-)topologies (>subt<, >zero<), symmetries \
(>symm<), cuts (>cuts<), graph structure (>legs<, >pros<) and partial \
setup (>setp<) are optional and may arise only from the call of some \
specific routines.
This pattern is dynamic, viz. several entry groups can be required by \
a string keyword as argument in [<r(s)>].  These keywords (cf. \
$TopologyKeywords) and their effect are:
- \"Hash\": {>hash<},
- \"Inds\": {>inds<},
- \"Solve\": {>scps<, >rels<},
- \"Alpha\": {>apar<, >arep<},
- \"Subt\": {>subt<},
- \"Zero\": {>zero<},
- \"Symm\": {>symm<},
- \"Cuts\": {>cuts<},
- \"Graph\": {>legs<, >pros<},
- \"Setup\": {>setp<}.
The special symbolic keywords >All<, >Full< and >None< are allowed in \
the list or sequence [<r(s)>].  >All< results in a pattern demanding \
all optional entries set properly.  In contrast, with >Full< \
placeholders (the keys itself) are also allowed.  With >None< entries \
kept optional by [<r(s)>] do not appear in the pattern.
DiagramPattern[[<r(s)>]] is a shortcut for requesting a Feynman \
diagram pattern, with mandatory graph data.  IntegralPattern[[<r(s)>]] \
is a shortcut for requesting a Feynman integral pattern, with a \
mandatory entry for indices.
Examples:
- TopologyPattern[],
- TopologyPattern[\"Solve\"],
- TopologyPattern[\"Solve\", \"Alpha\"],
- TopologyPattern[All],
- TopologyPattern[Full],
- TopologyPattern[\"Alpha\", None].";

TopologyPattern::keywords = "\
Warning: Unknown keyword(s) `1`, only those out of `2` (as strings) \
are valid.";

(* main: hidden, "complex" *)
$TopologyPattern["complex", rqs___String] :=
  Module[
    {cp},
    (* check: keywords *)
    cp = Complement[{rqs}, $TopologyKeywords];
    If[cp =!= {},
       Message[TopologyPattern::keywords,
         cp, $TopologyKeywords]];
    (* result: required pattern *)
    {name -> _String,
     If[MemberQ[{rqs}, "Hash"],
        hash -> _Integer,
        Elective[hash -> hash | _Integer]],
     hist -> _List,
     If[MemberQ[{rqs}, "Inds"],
        inds -> _List,
        Elective[inds -> inds | _List]],
     facs -> {FactorPattern[]...},
     If[MemberQ[{rqs}, "Solve"],
        Hold[
          scps -> {___Rule},
          rels -> _List],
        Elective[
          scps -> scps | {___Rule},
          rels -> rels | _List]],
     If[MemberQ[{rqs}, "Alpha"],
        Hold[
          apar -> _List,
          arep -> {_, _}],
        Elective[
          apar -> apar | _List,
          arep -> arep | {_, _}]],
     If[MemberQ[{rqs}, "Subt"],
        subt -> {GroupPattern[]...},
        Elective[subt -> subt | {GroupPattern[]...}]],
     If[MemberQ[{rqs}, "Zero"],
        zero -> GroupPattern[],
        Elective[zero -> zero | GroupPattern[]]],
     If[MemberQ[{rqs}, "Symm"],
        symm -> {GroupPattern[]...},
        Elective[symm -> symm | {GroupPattern[]...}]],
     If[MemberQ[{rqs}, "Cuts"],
        cuts -> GroupPattern[],
        Elective[cuts -> cuts | GroupPattern[]]],
     If[MemberQ[{rqs}, "Graph"],
        Hold[
          legs -> {EdgePattern[]...},
          pros -> {EdgePattern[]...}],
        Elective[
          legs -> legs | {EdgePattern[]...},
          pros -> pros | {EdgePattern[]...}]],
     If[MemberQ[{rqs}, "Setup"],
        setp -> _List,
        Elective[setp -> _List | setp]]
   } // ReleaseHold];

(* main: hidden, "simple" *)
$TopologyPattern["simple", rqs___String] :=
  ReleaseHold[
    {name -> _String,
     If[MemberQ[{rqs}, "Hash"],
        hash -> _Integer,
        ___Rule],
     hist -> _List,
     If[MemberQ[{rqs}, "Inds"],
        inds -> _List,
        ___Rule],
     facs -> _List,
     If[MemberQ[{rqs}, "Solve"],
        Hold[scps -> _List, rels -> _List],
        ___Rule],
     If[MemberQ[{rqs}, "Alpha"],
        Hold[apar -> _List, arep -> _List],
        ___Rule],
     If[MemberQ[{rqs}, "Subt"],
        subt -> _List,
        ___Rule],
     If[MemberQ[{rqs}, "Zero"],
        zero -> _List,
        ___Rule],
     If[MemberQ[{rqs}, "Symm"],
        symm -> _List,
        ___Rule],
     If[MemberQ[{rqs}, "Cuts"],
        cuts -> _List,
        ___Rule],
     If[MemberQ[{rqs}, "Graph"],
        Hold[legs -> _List, pros -> _List],
        ___Rule],
     If[MemberQ[{rqs}, "Setup"],
        setp -> _List,
        ___Rule]}];

(* main: switch *)
If[$TopoIDData === True,
   TopologyPattern[rqs___String] := $TopologyPattern["complex", rqs],
   TopologyPattern[rqs___String] := $TopologyPattern["simple", rqs]];

(* overload: list *)
TopologyPattern[rqs1___, rqs_List, rqs2___] :=
  TopologyPattern[rqs1, Sequence @@ rqs, rqs2];

(* shortcut: All *)
TopologyPattern[rqs1___, All, rqs2___] :=
  TopologyPattern[$TopologyKeywords];

(* shortcut: Full *)
TopologyPattern[rqs1___, Full, rqs2___] :=
  Module[
    {f, pat},
    pat = TopologyPattern[] /. Repeated -> f;
    pat /. f[x_, _] -> x /. PatternSequence -> Sequence];

(* shortcut: None *)
TopologyPattern[rqs1___, None, rqs2___] :=
  Module[
    {f, pat},
    pat = TopologyPattern[rqs1] /. Repeated -> f;
    pat = DeleteCases[pat, f[_, _]] /. f -> Repeated;
    Cases[pat, _Rule]];

(* trap *)
TopologyPattern[___] :=
  (Message[TopologyPattern::usage];
   Abort[]);

(* Needs:
- FactorPattern[], GroupPattern[], EdgePattern[].
*)

(* N.B.:
- <...> means mandatory for the object in parentheses,
  [...] means optional for topologies.
- Entries in object data type patterns:
     name,   -- object name,
    [hash,]  -- unique identifier,
     hist,   -- object history,
  "Inds" (integrals):
    <inds,>  -- factor indices,
     facs,   -- object factors,
  "Solve":
    [scps,   -- inverted factors,
     rels,]  -- additional relations,
  "Alpha":
    [apar,   -- alpha-parameters,
     arep,]  -- alpha-representation,
  "Subt":
    [subt,]  -- distinct subsets,
  "Zero":
    [zero,]  -- zero subsets,
  "Symm":
    [symm,]  -- symmetric subsets,
  "Cuts":
    [cuts,]  -- cut subsets,
  "Graph" (diagram):
    <legs,   -- external lines,
     pros,>  -- internal lines,
  "Setup":
    [setp]   -- (partial) setup.
*)

(* --- TopologyQ ... ------------------------------------------------ *)

TopologyQ::usage = "\
TopologyQ[<top>, [<r(s)>]] checks whether <top> matches \
TopologyPattern[[<r(s)>]].";

(* pass *)
TopologyQ[top_, rqs___] :=
  MatchQ[top, TopologyPattern[rqs]];

(* trap *)
TopologyQ[___] :=
  (Message[TopologyQ::usage];
   Abort[]);

(* -- *)

TopologyListQ::usage = "\
TopologyListQ[<tops>, [<r(s)>]] checks whether <tops> is a list and \
all elements match TopologyPattern[[<r(s)>]].";

(* pass *)
TopologyListQ[tops_, rqs___] :=
  Head[tops] === List && And @@
  (MatchQ[#, TopologyPattern[rqs]] & /@ tops);

(* trap *)
TopologyListQ[___] :=
  (Message[TopologyListQ::usage];
   Abort[]);

(* Needs:
- TopologyPattern[].
*)

(* --- Topology ----------------------------------------------------- *)

(* all keys allowed *)
$TopologyKeys =
  First /@ TopologyPattern[All];

Topology::usage = "\
Topology[[<r(s)>]] is a constructor for objects matching a \
TopologyPattern[[<r(s)>]] definition.
Topologies can conveniently be composed by using the keywords also \
allowed in TopologyPattern[[<r(s)>]] and rules assigning directly \
values to the allowed keys (cf. $TopologyKeys).  Here overwriting via \
subsequent assignments is possible and especially useful for modifying \
only some entries in a given object.  An entry can be removed by \
assigning the symbol >None< to the key.  The history entry >hist< is \
only cleared in this way (set to {}, but not removed entirely).  \
Again, >All< is a special symbolic keyword resulting in a full \
TopologyPattern[[<r(s)>]] object.
Diagram[[<r(s)>]] is a shortcut for constructing a Feynman diagram \
object with graph data.  Integral[[<r(s)>]] is a shortcut for \
constructing a Feynman integral object with an entry for indices.
Examples:
- Topology[]
    {name -> \"\", hist -> {}, facs -> {}},
- Topology[\"Solve\"]
    {name -> \"\", hist -> {}, facs -> {}, scps -> scps, rels -> rels},
- Topology[name -> \"top\"]
    {name -> \"top\", hist -> {}, facs -> {}},
- Topology[\"Solve\", name -> \"top\"]
    {name -> \"top\", hist -> {}, facs -> {}, scps -> scps, rels -> \
rels},
- Topology[name -> \"top\", {\"Solve\", \"Alpha\"}]
    {name -> \"top\", hist -> {}, facs -> {}, scps -> scps, rels -> \
rels, apar -> apar, arep -> arep},
- Topology[All]
    {name -> \"\", hist -> {}, inds -> {}, facs -> {}, scps -> scps, \
rels -> rels, apar -> apar, arep -> arep, subt -> subt, zero -> zero, \
symm -> symm, cuts -> cuts, legs -> legs, pros -> pros, setp -> setp}.";

Topology::keywords =
  TopologyPattern::keywords;

Topology::keys = "\
Warning: Unknown key(s) `1`, only those out of `2` are valid.";

Topology::nomatch = "\
Warning: Key(s) `1` in the object do(es) not match a valid \
TopologyPattern[[<r(s)>]].";

Topology::unknown = "\
Warning: Unknown key(word)(s) `1`, only those out of `2` (as strings) \
or `3` are valid.";

(* defaults *)
Defaults[Topology] =
{name -> "",
 hash -> 0,
 hist -> {},
 inds -> {},
 facs -> {},
 (* -- *)
 scps -> {},
 rels -> {},
 (* -- *)
 apar -> {},
 arep -> {0, 0},
 (* -- *)
 subt -> {},
 zero -> {},
 symm -> {},
 cuts -> {},
 (* -- *)
 legs -> {},
 pros -> {},
 (* -- *)
 setp -> {}};

(* default *)
Topology[] :=
{name -> "",
 hist -> {},
 facs -> {}};

(* initialize *)
Topology[rs__] /; !TopologyQ[First[{rs}]] :=
  Topology[Topology[], rs];

(* hidden case: strings *)
$Topology[rqs___String] :=
  Module[
    {cp,is},
    (* check: keywords *)
    cp = Complement[{rqs}, $TopologyKeywords];
    If[cp =!= {},
       Message[Topology::keywords,
         cp, $TopologyKeywords]];
    (* filter: keywords *)
    is = Intersection[{rqs}, $TopologyKeywords];
    (* result: required template *)
    # -> (# /. Defaults[Topology]) & /@
      (First /@ TopologyPattern[is, None])];

(* hidden case: topology, strings *)
$Topology[top:TopologyPattern[], rqs___String] :=
  Module[
    {f, tmp, res},
    (* helper: generate merging rules *)
    f = Rule[First[#], _] -> # & ;
    (* required template *)
    tmp = $Topology[rqs];
    (* build maximum template *)
    res = # -> Null & /@ $TopologyKeys;
    (* merge: first keywords, then topology *)
    res = res /. (f /@ tmp) /. (f /@ top);
    (* result: clean *)
    DeleteCases[res, _ -> Null]];

(* hidden case: rules *)
$Topology[rls___Rule] :=
  Module[
    {cp,is},
    (* check: keys *)
    cp = Complement[First /@ {rls}, $TopologyKeys];
    If[cp =!= {},
       Message[Topology::keys,
         cp, $TopologyKeys]];
    (* filter: keys *)
    is = Intersection[First /@ {rls}, $TopologyKeys];
    (* result: required template *)
    Select[{rls}, MemberQ[is, First[#]] & ]];

(* hidden case: topology, rules *)
$Topology[top:TopologyPattern[], rls___Rule] :=
  Module[
    {f, tmp, res, hs},
    (* helper: generate merging rules *)
    f = Rule[First[#], _] -> # & ;
    (* required template *)
    tmp = $Topology[rls];
    (* build maximum template *)
    res = # -> Null & /@ $TopologyKeys;
    (* history: collect *)
    hs = Join[hist /. top, Cases[{rls}, Rule[hist, x_] -> x]];
    (* history: clear *)
    hs = Drop[hs, Prepend[Position[hs, None, {1}], {0}][[-1, 1]]];
    (* merge: first topology, then keys reversed *)
    res = res /. (f /@ top) /. (f /@ Reverse[tmp]) /. f[hist -> hs];
    (* result: clean *)
    DeleteCases[res, _ -> None | Null]];

(* main: topology, strings or rules *)
Topology[top:TopologyPattern[], rs:(_String | _Rule)...] :=
  Module[
    {rqs,rls, tmp, ex},
    (* select: keywords, keys *)
    rqs = Cases[{rs}, _String];
    rls = Cases[{rs}, _Rule];
    (* merge: first keywords, then keys *)
    tmp = $Topology[top, Sequence @@ rqs];
    tmp = $Topology[tmp, Sequence @@ rls];
    (* check: object matches *)
    If[!TopologyQ[tmp],
       ex = Reap[
         If[!MatchQ[#[[2]], First[#] /. TopologyPattern[Full]],
            Sow[First[#]]] & /@ tmp];
       Message[Topology::nomatch,
         Join @@ Last[ex]]];
    (* result *)
    tmp];

(* overload: lists, topologies *)
Topology[top:TopologyPattern[], rs1___, rs_List, rs2___] :=
  Topology[top, rs1, Sequence @@ rs, rs2];

(* shortcut: all entries allowed *)
Topology[top:TopologyPattern[], rs1___, All, rs2___] :=
  Topology[top, rs1, $TopologyKeywords, rs2];

(* shortcut: keys as keywords *)
Topology[top:TopologyPattern[], rs1___, r_Symbol, rs2___] /;
MemberQ[$TopologyKeys, r] :=
  Topology[top, rs1, r -> (r /. Defaults[Topology]), rs2];

(* trap *)
Topology[top:TopologyPattern[], rs___] :=
  Module[
    {rqs,rls, cp},
    (* select: keywords, keys *)
    rqs = Cases[{rs}, _String];
    rls = Cases[{rs}, _Rule];
    (* check: key(word)s *)
    cp = Complement[{rs}, rqs, rls];
    If[cp =!= {},
       Message[Topology::unknown,
         cp, $TopologyKeywords, $TopologyKeys]];
    (* result *)
    Topology[top, Sequence @@ rqs, Sequence @@ rls]];

(* Needs:
- TopologyPattern[], TopologyQ[].
*)

(* N.B.:
- Topology[...] intentionally not used as data type wrapper, so one can
  use the returned object matching TopologyPattern[] as replacement list
  for element access.
*)

(* --- Diagram... --------------------------------------------------- *)

DiagramPattern::usage =
  TopologyPattern::usage;

(* shortcut *)
DiagramPattern[rs___] :=
  TopologyPattern["Graph", rs];

(* -- *)

DiagramQ::usage =
  TopologyQ::usage;

(* shortcut *)
DiagramQ[dia_, rs___] :=
  TopologyQ[obj, "Graph", rs];

(* -- *)

DiagramListQ::usage =
  TopologyListQ::usage;

(* shortcut *)
DiagramListQ[dias, rs___] :=
  TopologyListQ[dias, "Graph", rs]

(* -- *)

Diagram::usage =
  Topology::usage;

(* shortcut *)
Diagram[rs___] :=
  Topology["Graph", rs];

(* Needs:
- TopologyPattern[], TopologyQ[], TopologyListQ[], Topology[].
*)

(* --- Integral... -------------------------------------------------- *)

IntegralPattern::usage =
  TopologyPattern::usage;

(* shortcut *)
IntegralPattern[rs___] :=
  TopologyPattern["Inds", rs];

(* -- *)

IntegralQ::usage =
  TopologyQ::usage;

(* shortcut *)
IntegralQ[obj_, rs___] :=
  TopologyQ[obj, "Inds", rs];

(* -- *)

IntegralListQ::usage =
  TopologyListQ::usage;

(* shortcut *)
IntegralListQ[ints_, rs___] :=
  TopologyListQ[ints, "Inds", rs];

(* -- *)

Integral::usage =
  Topology::usage;

(* shortcut *)
Integral[rs___] :=
  Topology["Inds", rs];

(* Needs:
- TopologyPattern[], TopologyQ[], TopologyListQ[], Topology[].
*)

(* --- TopologyMatrix ----------------------------------------------- *)

TopologyMatrix::usage = "\
TopologyMatrix[<top>, [<set>]] returns a matrix with rows \
corresponding to factors of the topology <top> and columns \
corresponding to scalar products and constants listed in the setup \
[<set>] and factor symbols as variables (coefficient Matrix).";

(* main *)
TopologyMatrix[
  top:TopologyPattern[], set:SetupPattern[]] :=
  Module[
    {fc, vr},
    (* factors: symbol -> expression *)
    fc = facs /. top;
    (* all variables: scalar products, constants, factor symbols *)
    vr = Join[Last /@ (ss /. set), xs /. set, First /@ fc];
    (* construct matrix: each row corresponds to a factor *)
    Outer[Coefficient[Last[#1] - First[#1], #2] & , fc, vr]];

(* shortcut: "Setup" *)
TopologyMatrix[top:TopologyPattern["Setup"]] :=
  TopologyMatrix[top, setp /. top];

(* trap *)
TopologyMatrix[___] :=
  (Message[TopologyMatrix::usage];
   Abort[]);

(* --- TopologyConsistentQ ------------------------------------------ *)

TopologyConsistentQ::usage = "\
TopologyConsistenQ[<top(s)>, [<set>]] checks whether all constants \
appearing in <top(s)> have been declared properly in the setup \
[<set>].";

(* overload: plural *)
TopologyConsistentQ[
  tops_?TopologyListQ, set:Elective[SetupPattern[]]] :=
  TopologyConsistentQ[#, set] & /@ tops;

(* main *)
TopologyConsistentQ[
  top:TopologyPattern[], set:SetupPattern[]] :=
  Module[
    {fc, vr, tm},
    (* factors: symbol -> expression *)
    fc = facs /. top;
    (* all variables: scalar products, constants, factor symbols *)
    vr = Join[Last /@ (ss /. set), xs /. set, First /@ fc];
    (* compute topology matrix *)
    tm = TopologyMatrix[top, set];
    (* check: declaration of constants *)
    Expand[tm . vr - (Last[#] - First[#] & /@ fc)] === (0 & /@ fc)];

(* shortcut: "Setup" *)
TopologyConsistentQ[top:TopologyPattern["Setup"]] :=
  TopologyConsistentQ[top, setp /. top];

(* trap *)
TopologyConsistentQ[___] :=
  (Message[TopologyConsistentQ::usage];
   Abort[]);

(* Needs:
- TopologyMatrix[].
*)

(* --- TopologyMatrixReduce ----------------------------------------- *)

TopologyMatrixReduce::usage = "\
TopologyMatrixReduce[<mat>, <set>] discards all entries in the \
topology matrix (coefficient matrix) <mat> that do not correspond to \
scalar products listed in the setup [<set>].
Note that the same effect can be achieved directly with \
TopologyMatrixReduced[<top>, [<set>]].";

(* main *)
TopologyMatrixReduce[mat_?MatrixQ, set:SetupPattern[]] /;
Length[First[mat]] >= (ns /. set) :=
  (* result: discard all but scalar product coefficients *)
  Take[#, ns /. set] & /@ mat;

(* shortcut: "Setup" *)
TopologyMatrixReduce[mat_, top:TopologyPattern["Setup"]] :=
  TopologyMatrixReduce[mat, setp /. top];

(* trap *)
TopologyMatrixReduce[___] :=
  (Message[TopologyMatrixReduce::usage];
   Abort[]);

(* --- TopologyMatrixReduced ---------------------------------------- *)

TopologyMatrixReduced::usage = "\
TopologyMatrixReduced[<top>, [<set>]] returns a matrix with rows \
corresponding to factors of the topology <top> and columns \
corresponding to scalar products listed in the setup [<set>] as only \
variables (in contrast to TopologyMatrix[]).
Note that the same effect can be achieved with TopologyMatrix[<top>, \
[<set>]] and TopologyMatrixReduce[<mat>, <set>].";

(* main *)
TopologyMatrixReduced[
  top:TopologyPattern[], set:SetupPattern[]] :=
  Module[
    {fc, vr},
    (* factors: symbol -> expression *)
    fc = facs /. top;
    (* variables: scalar products only *)
    vr = Last /@ (ss /. set);
    (* construct matrix: each row corresponds to a factor *)
    Outer[Coefficient[Last[#1] - First[#1], #2] & , fc, vr]];

(* shortcut: "Setup" *)
TopologyMatrixReduced[top:TopologyPattern["Setup"]] :=
  TopologyMatrixReduced[top, setp /. top];

(* trap *)
TopologyMatrixReduced[___] :=
  (Message[TopologyMatrixReduced::usage];
   Abort[]);

(* --- TopologyRank ------------------------------------------------- *)

TopologyRank::usage = "\
TopologyRank[<top>, [<set>]] gives the rank of the reduced topology \
matrix (coefficient matrix), thus the number of linearly independent \
factors of topology <top> according to the setup [<set>].";

(* main *)
TopologyRank[top:TopologyPattern[], set:SetupPattern[]] :=
  MatrixRank[TopologyMatrixReduced[top, set]];

(* shortcut: "Setup" *)
TopologyRank[top:TopologyPattern["Setup"]] :=
  TopologyRank[top, setp /. top];

(* trap *)
TopologyRank[___] :=
  (Message[TopologyRank::usage];
   Abort[]);

(* Needs:
- TopologyMatrixReduced[].
*)

(* --- TopologyCompleteQ -------------------------------------------- *)

TopologyCompleteQ::usage = "\
TopologyCompleteQ[<top(s)>, [<set>]] checks whether all appearing \
scalar products defined via the setup [<set>] can be expressed in \
terms of topology factors of <top(s)>.";

(* overload: plural *)
TopologyCompleteQ[tops_?TopologyListQ, set:Elective[SetupPattern[]]] :=
  TopologyCompleteQ[#, set] & /@ tops;

(* main *)
TopologyCompleteQ[top:TopologyPattern[], set:SetupPattern[]] :=
  TopologyRank[top, set] === (ns /. set);

(* shortcut: "Setup" *)
TopologyCompleteQ[top:TopologyPattern["Setup"]] :=
  TopologyCompleteQ[top, setp /. top];

(* trap *)
TopologyCompleteQ[___] :=
  (Message[TopologyCompleteQ::usage];
   Abort[]);

(* Needs:
- TopologyRank[].
*)

(* N.B.:
- By construction not possible:
    TopologyRank[<top>, <set>] > ns /. <set>.
*)

(* --- TopologyDependentQ ------------------------------------------- *)

TopologyDependentQ::usage = "\
TopologyDependentQ[<top(s)>, [<set>]] checks whether there are \
linearly dependent topology factors in <top(s)> according to the setup \
[<set>].";

(* overload: plural *)
TopologyDependentQ[tops_?TopologyListQ, set:Elective[SetupPattern[]]] :=
  TopologyDependentQ[#, set] & /@ tops;

(* main *)
TopologyDependentQ[top:TopologyPattern[], set:SetupPattern[]] :=
  TopologyRank[top, set] < Length[facs /. top];

(* shortcut: "Setup" *)
TopologyDependentQ[top:TopologyPattern["Setup"]] :=
  TopologyDependentQ[top, setp /. top];

(* trap *)
TopologyDependentQ[___] :=
  (Message[TopologyDependentQ::usage];
   Abort[]);

(* Needs:
- TopologyRank[].
*)

(* N.B.:
- By construction not possible:
    TopologyRank[<top>, <set>] > Length[facs /. <top>].
*)

(* --- TopologySolve ------------------------------------------------ *)

Options[TopologySolve] =
  Options[Solve];

TopologySolve::usage = "\
TopologySolve[<top>, [<set>]] tries to express scalar products listed \
in the setup [<set>] via topology factor symbols of <top>.
The result is given as a list of replacement rules.
Note that the factors of <top> should be linearly independent and \
complete in this sense for the operation to succeed fully.";

TopologySolve::dependent = "\
Warning: \"`1`\" still contains linearly dependent factor(s), it may \
have to be mapped to linearly independent sub-topologies first.";

TopologySolve::incomplete = "\
Warning: \"`1`\" has not enough factors to rewrite all scalar \
products, it needs to be completed for `2`.";

(* main *)
TopologySolve[
  top:TopologyPattern[], set:SetupPattern[],
    opts:OptionsPattern[]] :=
  Module[
    {fc, vr, rl, sl, cp},
    (* factors: scalar product symbols *)
    fc = facs /. top /. (is /. set);
    (* variables: scalar product symbols *)
    vr = First /@ (ss /. set);
    (* system of linear equations *)
    rl = Expand[Last[#] - First[#] == 0] & /@ fc;
    (* solve for scalar products *)
    sl = Quiet[Solve[rl, vr, opts]];
    (* pick only one solution *)
    If[Length[sl] > 0, sl = First[sl]];
    (* check: dependent *)
    If[sl === {},
       Message[TopologySolve::dependent,
         name /. top]];
    (* check: incomplete *)
    cp = Complement[vr, First /@ sl];
    If[sl =!= {} && cp =!= {},
       Message[TopologySolve::incomplete,
         name /. top, cp /. (ss /. set)]];
    (* result: scalar product expressions *)
    sl /. (ss /. set)];

(* shortcut: "Setup" *)
TopologySolve[top:TopologyPattern["Setup"]] :=
  TopologySolve[top, setp /. top];

(* trap *)
TopologySolve[___] :=
  (Message[TopologySolve::usage];
   Abort[]);

(*Off[TopologySolve::dependent];*)
(*Off[TopologySolve::incomplete];*)

(* N.B.:
- It is not allowed to throw an Abort[] since also partial results may
  be used elsewhere.
*)

(* --- TopologyReduce ----------------------------------------------- *)

Options[TopologyReduce] =
  Options[Reduce];

TopologyReduce::usage = "\
TopologyReduce[<top>, [<set>]] tries to express scalar products listed \
in the setup [<set>] via topology factor symbols of <top> and gives \
additional relations among linearly dependent topology factors, if \
such exist.
In contrast to TopologySolve[<top>, [<set>]] the result is given in \
form of two list.  The first consists of replacement rules for scalar \
products, the second contains expressions equal to zero stating linear \
relations involving topology factors.
Note that the factors of <top> should be complete in this sense for \
the operation to succeed fully.";

TopologyReduce::dependent = "\
Warning: \"`1`\" contains `2` linearly dependent factor(s) related by \
`3`.";

TopologyReduce::incomplete = "\
Warning: \"`1`\" has not enough factors to rewrite all scalar \
products, it needs to be completed for `2`.";

(* main *)
TopologyReduce[
  top:TopologyPattern[], set:SetupPattern[],
    opts:OptionsPattern[]] :=
  Module[
    {fc, vr, rl, rd, sr,fr, cp},
    (* factors: symbolic scalar products *)
    fc = facs /. top /. (is /. set);
    (* variables: factor and scalar product symbols *)
    vr = Join[First /@ fc, First /@ (ss /. set)];
    (* system of linear equations *)
    rl = Expand[Last[#] - First[#] == 0] & /@ fc;
    (* solve for scalar products *)
    rd = {ToRules[Reduce[rl, vr, opts]]};
    (* pick only one solution *)
    If[Length[rd] > 0, rd = First[rd]];
    (* scalar product relations *)
    sr = Cases[rd, HoldPattern[s[_] -> _]];
    (* factor relations *)
    fr = Complement[rd, sr];
    (* rewrite as zero *)
    fr = First[#] - Last[#] & /@ fr;
    (* check: dependent *)
    If[fr =!= {},
       Message[TopologyReduce::dependent,
         name /. top, Length[fr], fr /. (ss /. set)]];
    (* check: incomplete *)
    cp = Complement[First /@ (ss /. set), First /@ sr];
    If[cp =!= {},
       Message[TopologyReduce::incomplete,
         name /. top, cp /. (ss /. set)]];
    (* result, scalar product expressions, factor relations *)
    {sr, fr} /. (ss /. set)];

(* shortcut: "Setup" *)
TopologyReduce[top:TopologyPattern["Setup"]] :=
  TopologyReduce[top, setp /. top];

(* trap *)
TopologyReduce[___] :=
  (Message[TopologyReduce::usage];
   Abort[]);

Off[TopologyReduce::dependent];
Off[TopologyReduce::incomplete];

(* N.B.:
- It is not allowed to throw an Abort[] since also partial results may
  be used elsewhere (e.g. in TopologyScalarProducts[]).
- In contrast to TopologySolve[], TopologyReduce[] returns two lists,
  not just one.
*)

(* --- TopologyScalarProducts --------------------------------------- *)

Options[TopologyScalarProducts] =
  Options[TopologyReduce];

TopologyScalarProducts::usage = "\
TopologyScalarProducts[<top>, [<set>]] returns those scalar products \
defined in the setup [<set>] that cannot be expressed in terms of \
topology factors of <top>.";

TopologyScalarProducts::dependent =
  TopologyReduce::dependent;

(* main *)
TopologyScalarProducts[
  top:TopologyPattern[], set:SetupPattern[],
    opts:OptionsPattern[]] :=
  Module[
    {vr, rd, cp},
    (* variables: scalar products *)
    vr = Last /@ (ss /. set);
    (* reduce scalar products *)
    rd = Quiet[TopologyReduce[top, set, opts]];
    (* check: dependent *)
    If[Last[rd] =!= {},
       Message[TopologyScalarProducts::dependent,
         name /. top, Length[Last[rd]], Last[rd]]];
    (* inexpressible via factors *)
    cp = Complement[vr, First /@ First[rd]];
    (* result: rewrite as rules *);
    Select[ss /. set, MemberQ[cp, Last[#]] & ]];

(* shortcut: "Setup" *)
TopologyScalarProducts[top:TopologyPattern["Setup"]] :=
  TopologyScalarProducts[top, setp /. top];

(* trap *)
TopologyScalarProducts[___] :=
  (Message[TopologyScalarProducts::usage];
   Abort[]);

Off[TopologyScalarProducts::dependent];

(* Needs:
- TopologyReduce[].
*)

(* --- TopologyMasses ----------------------------------------------- *)

TopologyMasses::usage = "\
TopologyMasses[<top>, [<set>]] gives a list of expressions \
corresponding to the (quadratic) mass of each factor in the topology \
<top>, according to previous declarations in the setup [<set>].";

(* main *)
TopologyMasses[top:TopologyPattern[], set:SetupPattern[]] :=
  Function[f, Plus @@ Select[
    If[Head[f] === Plus, List @@ f, List @ f],
    (*Intersection[Variables[#], Union @@ ({xs, ms} /. set)] =!= {} &*)  (* TODO: ??? *)
    Intersection[Variables[#], ms /. set] =!= {} &
    ]] /@ Last /@ (facs /. top);

(* shortcut: "Setup" *)
TopologyMasses[top:TopologyPattern["Setup"]] :=
  TopologyMasses[top, setp /. top];

(* trap *)
TopologyMasses[___] :=
  (Message[TopologyMasses::usage];
   Abort[]);

(* --- TopologyMomentaFlows ----------------------------------------- *)

Options[TopologyMomentaFlows] =
  Options[Solve];

TopologyMomentaFlows::usage = "\
TopologyMomentaFlows[<top>, [<set>]] gives a list of expressions \
corresponding to linear combinations of external and internal momenta \
defined in the setup [<set>] flowing through each of the topology \
factors of <top>.
Note that the signs of the solutions are not fixed in general.";

TopologyMomentaFlows::noflows = "\
Warning: Could not reconstruct momenta flow(s) for `1`.";

(* main: atomic *)
$TopologyMomentaFlows[
  x_, set:SetupPattern[],
  opts:OptionsPattern[]] :=
  Module[
    {c, vr, mf, zr, rl, sl},
    (* variables: momentum coefficient symbols *)
    vr = c /@ (vs /. set);
    (* generic momenta flow *)
    mf = vr . (vs /. set);
    (* rules for constants, masses *)
    zr = # -> 0 & /@ (Join @@ {xs, ms} /. set);
    (* system of equations for scalar products *)
    rl = Expand[mf^2 - x] /. (cs /. set) /. zr;
    rl = Coefficient[rl, #] == 0 & /@ (Last /@ (ss /. set));
    (* solve for momentum coefficients *)
    sl = Quiet[Solve[rl, vr, opts]];
    sl = Reverse[Sort[sl]];
    (* pick only one solution *)
    If[Length[sl] > 0, sl = First[sl]];
    (* check: no solution *)
    If[sl === {} || Length[sl] < Length[vr],
       Message[TopologyMomentaFlows::noflows,
         x];
       Return[0]];
    (* result: momentum flow expression *)
    mf /. sl];

(* main *)
TopologyMomentaFlows[
  top:TopologyPattern[], set:SetupPattern[],
    opts:OptionsPattern[]] :=
  Module[
    {mfs, ck},
    (* call for each factor *)
    mfs = Quiet[$TopologyMomentaFlows[#, set, opts]] &
      /@ Last /@ (facs /. top);
    (* check: flows reconstructed *)
    ck = # === 0 & /@ mfs;
    If[Or @@ ck,
       Message[TopologyMomentaFlows::noflows,
         Pick[Last /@ (facs /. top), ck]]];
    (* result: all momenta flows *)
    mfs];

(* shortcut: "Setup" *)
TopologyMomentaFlows[top:TopologyPattern["Setup"]] :=
  TopologyMomentaFlows[top, setp /. top];

(* trap *)
TopologyMomentaFlows[___] :=
  (Message[TopologyMomentaFlows::usage];
   Abort[]);

SetOptions[
  TopologyMomentaFlows,
  Method -> Reduce];

Off[TopologyMomentaFlows::noflows];

(* N.B.:
- A generic linear combination for the momenta flow is introduced.
- From the relation for the topology factor one obtains a relation for
  each appearing scalar product by comparing coefficients.
- The emerging system is over-constrained and one tries to solve it for
  the coefficients in the linear combination.
*)

(* --- TopologyMomentaShifts ---------------------------------------- *)

$TopologyMomentaShiftsMethods =
{(* system of equations *)
 "Momenta", "ScalarProducts", "Bounded",
 (* method of solution *)
 "FindInstance", "FindInstanceReduce", "Reduce", "Solve",
 (* verify the result *)
 "Check"};

Options[TopologyMomentaShifts] =
{Method
   -> {"Momenta", "FindInstanceReduce", "Check"},
 Transformations
   -> {{}, {Global`p1 -> Global`p2, Global`p2 -> Global`p1}},
 Verbosity
   -> False};
(* cf. $TopologyMomentaShiftsMethods *)

Options[TopologyMomentaShifts] =
  Join @@ Options /@ {TopologyMomentaShifts, TopologyMomentaFlows};

TopologyMomentaShifts::usage = "\
TopologyMomentaShifts[<src>, <trg>, [<set>], [opts]] tries to compute \
shifts in loop momenta declared in the setup [<set>] to be applied to \
the set of factors of the source topology <src> in order to arrive at \
those of the target topology <trg>.
The option [Method] allows steering the way of computation by combining
- the considered system of equations from
    {\"Momenta\", \"ScalarProducts\"},
- the solution method from
    {\"FindInstance\", \"FindInstanceReduce\", \"Reduce\", \"Solve\"},
- the flags \"Bounded\" for imposing integer bounds and \"Check\" for \
performing a security check on the solution
into a list of strings.  The option [Transformations] can be used to \
allow for additional modifications by providing (lists of) \
transformation rules, e.g. of external momenta.";

TopologyMomentaShifts::transformations = "\
Warning: Invalid option value(s) `1` for [Transformations] will be \
ignored.";

TopologyMomentaShifts::missing = "\
Setting(s) missing in option value(s) `1` for [Method].
Possible choices are from `2` (as strings).";

TopologyMomentaShifts::noshifts = "\
Warning: Could not determine momenta shifts from \"`1`\" to \"`2`\".";

TopologyMomentaShifts::failed = "\
The found solution failed the security check with result `1`.";

(* overload: plural, plural *)
TopologyMomentaShifts[
  srcs_?TopologyListQ, trgs_?TopologyListQ,
  set:Elective[SetupPattern[]], opts:OptionsPattern[]] :=
  Outer[TopologyMomentaShifts[#1, #2, set, opts] & , srcs, trgs];

(* overload: plural, singular *)
TopologyMomentaShifts[
  srcs_?TopologyListQ, trg:TopologyPattern[],
  set:Elective[SetupPattern[]], opts:OptionsPattern[]] :=
  TopologyMomentaShifts[#, trg, set, opts] & /@ srcs;

(* overload: singular, plural *)
TopologyMomentaShifts[
  src:TopologyPattern[], trgs_?TopologyListQ,
    set:Elective[SetupPattern[]], opts:OptionsPattern[]] :=
  TopologyMomentaShifts[src, #, set, opts] & /@ trgs;

(* main *)
TopologyMomentaShifts[
  src:TopologyPattern[], trg:TopologyPattern[], set:SetupPattern[],
    opts:OptionsPattern[]] :=
  Module[
    {os, me,ts,vb, cp,ck, c, vr, mr, fs,gs, vl, ct, ln, solve, sl, tr},

    (* -- options -- *)

    os = FilterRules[{opts}, Options[TopologyMomentaFlows]];
    {me, ts, vb} = OptionValue[{Method, Transformations, Verbosity}];
    Report[vb, 1, "TopologyMomentaShifts[]."];

    (* Verbosity *)
    vb = vb /. VerbosityRules;
    If[vb > 0, c = Unique["Global`c"]];

    (* check: Method *)
    me = Flatten[{me}];
    cp = Complement[me, $TopologyMomentaShiftsMethods];
    If[cp =!= {},
       Message[Method::invalid, cp, $TopologyMomentaShiftsMethods];
       me = Complement[me, cp]];
    Report[vb, 2, "Method: ", me, "."];

    (* check: Transformations (form, identity) *)
    If[Head[ts] =!= List, ts = {ts}];
    If[FreeQ[ts, {}], PrependTo[ts, {}]];
    ck = MatchQ[#, _Rule | {___Rule}] & /@ ts;
    If[!And @@ ck,
       Message[TopologyMomentaShifts::transformations,
         Pick[ts, Not /@ ck]];
       ts = Pick[ts, ck]];
    Report[vb, 2, "Transformations: ", ts, "."];

    (* -- prepare variables, relations -- *)

    (* variables: momenta shift coefficient symbols *)
    vr = Join @@ Outer[c[#1, #2] & , ks /. set, vs /. set];

    (* generic momenta shift rules *)
    mr = Plus @@@ Outer[c[#1, #2]*#2 & , ks /. set, vs /. set];
    mr = MapThread[Rule, {ks /. set, mr}];
    Report[vb, 3, "Momenta shift rules:\n", TableForm[mr], "."];

    (* relations: dependent on Method *)
    Which[

      MemberQ[me, "Momenta"],
      {fs, gs} = TopologyMomentaFlows[#, set, Sequence @@ os] &
        /@ {src, trg};
      vl = c /@ Range[Length[fs]];
      fs = MapThread[Times, {vl, fs}];
      vr = Join[vr, vl];
      ct = vs /. set,

      MemberQ[me, "ScalarProducts"],
      {fs, gs} = Last /@ (facs /. #) & /@ {src, trg};
      ct = Join[xs /. set, Last /@ (ss /. set)],

      True,
      Message[TopologyMomentaShifts::missing,
        me, {"Momenta", "ScalarProducts"}];
      Abort[]];
    Report[vb, 3, "Variables: ", vr, "."];

    (* assure same number of factors *)
    ln = Min[Length /@ {fs, gs}];
    {fs, gs} = Take[#, ln] & /@ {fs, gs};
    Report[vb, 2, "Trying to match ", ln, " denominators."];

    (* -- try to find solution -- *)

    (* helper: treat one transformation *)
    solve[t_] := Module[
      {rl, sl},

      (* system of equations *)
      rl = Expand[(fs /. t /. mr) - gs] /. (cs /. set);
      rl = Join @@ Outer[Coefficient[#1, #2] & , rl, ct];
      rl = # == 0 & /@ rl;
      If[MemberQ[me, "Bounded"],
         rl = Join[rl, Join @@ ({-2 <= #, # <= +2} & /@ vr)]];
      Report[vb, 4, "System of equations:\n", TableForm[rl], "."];

      (* solve for momenta shift coefficients *)
      sl = Which[

        MemberQ[me, "FindInstance"],
        FindInstance[And @@ rl, vr, Integers, 1],

        MemberQ[me, "FindInstanceReduce"],
        FindInstance[
          Reduce[And @@ rl, vr, Integers],
          vr, Integers, 1],

        MemberQ[me, "Reduce"],
        {ToRules[Reduce[And @@ rl, vr, Integers]]},

        MemberQ[me, "Solve"],
        Quiet[Solve[rl, vr, Integers, Method -> Reduce]],

        True,
        Message[TopologyMomentaShifts::missing, me,
          {"FindInstance", "FindInstanceReduce", "Reduce", "Solve"}];
        Abort[]];
      sl = Reverse[Sort[sl]];
      Report[vb, 3, "All solutions:\n", TableForm[sl], "."];

      (* throw only one solution *)

      If[sl =!= {},
         Throw[{t, First[sl]}]];

      {t, sl}];

    (* scan over all transformations *)
    sl = Catch[Scan[solve, ts]];

    (* check: no solution *)
    If[sl =!= Null,
       {tr, sl} = sl,
       Message[TopologyMomentaShifts::noshifts,
         name /. src, name /. trg];
       Return[{}]];

    (* check: result *)
    If[MemberQ[me, "Check"],
       {fs, gs} = Last /@ (facs /. #) & /@ {src, trg};
       {fs, gs} = Take[#, ln] & /@ {fs, gs};
       ck = Expand[(fs /. tr /. mr /. sl) - gs] /. (cs /. set);
       Report[vb, 2, "Check: ", ck, "."];
       If[!And @@ (# === 0 & /@ ck),
          Message[TopologyMomentaShifts::failed,
           ck];
          Return[$Failed]]];

    (* result: transformations, momenta shifts *)
    Join[tr, mr /. sl]];

(* overload: source "Setup" *)
TopologyMomentaShifts[
  src:TopologyPattern["Setup"], trg:TopologyPattern[],
    opts:OptionsPattern[]] :=
  TopologyMomentaShifts[src, trg, setp /. src, opts];

(* overload: target "Setup" *)
TopologyMomentaShifts[
  src:TopologyPattern[], trg:TopologyPattern["Setup"],
    opts:OptionsPattern[]] :=
  TopologyMomentaShifts[src, trg, setp /. trg, opts];

(* trap *)
TopologyMomentaShifts[___] :=
  (Message[TopologyMomentaShifts::usage];
   Abort[]);

Off[TopologyMomentaShifts::noshifts];

(* Needs:
- TopologyMomentaFlows[].
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

- Let TopologySolve[], TopologyReduce[], TopologyMomentaShifts[] return
  all found solutions?  Would need to change to First[...] everywhere!

-

*)
