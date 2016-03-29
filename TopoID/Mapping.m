(* -- "Mapping.m": Data Type Pattern, Atomic Operations ------------- *)

(* --- provided functions:

MappingPattern[] -> <pat>
-- data type pattern

MappingQ[<exp>] -> <bool>
MappingListQ[<exp>] -> <bool>
MappingsQ[<exp>...] -> <bool>
-- corresponding query functions

Mapping[[<fr>], [<to>], [<id>]] -> <map>
-- corresponding constructor


PermuteMapping[<map(s)>, <perm>] -> <map(s)>
-- permute mapping(s)

ReverseMapping[<map(s)>] -> <map(s)>
-- reverse direction of mapping(s)

ComposeMapping[<map(s)>...] -> <map(s)>
-- combine into direct mapping(s)


SelectBackwardMapping[<map(s)>..., <key(s)>] -> {<map(s)>...}
SelectForwardMapping[<map(s)>..., <key(s)>] -> {<map(s)>...}
SelectMapping[<map(s)>..., <key(s)>] -> {<map(s)>...}
-- select parts of the mapping tree


MappingToRule[<map(s)>] -> <rule(s)>
MappingToSymbolRule[<map(s)>] -> <rule(s)>
MappingToPermuteRule[<map(s)>] -> <rule(s)>
-- convert to replacement rule(s)

MappingToGraph[<map(s)>...] -> <graph>
-- convert to graph data

ToMapping[<int(s)>, [opts]] -> <map(s)>
ToMapping[<rule(s)>, [opts]] -> <map(s)>
ToMapping[<expr>, [<key(s)>], [opts]] -> <map(s)>
-- convert arbitrary expressions into mapping(s)

SectorToMapping[<sec(s)>] -> <map(s)>
SectorToSubset[<sec(s)>] -> <list(s)>
-- convert sectors to mappings or line subsets

GroupToSector[<grp(s)>] -> <sec(s)>
-- convert group to sector notation

*)

(* --- package begin ------------------------------------------------ *)

Unprotect["TopoID`Mapping`*"];

ClearAll["TopoID`Mapping`*", "TopoID`Mapping`Private`*"];

BeginPackage[
  "TopoID`Mapping`",
  {"TopoID`Common`",
   "TopoID`Topology`"}];

{fr,to,id, MappingPattern,
 MappingQ, MappingListQ, MappingsQ,
 Mapping};

{PermuteMapping, ReverseMapping, ComposeMapping};

{SelectBackwardMapping, SelectForwardMapping, SelectMapping};

{MappingToRule, MappingToSymbolRule, MappingToPermuteRule,
 MappingToGraph,
 (*$ToMappingHash,*) (*$ToMappingInherit,*)
 $ToMappingUniqueRules, (*$ToMappingUnique,*) ToMapping};

{$SectorToMapping, SectorToMapping,
 SectorToSubset,
 GroupToSector};

Begin["`Private`"];

(* --- MappingPattern ----------------------------------------------- *)

fr::usage = "\
MappingPattern[]: Key indicating the source object name of a mapping \
which can be a string or zero.";

to::usage = "\
MappingPattern[]: Key indicating the target object name of a mapping \
which can be a string or zero.";

id::usage = "\
MappingPattern[]: Key indicating the extended permutation of a mapping \
that is a list of integers.";

MappingPattern::usage = "\
MappingPattern[] gives a data type pattern describing how one object \
is mapped onto an other.
Names of source and target objects (diagrams, topologies or integrals) \
stored in >fr< and >to< entries are strings or zero (for the case of \
scalelessness).
The mapping relation between source and target held in >id< is an \
extended permutation consisting of integers greater or equal to zero \
(indicating contraction of a line).  Element values in >id< refer to \
the target, their position to the source.  Hence, the length of an \
extended permutation gives the number of factors in the source, its \
maximum a lower limit on the number of factors of the target.  In case \
of integral objects the >id< entry can be interpreted as list of \
indices (allowing also for negative integers).";

(* data type pattern *)
MappingPattern[] :=
{fr -> _String | 0,    (* source object name *)
 to -> _String | 0,    (* target object name *)
 id -> {___Integer}};  (* extended permutation *)

(* trap *)
MappingPattern[___] :=
  (Message[MappingPattern::usage];
   Abort[]);

(* --- MappingQ ... ------------------------------------------------- *)

MappingQ::usage = "\
MappingQ[<map>] checks whether <map> matches the MappingPattern[] \
definition.";

(* pass *)
MappingQ[map_] :=
  MatchQ[map, MappingPattern[]];

(* trap *)
MappingQ[___] :=
  (Message[MappingQ::usage];
   Abort[]);

(* -- *)

MappingListQ::usage = "\
MappingListQ[<maps>] returns True if <maps> matches a list of mappings \
or and False otherwise.";

(* pass *)
MappingListQ[maps_] :=
  Head[maps] === List && And @@ (MatchQ[#, MappingPattern[]] & /@ maps);

(* trap *)
MappingListQ[___] :=
  (Message[MappingListQ::usage];
   Abort[]);

(* -- *)

MappingsQ::usage = "\
MappingsQ[<map(s)>...] returns True if <map(s)> matches a sequence \
composed of mappings or lists of mappings and False otherwise.";

(* pass *)
MappingsQ[maps___] :=
  And @@ (MappingQ[#] || MappingListQ[#] & /@ {maps});

(* Needs:
- MappingPattern[].
*)

(* --- Mapping ------------------------------------------------------ *)

Mapping::usage = "\
Mapping[[<fr>], [<to>], [<id>]] is a constructor for objects matching \
the MappingPattern[] definition where all arguments are optional.
The arguments [<fr>] and [<to>] can be zero, strings or objects \
referring to source and target, respectively.  The argument [<id>] can \
be an integer or a list of integers.
In case less than three arguments are given, an identical mapping is \
assumed and defaults are used as far as possible.";

(* main *)
Mapping[f:_String | 0, t:_String | 0, i:{___Integer}] :=
{fr -> f, to -> t, id -> i};

(* overload: no >id< *)
Mapping[ft:Repeated[_String | 0, {1, 2}]] :=
  Mapping[ft, {}];

(* overload: no >to< *)
Mapping[f:_String | 0, i:{___Integer}] :=
  Mapping[f, f, i];

(* overload: no >fr<, no >to< *)
Mapping[i:{___Integer}] :=
  Mapping["", "", i];

(* default *)
Mapping[] :=
  Mapping["", "", {}];

(* shortcut: source object *)
Mapping[src:TopologyPattern[], t___, i:{___Integer}] :=
  Mapping[name /. src, t, i];

(* shortcut: target object *)
Mapping[f_, trg:TopologyPattern[], i___] :=
  Mapping[f, name /. trg, i];

(* shortcut: identity, integer *)
Mapping[ft___, i_Integer] /; i >= 0 :=
  Mapping[ft, Range[i]];

(* shortcut: identity, source object *)
Mapping[src:TopologyPattern[], t___] :=
  Mapping[name /. src, t, Length[facs /. src]];

(* trap *)
Mapping[___] :=
  (Message[Mapping::usage];
   Abort[]);

(* --- PermuteMapping ----------------------------------------------- *)

PermuteMapping::usage = "\
PermuteMapping[<map(s)>, <perm>] applies the permutation <perm> to the \
mapping(s) given by <map(s)> (that is to the value of >id<).";

PermuteMapping::length = "\
`1` is too long to be reordered by `2`.";

PermuteMapping::noperm = "\
`1` is no valid permutation list.";

(* overload: plural *)
PermuteMapping[maps_?MappingListQ, pm_] :=
  PermuteMapping[#, pm] & /@ maps;

(* main: singular *)
PermuteMapping[
  map:MappingPattern[], pm_] /; PermutationListQ[pm] :=
  Module[
    {mp, pam},
    (* check: permutation length *)
    If[Max[id /. map] > Length[pm],
       Message[PermuteMapping::length, id /. map, pm];
       Abort[]];
    (* inverse permutation *)
    mp = InversePermutation[pm];
    (* contracted lines need no reordering *)
    pam = If[# === 0, 0, mp[[#]]] & /@ (id /. map);
    (* result: permuted mapping *)
    Mapping[fr /. map, to /. map, pam]];

(* trap: no permutation *)
PermuteMapping[map:MappingPattern[], pm_] :=
  (Message[PermuteMapping::noperm, pm];
   Abort[]);

(* trap *)
PermuteMapping[___] :=
  (Message[PermuteMapping::usage];
   Abort[]);

(* Needs:
- Mapping[].
*)

(* --- ReverseMapping ----------------------------------------------- *)

ReverseMapping::usage = "\
ReverseMapping[<map(s)>, [<t>]] reverses the direction of the \
mapping(s) <map(s)>.
Hereby one can optionally specify the number of factors in the target \
object via [<t>] which can be an integer or the object itself.";

(* overload: plural *)
ReverseMapping[maps_?MappingListQ, t_:0] :=
  ReverseMapping[#, t] & /@ maps;

(* main: singular *)
ReverseMapping[
  map:MappingPattern[], t_Integer:0] :=
  Module[
    {lu, um, pt},
    (* length of target set *)
    lu = Max[id /. map, t];
    (* unit mapping *)
    um = Range[lu];
    (* position of target element in mapping *)
    pt = Position[id /. map, #, 1, 1] /. {} -> {0} & /@ um;
    (* result: reversed mapping *)
    Mapping[to /. map, fr /. map, Flatten[pt]]];

(* shortcut: target object *)
ReverseMapping[map:MappingPattern[], trg:TopologyPattern[]] :=
  ReverseMapping[map, Length[facs /. trg]];

(* trap *)
ReverseMapping[___] :=
  (Message[ReverseMapping::usage];
   Abort[]);

(* Needs:
- Mapping[].
*)

(* --- ComposeMapping ----------------------------------------------- *)

ComposeMapping::usage = "\
ComposeMapping[<map(s)>...] composes the given mapping(s) into direct \
mapping(s).
This is done by first matching target names to source names and then \
appropriately combining the extended permutations.";

ComposeMapping::nocomposition = "\
`1` and `2` cannot be composed.";

ComposeMapping::nomatch = "\
Warning: Names of source \"`1`\" and target \"`2`\" objects do not \
match.";

(* overload: plural, plural *)
ComposeMapping[srcs_?MappingListQ, trgs_?MappingListQ] :=
  Union[DeleteCases[
    Join @@ (ComposeMapping[#, trgs] & /@ srcs), {}]];

(* overload: plural, singular *)
ComposeMapping[
  srcs_?MappingListQ, trg:MappingPattern[]] :=
  Module[
    {sel},
    sel = Select[srcs, (to /. #) === (fr /. trg) & ];
    Union[DeleteCases[ComposeMapping[#, trg] & /@ sel, {}]]];

(* overload: singular, plural *)
ComposeMapping[
  src:MappingPattern[], trgs_?MappingListQ] :=
  Module[
    {sel},
    sel = Select[trgs, (to /. src) === (fr /. #) & ];
    Union[DeleteCases[ComposeMapping[src, #] & /@ sel, {}]]];

(* main: atomic *)
ComposeMapping[
  src:{___Integer}, trg:{___Integer}] :=
  Module[
    {},
    (* case: target empty *)
    If[trg === {}, Return[{}]];
    (* check: target length *)
    If[Max[src] > Length[trg],
       Message[ComposeMapping::nocomposition, src, trg];
       Abort[]];
    (* result: composed mapping *)
    If[# === 0, 0, trg[[#]]] & /@ src];

(* main: singular, singular *)
ComposeMapping[
  src:MappingPattern[], trg:MappingPattern[]] :=
  Module[
    {cm},
    (* check: matching names *)
    If[(to /. src) =!= (fr /. trg),
       Message[ComposeMapping::nomatch, to /. src, fr /. trg];
       Return[{}]];
    (* compose extended permutations *)
    cm = ComposeMapping[id /. src, id /. trg];
    (* result: composed mapping *)
    Mapping[fr /. src, to /. trg, cm]];

(* default *)
ComposeMapping[] := {};

(* overload: multiple mapping(s) *)
ComposeMapping[maps___?MappingsQ] :=
  Fold[ComposeMapping, First[{maps}], Rest[{maps}]];

(* overload: rewrite list as sequence *)
ComposeMapping[maps_List] /; MappingsQ @@ maps :=
  ComposeMapping[Sequence @@ maps];

(* trap *)
ComposeMapping[___] :=
  (Message[ComposeMapping::usage];
   Abort[]);

(* N.B.:
- Union gets rid of identical mapping(s) from loops in the mapping tree.
*)

(* --- SelectMapping ... -------------------------------------------- *)

SelectBackwardMapping::keys = SelectForwardMapping::keys =
  SelectMapping::keys = "\
Warning: Invalid selection key(s) `1` will be ignored.";

(* -- *)

SelectBackwardMapping::usage = "\
SelectBackwardMapping[<map(s)>..., <key(s)>] returns the part of the \
mapping tree given by <map(s)> ending with object names specified via \
<key(s)>.
Elements in <key(s)> can have the heads String, StringExpression or \
RegularExpression";

(* main: backward *)
SelectBackwardMapping[
  maps_List, keys_List] /; And @@ (MappingListQ /@ maps) :=
  Module[
    {sl,cp, f, lt, ks, mt},
    (* end recursion *)
    If[maps === {}, Return[{}]];
    (* check: keys *)
    sl = Select[keys, MemberQ[
      {String, StringExpression, RegularExpression}, Head[#]] & ];
    cp = Complement[keys, sl];
    If[cp =!= {},
       Message[SelectMapping::keys, cp]];
    (* helper: matching function *)
    f = Or @@ (Function[k, StringMatchQ[#, k]] /@ sl) & ;
    (* last part *)
    lt = Select[Last[maps], f[to /. #] & ];
    (* combine keys *)
    ks = Union[keys, fr /. # & /@ lt];
    (* recurse in most *)
    mt = SelectBackwardMapping[Most[maps], ks];
    (* result: combine *)
    Append[mt, lt]];

(* overload: rewrite mapping(s) as list *)
SelectBackwardMapping[
  maps___?MappingsQ, keys___] /; FreeQ[{keys}, MappingPattern[]] :=
  SelectBackwardMapping[{maps}, keys];

(* overload: rewrite key(s) as list *)
SelectBackwardMapping[maps_List, keys___] /; MappingsQ @@ maps :=
  SelectBackwardMapping[maps, {keys}];

(* overload: rewrite single mapping(s) *)
SelectBackwardMapping[maps_List, keys_List] /; MappingsQ @@ maps :=
  SelectBackwardMapping[If[MappingQ[#], {#}, #] & /@ maps, keys];

(* trap *)
SelectBackwardMapping[___] :=
  (Message[SelectBackwardMapping::usage];
   Abort[]);

(* -- *)

SelectForwardMapping::usage = "\
SelectForwardMapping[<map(s)>..., <key(s)>] returns the part of the \
mapping tree given by <map(s)> starting with object names specified \
via <key(s)>.
Elements in <key(s)> can have the heads String, StringExpression or \
RegularExpression";

(* main: forward *)
SelectForwardMapping[
  maps_List, keys_List] /; And @@ (MappingListQ /@ maps) :=
  Module[
    {sl,cp, f, ft, ks, rt},
    (* end recursion *)
    If[maps === {}, Return[{}]];
    (* check: keys *)
    sl = Select[keys, MemberQ[
      {String, StringExpression, RegularExpression}, Head[#]] & ];
    cp = Complement[keys, sl];
    If[cp =!= {},
       Message[SelectMapping::keys, cp]];
    (* helper: matching function *)
    f = Or @@ (Function[k, StringMatchQ[#, k]] /@ sl) & ;
    (* first part *)
    ft = Select[First[maps], f[fr /. #] & ];
    (* combine keys *)
    ks = Union[keys, to /. # & /@ ft];
    (* recurse in rest *)
    rt = SelectForwardMapping[Rest[maps], ks];
    (* result: combine *)
    Prepend[rt, ft]];

(* overload: rewrite mapping(s) as list *)
SelectForwardMapping[
  maps___?MappingsQ, keys___] /; FreeQ[{keys}, MappingPattern[]] :=
  SelectForwardMapping[{maps}, keys];

(* overload: rewrite key(s) as list *)
SelectForwardMapping[maps_List, keys___] /; MappingsQ @@ maps :=
  SelectForwardMapping[maps, {keys}];

(* overload: rewrite single mapping(s) *)
SelectForwardMapping[maps_List, keys_List] /; MappingsQ @@ maps :=
  SelectForwardMapping[If[MappingQ[#], {#}, #] & /@ maps, keys];

(* trap *)
SelectForwardMapping[___] :=
  (Message[SelectForwardMapping::usage];
   Abort[]);

(* -- *)

SelectMapping::usage = "\
SelectMapping[<map(s)>..., <key(s)>] returns the part of the mapping \
tree given by <map(s)> involving object names specified via <key(s)>.
Elements in <key(s)> can have the heads String, StringExpression or \
RegularExpression";

(* main *)
SelectMapping[
  maps_List, keys_List] /; And @@ (MappingListQ /@ maps) :=
  Module[
    {sl,cp, f, l, bw,fw},
    (* end recursion *)
    If[maps === {}, Return[{}]];
    (* check: keys *)
    sl = Select[keys, MemberQ[
      {String, StringExpression, RegularExpression}, Head[#]] & ];
    cp = Complement[keys, sl];
    If[cp =!= {},
       Message[SelectMapping::keys, cp]];
    (* helper: matching function *)
    f = Or @@ (Function[k, StringMatchQ[#, k]] /@ sl) & ;
    (* first appearance of a key *)
    l = LengthWhile[maps, And @@ (!f[fr /. #] & /@ #) & ];
    (* check: search only forward *)
    If[l === 0,
       Return[SelectForwardMapping[maps, sl]]];
    (* check: search only backward *)
    If[l === Length[maps],
       Return[SelectBackwardMapping[maps, sl]]];
    (* search both directions *)
    bw = SelectBackwardMapping[Take[maps, l + 1], sl];
    fw = SelectForwardMapping[Drop[maps, l], sl];
    (* result: combine both *)
    Join[Most[bw], fw]];

(* overload: rewrite mapping(s) as list *)
SelectMapping[
  maps___?MappingsQ, keys___] /; FreeQ[{keys}, MappingPattern[]] :=
  SelectMapping[{maps}, keys];

(* overload: rewrite key(s) as list *)
SelectMapping[maps_List, keys___] /; MappingsQ @@ maps :=
  SelectMapping[maps, {keys}];

(* overload: rewrite single mapping(s) *)
SelectMapping[maps_List, keys_List] /; MappingsQ @@ maps :=
  SelectMapping[If[MappingQ[#], {#}, #] & /@ maps, keys];

(* trap *)
SelectMapping[___] :=
  (Message[SelectMapping::usage];
   Abort[]);

(* N.B.:
- Do not remove empty sets intentionally.
- Position of last overload must be like this.
*)

(* --- MappingToRule ------------------------------------------------ *)

MappingToRule::usage = "\
MappingToRule[<map(s)>] converts the given mapping(s) <map(s)> into \
symbolic replacement rule(s).";

MappingToRule::nosymbol = "\
Cannot convert the string(s) `1` into valid symbol(s).";

(* overload: plural *)
MappingToRule[maps_?MappingListQ] :=
  Union[MappingToRule /@ maps];

(* main *)
MappingToRule[
  map:MappingPattern[]] :=
  Module[
    {hs, cs},
    (* conversion to symbols *)
    hs = Quiet[Check[Symbol[#], #]] & /@ ({fr, to} /. map);
    (* check: no symbol *)
    cs = Cases[hs, _String];
    If[cs =!= {},
       Message[MappingToRule::nosymbol, cs];
       Abort[]];
    (* result: replacement rule *)
    Last[hs] -> First[hs] @@ (id /. map)];

(* trap *)
MappingToRule[___] :=
  (Message[MappingToRule::usage];
   Abort[]);

(* --- MappingToSymbolRule ------------------------------------------ *)

MappingToSymbolRule::usage = "\
MappingToSymbolRule[<map(s)>] converts the given mapping(s) <map(s)> \
into symbolic replacement rule(s) without index list.";

MappingToSymbolRule::nosymbol = "\
Cannot convert the string(s) `1` into valid symbol(s).";

(* overload: plural *)
MappingToSymbolRule[maps_?MappingListQ] :=
  Union[MappingToSymbolRule /@ maps];

(* main *)
MappingToSymbolRule[
  map:MappingPattern[]] :=
  Module[
    {hs, cs},
    (* conversion to symbols *)
    hs = Quiet[Check[Symbol[#], #]] & /@ ({fr, to} /. map);
    (* check: no symbol *)
    cs = Cases[hs, _String];
    If[cs =!= {},
       Message[MappingToSymbolRule::nosymbol, cs];
       Abort[]];
    (* result: replacement rule *)
    First[hs] -> Last[hs]];

(* trap *)
MappingToSymbolRule[___] :=
  (Message[MappingToSymbolRule::usage];
   Abort[]);

(* --- MappingToPermuteRule ----------------------------------------- *)

MappingToPermuteRule::usage = "\
MappingToPermuteRule[<map(s)>] returns a rule (list) to apply the \
extended permutation(s) described by the mapping(s) <map(s)> to \
functions with matching heads.";

MappingToPermuteRule::nosymbol = "\
Cannot convert the string(s) `1` into valid symbol(s).";

(* overload: plural *)
MappingToPermuteRule[maps_?MappingListQ] :=
  MappingToPermuteRule /@ maps;

(* main *)
MappingToPermuteRule[
map:MappingPattern[]] :=
  Module[
    {p, hs, cs, as,ps},
    (* helper: permutation *)
    p = {If[#2 =!= 0, ReplacePart[
      First[#1], #2 -> First[Last[#1]]], First[#1]], Rest[Last[#1]]} & ;
    (* conversion to symbols *)
    hs = Quiet[Check[Symbol[#], #]] & /@ ({fr, to} /. map);
    (* check: no symbol *)
    cs = Cases[hs, _String];
    If[cs =!= {},
       Message[MappingToPermuteRule::nosymbol, cs];
       Abort[]];
    (* pattern symbols *)
    as = MapIndexed[Symbol["a" <> ToString[First[#2]]] & , id /. map];
    ps = Pattern[#, _] & /@ as;
    (* result: replacement rule *)
    Rule[
      First[hs] @@ MapIndexed[
        If[#1 === 0, 0, ps[[First[#2]]]] & , id /. map],
      Last[hs] @@ First[Fold[
        p, {Table[0, {Max[id /. map]}], as}, id /. map]]]];

(* trap *)
MappingToPermuteRule[___] :=
  (Message[MappingToPermuteRule::usage];
   Abort[]);

(* --- MappingToGraph ----------------------------------------------- *)

MappingToGraph::usage = "\
MappingToGraph[<map(s)>...] converts the mapping tree given by \
<map(s)> into standard Mathematica graph data.";

(* main *)
MappingToGraph[
  maps___?MappingListQ] :=
{Rule @@ {fr, to} /. #, id /. #} & /@ Join @@ {maps};

(* overload: rewrite single mapping(s) *)
MappingToGraph[maps___] /; MappingsQ[maps] :=
  MappingToGraph[
    Sequence @@ If[MappingQ[#], {#}, #] & /@ {maps}];

(* overload: rewrite list as sequence *)
MappingToGraph[maps_List] /; MappingsQ @@ maps :=
  MappingToGraph[Sequence @@ maps];

(* trap *)
MappingToGraph[___] :=
  (Message[MappingToGraph::usage];
   Abort[]);

(* --- ToMapping ---------------------------------------------------- *)

$ToMappingHash =
  ToUpperCase[IntegerString[Hash[#, "Adler32"], 36, 7]] & ;

(* N.B.:
- Adjust length of result returned by IntegerString[].
- Adjust hash type to one of:
  "Adler32", "CRC32", "MD2", "MD5", "SHA", "SHA256", "SHA384", "SHA512".
*)

$ToMappingInherit[l:{___Integer}] :=
  Module[
    {i, ds,ns, ld,ln, sd,sn},
    (* denominators -> lines *)
    ds = Cases[l, i_ /; i > 0];
    (* numerators -> scalar products *)
    ns = Cases[l, i_ /; i < 0];
    (* numbers *)
    {ld, ln} = ToString /@ Length /@ {ds, ns};
    (* sums of positive, negative indices *)
    {sd, sn} = ToString /@ Total /@ {ds, -ns};
    "L" <> ld <> "S" <> ln <> "P" <> sd <> "N" <> sn];

$ToMappingUniqueRules = {};

$ToMappingUnique[i:_Symbol[___Integer]] :=
  Module[
    {j},
    (* check: already old index in database *)
    j = i /. $ToMappingUniqueRules;
    If[Head[j] === Integer,
       Return[j]];
    (* rules involving same head *)
    j = Select[$ToMappingUniqueRules, Head[First[#]] === Head[i] & ];
    (* extract biggest used index, increment by one *)
    j = Max[0, Last /@ j] + 1;
    (* result: add new index to database *)
    AppendTo[$ToMappingUniqueRules, i -> j];
    j];

(* -- *)

(* #1: integral _Symbol[___Integer], #2: index _Integer *)
NamingRules[ToMapping] =
{"Hash" ->
   (ToString[Head[#]] <> "m" <> $ToMappingHash[List @@ #] & ),

 "Inherit" ->
   (ToString[Head[#]] <> "m" <> $ToMappingInherit[List @@ #] & ),

 "Unique" ->
   (ToString[Head[#]] <> "m" <> ToString[$ToMappingUnique[#]] & ),

 "InheritUnique" ->
   (ToString[Head[#]]
    <> "m" <> $ToMappingInherit[List @@ #]
    <> "v" <> ToString[$ToMappingUnique[#]] & ),

 Inherit ->
   (ToString[Head[#]] <> "m" <> StringReplace[
     StringJoin @@ ToString /@ #, "-" -> "N"] & ),
 Inherit[s_String] ->
   (ToString[Head[#]] <> s <> StringReplace[
     StringJoin @@ ToString /@ #, "-" -> "N"] & ),
 Inherit[s_Symbol] ->
   (ToString[s] <> StringReplace[
     StringJoin @@ ToString /@ #, "-" -> "N"] & ),

 Iterate | InheritIterate ->
   (ToString[Head[#1]] <> "m" <> ToString[#2] & ),
 Iterate[s_String] ->
   (s <> ToString[#2] & ),
 Iterate[s_Symbol] ->
   (ToString[s] <> ToString[#2] & ),
 Iterate[i_Integer] ->
   (ToString[Head[#1]] <> "m" <> ToString[i + #2] & ),
 Iterate[s_String, i_Integer] | Iterate[i_Integer, s_String] ->
   (s <> ToString[i + #2] & ),
 Iterate[s_Symbol, i_Integer] | Iterate[i_Integer, s_Symbol] ->
   (ToString[s] <> ToString[i + #2] & ),

 InheritIterate[s_String] ->
   (ToString[Head[#1]] <> s <> ToString[#2] & ),

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
   If[Head[f[Null[0], 1]] === String, f,
      Message[NamingRules::function]; Abort[]],

 x_ :>
   (Message[NamingRules::keys, x, $NamingRulesKeys]; Abort[])};

(* -- *)

Options[ToMapping] =
{Naming -> "Unique"};  (* cf. NamingRules[ToMapping] *)

ToMapping::usage = "\
ToMapping[<expr>, [<key(s)>], [opts]] extracts from arbitrary \
expressions objects matching the MappingPattern[] definition.
The optional argument [<key(s)>] can contain expressions with heads \
Symbol, String, StringExpression or RegularExpression in order to \
discriminate names for which mappings are generated.
The option [Naming] controls the generation of target names, \
cf. NamingRules[ToMapping].";

ToMapping::keys = "\
Warning: Invalid selection key(s) `1` will be ignored.";

(* default *)
ToMapping[] :=
{};

(* -- *)

(* overload: function, plural *)
ToMapping[
  is_List, opts:OptionsPattern[]] /;
And @@ (MatchQ[#, _Symbol[___Integer]] & /@ is) :=
  Module[
    {ng, ns},
    ng = OptionValue[Naming] /. NamingRules[ToMapping];
    ns = MapIndexed[ng[#1, First[#2]] & , is];
    Union[MapThread[ToMapping[#1, Naming -> #2, opts] &, {is, ns}]]];

(* main: function, singular *)
ToMapping[i:_Symbol[___Integer], opts:OptionsPattern[]] :=
  Mapping[
    ToString[Head[i]],
    (OptionValue[Naming] /. NamingRules[ToMapping])[i, 1],
    List @@ i];

(* -- *)

(* overload: rule, plural *)
ToMapping[rs:{___Rule}, opts:OptionsPattern[]] :=
  Union[DeleteCases[ToMapping[#, opts] & /@ rs, {}]];

(* main: source -> target *)
ToMapping[s:_Symbol[___Integer] -> t_, opts:OptionsPattern[]] /;
!MatchQ[t, _Symbol[___Integer]] :=
  Mapping[
    ToString[Head[s]],
    If[Head[t] === String, t, ToString[t]],
    List @@ s];

(* overload: target -> source *)
ToMapping[t_ -> s:_Symbol[___Integer], opts:OptionsPattern[]] /;
!MatchQ[t, _Symbol[___Integer]] :=
  ToMapping[s -> t, opts];

(* overload: rule, singular *)
ToMapping[r_Rule, opts:OptionsPattern[]] :=
  Union[DeleteCases[Join @@ (ToMapping[#, opts] & /@ List /@ r), {}]];

(* -- *)

(* overload: expression, selection keys *)
ToMapping[
  x_, ks___:{}, opts:OptionsPattern[]] :=
  Module[
    {sl,cp, f, iss},
    (* check: keys *)
    sl = Select[Flatten[{ks}], MemberQ[
      {Symbol, String, StringExpression, RegularExpression},
      Head[#]] & ];
    cp = Complement[Flatten[{ks}], sl];
    If[cp =!= {},
       Message[ToMapping::keys, cp]];
    (* symbols -> strings *)
    sl = If[Head[#] === Symbol, ToString[#], #] & /@ sl;
    (* helper: matching function *)
    f = Or @@ (Function[k, StringMatchQ[#, k]] /@ sl) & ;
    (* all variables *)
    iss = Variables[Cases[x, _Symbol[___Integer], {0, Infinity}]];
    (* select heads by keys *)
    If[sl =!= {},
       iss = Select[iss, f[ToString[Head[#]]] & ]];
    (* result: pass *)
    ToMapping[iss, opts]];

(* -- *)

(* trap *)
ToMapping[___] :=
  (Message[ToMapping::usage];
   Abort[]);

(* --- SectorToMapping, SectorToSubset ------------------------------ *)

SectorToMapping::usage = "\
SectorToMapping[<expr>, [<key(s)>], [opts]] extracts from arbitrary \
expressions objects matching the MappingPattern[] definition, in \
analogy to ToMapping[], which are understood to denote sectors.";

$SectorToMapping[sec:{___Integer}] :=
  Last[Fold[
    If[
      #2 === 0,
      {First[#1], Append[Last[#1], 0]},
      {First[#1] + 1, Append[Last[#1], First[#1]]}] & ,
    {1, {}}, sec]];

SectorToMapping[x_, ks___:{}, opts:OptionsPattern[]] :=
  (ToMapping[x, ks, opts]
   /. Rule[id, y_] :> Rule[id, $SectorToMapping[y]]);

(* trap *)
SectorToMapping[___] :=
  (Message[SectorToMapping::usage];
   Abort[]);

(* -- *)

SectorToSubset::usage = "\
SectorToSubset[<expr>, [<key(s)>], [opts]] extracts from arbitrary \
expressions objects matching the MappingPattern[] definition, in \
analogy to ToMapping[], which are understood to denote sectors and \
converts them into the subset notation.";

SectorToSubset[x_, ks___:{}, opts:OptionsPattern[]] :=
  id /. ReverseMapping[SectorToMapping[x, ks, opts]] /. id -> {};

(* trap *)
SectorToSubset[___] :=
  (Message[SectorToSubset::usage];
   Abort[]);

(* --- GroupToSector ------------------------------------------------ *)

GroupToSector::usage = "\
GroupToSector[<group(s)>] converts the line subset(s) <group(s)> to \
sector notation.";

(* main: singular *)
GroupToSector[sg:{___Integer}, n_Integer:-1] :=
  ReplacePart[Table[0, {Max[sg, n]}], # -> 1 & /@ sg];

(* overload: plural *)
GroupToSector[sgs:{__List}, n_Integer:-1] :=
  GroupToSector[#, Max[sgs, n]] & /@ sgs;

(* trap *)
GroupToSector[___] :=
  (Message[GroupToSector::usage];
   Abort[]);

(* --- package end -------------------------------------------------- *)

Protect["TopoID`Mapping`*"];

Scan[
  SetAttributes[#, {ReadProtected}] & ,
  Select[Symbol /@ Names["TopoID`Mapping`*"], Head[#] === Symbol & ]];

Unprotect[$ToMappingUniqueRules];
ClearAttributes[$ToMappingUniqueRules, {ReadProtected}];

End[];

EndPackage[];

(* ------------------------------------------------------------------ *)



(* --- TODO:

- SectorToSubset[] -> SectorToGroup[]?

- ReverseMapping[]: use GroupToSector[]?

- GroupToSector[]: move? where?

*)
