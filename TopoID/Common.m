(* -- "Common.m": Global Variables and Common Definitions ----------- *)

SetOptions[ParallelMap, DistributedContexts -> Automatic];  (* TODO *)

BeginPackage["TopoID`Common`", {"TopoID`System`"}];  (* TODO: TEMP *)

(* --- global flags, variables: *)

(* current version *)
$TopoIDVersion =
  "2.0";

(* corresponding release date *)
$TopoIDDate =
  "2016-XX-XX";  (* TODO: update when done *)

(* list of provided functions *)
$TopoIDFunctions =
{};  (* TODO: fill when done; move to the end to prevent shadowing *)

$TopoIDSymbols =
{};  (* TODO: fill when done; move to the end to prevent shadowing *)

$TopoIDScale::usage = "\
TopoID: global flag whether to use extensive pattern matching on input \
to most functions.";

(* whether to use extensive pattern matching *)
$TopoIDData =
(*True;*)
  False;

$TopoIDScale::usage = "\
TopoID: global setting how to combine U and F to check for \
scalefulness.";

$TopoIDScale =
  Times;

$TopoIDMetric::usage = "\
TopoID: global setting how to combine U and F to contruct canonical \
metric.";

$TopoIDMetric = If[
  ValueQ[Global`TopoIDMetric],
  Global`TopoIDMetric,
  Times];
(*Plus;*)

(* N.B.:
- How to combine U and W to
  * construct polynomial to check for scalefulness,
  * construct metric for canonical ordering.
*)




{TopoID};





(* --- common symbols (for wrappers): *)

{(*ep,*) a, e};

Global`ep::usage = "\
Symbol used for the parameter describing the deviation from four \
space-time dimensions within dimensional regularization and appearing \
in generated integration-by-parts identities (IBPs).";

a::usage = "\
Symbol used (as wrapper) for indices, e.g. in integration-by-parts \
identities (IBPs).";

e::usage = "\
Symbol used (as wrapper) for external line, e.g. in graph labels.";

(* --- common symbols denoting options: *)

{Externals,
 Internals,
 Masses,
 (*Constants,*)
 Constraints,
 Rules};

{Exclude,
 Naming,
 Parallel,
 Settings,
 Simplifier,
 Transformations,
 Verbosity};

{BaseScaleFunction, IncludeFunction, LoadingFunction};

(* TODO: usage *)

BaseScaleFunction::usage = "\
Option available for certain FORM code generation routines requiring a \
dimensionful global factor.  The corresponding option value should be \
a function to which the following arguments are passed:
  #1: TopologyPattern[], #2: SetupPattern[].";

(* TODO: usage *)

(* --- common symbols storing information: *)

{Defaults,
 MethodRules,
 NamingRules};

(* TODO: usage *)

MethodRules::norule = "\
No rule for assignment to option [Method] matching `1`.";

NamingRules::usage = "\
TODO";

(* TODO: usage *)

(* --- common defines, symbols: *)

{Elective,
 Inherit,Iterate,InheritIterate, $NamingRulesKeys,
 VerbosityRules};

(* --- common helpers: *)

{$CheckKeys, $CheckName, $CheckMethod, $CheckLevel,
 $AppendOptions, $AppendToOptions};

{$FlagRules};

(* --- where to put this: *)                                            (* TODO: remove at some point *)

Unprotect[Method];

Method::invalid = "\
Warning: Invalid option value(s) for [Method] in `1` will be ignored.
Possible choices are from `2` (as strings).";

Protect[Method];

Begin["`Private`"];

(* --- Elective ----------------------------------------------------- *)

Elective::usage = "\
The enclosed pattern may be completely absent or appear exactly once.";

Elective[pat___] :=
  Longest[Repeated[PatternSequence[pat], {0, 1}]];


(* --- NamingRules -------------------------------------------------- *)

Inherit::usage = "\
TODO.";

Iterate::usage = "\
TODO.";

$NamingRulesKeys =
{Inherit, Inherit[_String],
 Iterate, Iterate[_Integer], Iterate[_String],
 Iterate[_String, _Integer], Iterate[_Integer, _String],
 InheritIterate[_String],
 _List, _Integer, _String, _Symbol, _Function};

NamingRules::list = "\
List passed as option value of [Naming] too short: `1`.";

NamingRules::function = "\
Function passed as option value of [Naming] failed sanity check (no \
string value produced).";

NamingRules::keys = "\
Invalid option value for [Naming]: `1`.
Use one of the following instead: `2`.";




(* --- VerbosityRules ----------------------------------------------- *)

(* VerbosityRules
description:
  Rules mapping every expression either onto integers or Infinity.
  The level of verbosity is specified as follows:
  - Corresponding to maximal verbosity are
      Automatic, All, Full, Infinity, On, or True.
  - An integer will be interpreted as a specific maximal level of
    verbosity.
  - Everything else results in no output at all (-1, -Infinity, False,
    None, Null, Off, or are suggested).
notes:
- Pairs and lists denoting verbosity ranges and specific single levels
  should be processed in the using functions after invoking
  VerbosityRules.
*)

VerbosityRules::usage = "\
TODO.";

VerbosityRules =
{Automatic | All | Full | Infinity | On | True -> Infinity,
 i_Integer -> i,
 _ -> 0};

(* sample usage:
Print[Automatic /. VerbosityRules];
Print[Full /. VerbosityRules];
Print[Infinity /. VerbosityRules];
Print[True /. VerbosityRules];
Print[1 /. VerbosityRules];
Print[10 /. VerbosityRules];
Print[-3 /. VerbosityRules];
Print[None /. VerbosityRules];
Print[No /. VerbosityRules];
Print[x /. VerbosityRules];
Print["No" /. VerbosityRules];
*)

(* sample output:
Infinity
Infinity
Infinity
Infinity
1
10
-3
0
0
0
0
*)


(* --- $CheckKeys --------------------------------------------------- *)  (* DONE *)

TopoID::keys = "\
Warning: Invalid selection key(s) `1` will be ignored.";

$CheckKeys[ks___] := Module[
  {sl, cp, f},
  (* check: keys *)
  sl = Select[Flatten[{ks}], MemberQ[
    {Symbol, String, StringExpression, RegularExpression}, Head[#]] & ];
  cp = Complement[Flatten[{ks}], sl];
  If[cp =!= {}, Message[TopoID::keys, cp]];
  (* case: nothing to match *)
  If[sl === {}, Return[Function[True]]];
  (* symbols -> strings *)
  sl = If[Head[#] === Symbol, ToString[#], #] & /@ sl;
  (* result: selection function *)
  Function[x, Or @@ (StringMatchQ[ToString[Head[x]], #] & /@ sl)]];

(* N.B.:
- Consider only symbols, strings or string patterns as keys.
- Return an adequate selection function.
*)

(* --- $CheckName --------------------------------------------------- *)  (* DONE *)

TopoID::name = "\
The topology name \"`1`\" cannot be converted to a valid symbol.";

$CheckName[tn_String] := Module[
  {ts},
  (* generate symbol *)
  ts = Quiet[Check[Symbol[tn], Null]];
  (* check: symbol *)
  If[ts === Null,
     Message[TopoID::name];
     Abort[]];
  (* result: symbol *)
  ts];

(* N.B.:
- Check if a topology name can be converted to a valid symbol.
- Return the symbol or throw an Abort[] otherwise.
*)

(* --- $CheckMethod ------------------------------------------------- *)  (* DONE *)

TopoID::method = "\
Warning: Invalid option value(s) for [Method] in `1` will be ignored.
Possible choices are from `2` (as strings).";

$CheckMethod[me__:{}, ma_:{}] := Module[
  {f, mf,mg, cp},
  f = If[Head[#] === String, "\"" <> # <> "\"", #] & ;
  {mf, mg} = {Flatten[{me}], Flatten[{ma}]};
  cp = Complement[mf, mg];
  If[cp =!= {},
     Message[TopoID::method, cp, f /@ mg];
     mf = Complement[mf, cp]];
  mf];

(* --- $CheckLevel -------------------------------------------------- *)  (* DONE *)

TopoID::level = "\
Warning: No valid level specification (\"levelspec\") given by `1`.  \
The default All will be used.
Use one of the following:
- All/Infinity -- all levels (equivalent to {1, nmax}),
- n -- down to n elements (equivalent to {n, nmax}),
- {n} -- exactly n elements,
- {na, nb/Infinity} -- between na and nb elements,
- {na, nb/Infinity, dn} -- between na and nb elements with distance dn,
where symbols denote positive integers and nmax is the number of \
available elements.";

$CheckLevel[lv_, nm_Integer] := Module[
  {n, lw, rls},
  (* rewrite <lv> *)
  rls = {All | Infinity -> {1, nm}, n_Integer -> {n, nm}};
  If[Head[lv] === List,
     rls = {All | Infinity -> nm}];
  lw = lv /. rls;
  If[!MatchQ[lw, {__Integer}] || Length[lw] > 3 || !And @@ Positive /@ lw,
     Message[TopoID::level, lv];
     lw = {1, nm}];
  lw];

(* N.B.:
- Check and rewrite to standard levelspec notation.
*)


(* --- $Append[To]Options ------------------------------------------- *)  (* DONE *)

(* main *)
$AppendOptions[trg_, srcs___] := Module[
  {ta, st,uf, trgos, srcsos},
  (* helper: transform argument *)
  ta = If[Head[#] === Symbol, Options[#], Flatten[{#}]] & ;
  (* helpers: same test, union function *)
  st = First[#1] === First[#2] & ;
  uf = Join[#1, Complement[#2, #1, SameTest -> st]] & ;
  (* arguments transformed to options lists *)
  trgos = ta @ trg;
  srcsos = ta /@ {srcs};
  (* unioned lists *)
  Sort[Fold[uf, trgos, srcsos]]];

(* alias *)
$AppendToOptions[trg_Symbol, srcs___] :=
  (Options[trg] = $AppendOptions[trg, srcs]);

(* N.B.:
- Adjoins multiple options to those of <trg>.
- Options of <trg> are not overwritten.
- Settings are preferred in the order of <srcs>.
- The result is free of duplicates and returned sorted.
*)

(* --- $FlagRules --------------------------------------------------- *)  (* DONE *)

$FlagRules =
{1 | All | Automatic | Full | On | True -> True,
 0 | None | Off | False -> False,
 _ -> False};













(* --- from old init.m --- *)

If[$FrontEnd === Null,

(* set output options *)
SetOptions[{"stdout", "stderr"},
           {PageWidth -> 100,
            TotalWidth -> Infinity,
            FormatType -> OutputForm}];

(* redefine Print (no quotes around strings) for batch mode *)

Unprotect[Print];

Print[pr___] :=
    WriteString["stdout",
                Sequence @@
                    (If[#[[0]] === String, #, InputForm[#]] & /@ ({pr} /. TableForm[x__] -> x)),
                "\n"];

Protect[Print];

];

(* ------------------------------------------------------------------ *)

(* TODO:
- Handle TableForm[MatrixForm[]]
- ...
*)



















(* ------------------------------------------------------------------ *)

End[];

EndPackage[];

(* ------------------------------------------------------------------ *)
