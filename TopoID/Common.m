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

(* --- common messages *)

TopoID::path = "\
Warning: Could not resolve path \"`1`\".";

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
 Methods,
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

{$ToString};

{$CheckKeys, $CheckName, $CheckMethod, $CheckLevel,
 $AppendOptions, $AppendToOptions};

{$FlagRules};

{$Print};

{CheckMethod};

{(*$FileDate,*) (*$WriteStringFileDate,*) WriteStringFile};

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

(* --- $ToString ---------------------------------------------------- *)

$ToString::usage = "\
$ToString[<arg(s)>] converts argument(s) <args(s)> to string(s) only \
if they are of different type.";

(* main *)
$ToString[x_] :=
  If[Head[x] === String, x, ToString[x, InputForm]];

(* overload: list *)
$ToString[l_List] :=
  $ToString /@ l;

(* overload: sequence *)
$ToString[xs___] :=
  StringJoin[$ToString /@ {xs}];

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

TopoID::method2 = "\
Warning: Invalid option value(s) for [Method] in `1` will be ignored.
Possible choices are from `2` (as strings).";

$CheckMethod[me__:{}, ma_:{}] := Module[
  {f, mf,mg, cp},
  f = If[Head[#] === String, "\"" <> # <> "\"", #] & ;
  {mf, mg} = {Flatten[{me}], Flatten[{ma}]};
  cp = Complement[mf, mg];
  If[cp =!= {},
     Message[TopoID::method2, cp, f /@ mg];
     mf = Complement[mf, cp]];
  mf];

(* --- CheckMethod -------------------------------------------------- *)

CheckMethod::usage = "\
CheckMethod[<x>, <list>] checks whether the setting for the option \
[Method] in <x> matches any of the patterns in <list>, reports and \
discards cases which do not pass this test.";

TopoID::unsmeths = "\
Warning: Methods[`1`] is not set to a list of valid patterns for the \
option value(s) of [Method] of `1`.";

TopoID::invmeths = "\
Warning: Invalid option value(s) for [Method] in `1` will be ignored.  \
For possible choices see Methods[`2`]";

CheckMethod[me_, fh_Symbol] := Module[
  {mf, mes, mg, cp},
  mf = Flatten[{me}];
  mes = Methods[fh];
  If[Head[mes] =!= List,
     Message[TopoID::unsmeths, fh];
     Return[me]];
  mes = Alternatives @@ mes;
  mg = Select[mf, MatchQ[#, mes] & ];
  cp = Complement[mf, mg];
  If[cp =!= {}, Message[TopoID::invmeths, cp, fh]];
  If[mg === {}, Return[{Automatic}]];
  mg];

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

(* --- $Print ------------------------------------------------------- *)

$Print::usage = "\
$Print[<expr(s)>] is an alternative to Print[] to write the \
expression(s) <expr(s)> on the standard output stream which may be \
advantageous (e.g. no quotes around strings) in batch mode.";

$Print[xs___] := Module[
  {f, ys},
  f = If[Head[#] === String, #, InputForm[#]] & ;
  ys = {xs} /. MatrixForm | TableForm -> Identity;
  WriteString["stdout", Sequence @@ (f /@ ys), "\n"]];





















(* --- WriteStringFile ---------------------------------------------- *)

(* fix for Mathematica 10 *)
$FileDate[fn_String] :=
  FileDate[fn] /. {DateObject -> Join, TimeObject -> Identity};

(* helper: formatted modification date of file <fn> or current date *)
$WriteStringFileDate[fn_String:""] := Module[
  {dt},
  dt = If[fn === "", DateList[], $FileDate[fn]];
  (* round date components *)
  dt = ToString /@ Round /@ dt;
  (* fix number of digits to at least two *)
  dt = If[StringLength[#] >= 2, #, "0" <> #] & /@ dt;
  (* apply formatting  *)
  StringJoin[
    dt[[1]], "-", dt[[2]], "-", dt[[3]], "_",
    dt[[4]], ":", dt[[5]], ":", dt[[6]]]];

Methods[WriteStringFile] =
{Automatic, Append, None, RenameFile};

MethodRules[WriteStringFile] =
{Automatic -> RenameFile};

Options[WriteStringFile] =
{FormatType -> InputForm,
 Method -> Automatic,
 Path -> "."};

$AppendToOptions[WriteStringFile, OpenWrite];

WriteStringFile::usage = "\
WriteStringFile[<file>, <expr(s)>, [opts]] works similar to \
WriteString[<stream>, <expr(s)>], but does not require <stream> opened \
beforehand.  Instead, the expressions <expr(s)> are converted to \
strings and written directly to <file>.
The option [Method] allows to control how to proceed in case <file> \
already exists:
- Append -- append new content to the old file,
- None -- overwrite the existing file,
- RenameFile (Automatic) -- rename the old file.
The option [Path] allows to specify the relative directory.";

WriteStringFile::switch = "\
Warning: Switched [Method] from `1` to `2`.";

WriteStringFile::moved = "\
Warning: The file \"`1`\" was moved to \"`2`\".";

WriteStringFile::failed = "\
Could not write to the file \"`1`\".";

WriteStringFile[fn_String, xs___, opts:OptionsPattern[]] := Module[  (* fn_String:"" *)
  {f, pt,me, optr, pr, ys, fm = fn, fo, rf, fs},
  (* helper: convert to string *)
  f = If[Head[#] === String, #, ToString[#, OptionValue[FormatType]]] & ;
  (*  options *)
  {pt, me} = OptionValue[{Path, Method}];
  me = CheckMethod[me, WriteStringFile];
  me = me /. MethodRules[WriteStringFile];
  optr = FilterRules[{opts}, Options[OpenWrite]];
  (* check: path name *)
  pr = AbsoluteFileName[pt];
  If[pr === $Failed, Message[TopoID::path, pt], pt = pr];
  (* convert input *)
  ys = f /@ {xs};
  (* file name *)
  fm = If[fn === "", Hold[Sequence[]], FileNameJoin[{pt, fm}]];
  (* method: rename file *)
  If[fn =!= "" && First[me] === RenameFile && FileExistsQ[fm],
     fo = fm <> "_" <> $WriteStringFileDate[fm];
     rf = RenameFile[fm, fo];
     (* check: rename file *)
     If[rf === $Failed,
        me = {Append};
        Message[WriteStringFile::switch, RenameFile, Append],
        Message[WriteStringFile::moved, fn, fo]]];
  (* method: append *)
  Check[
    fs = If[First[me] === Append, OpenAppend, OpenWrite][
      Release[fm], Sequence @@ optr];
    If[fn === "", fm = First[fs]],
    Message[WriteStringFile::failed, fm];
    Return[fs]];
  Check[
    WriteString[fs, Sequence @@ ys, "\n"],
    Message[WriteStringFile::failed, fm];
    Return[$Failed]];
  Check[
    Close[fs],
    Message[WriteStringFile::failed, fm];
    $Failed]];

WriteStringFile[___] :=
  (Message[WriteStringFile::usage];
   Abort[]);






(* TODO:
- check options
*)







(* ------------------------------------------------------------------ *)

End[];

EndPackage[];

(* ------------------------------------------------------------------ *)
