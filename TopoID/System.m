(* -- "syst.m": Mathematica System Input and Output Operations  ----- *)

BeginPackage[
  "TopoID`System`",
  {"TopoID`Text`"}];

ClearAll[Defaults, DefaultList];
ClearAll[Checks, CheckMessages, CheckList];
ClearAll[DateText];
ClearAll[Status, Console, FrontEnd];
ClearAll[Progress, Console, FrontEnd];
ClearAll[Report];

Defaults::usage = "";
DefaultList::usage = "";
Checks::usage = "";
CheckMessages::usage = "";
CheckList::usage = "";
DateText::usage = "";
Status::usage = "";
Console::usage = "";
FrontEnd::usage = "";
Progress::usage = "";
Console::usage = "";
FrontEnd::usage = "";
Report::usage = "";




















Begin["`Private`"];

(* --- DefaultList -------------------------------------------------- *)

(* DefaultList[<tag>, <arg>]
arguments:
  <tag> -- tagging expression referring to the defaults to be used and
  <arg> -- settings to be complemented by these defaults.
options:
  [ExitCode] -- whether to return ExitCode[2] or <arg> in case no
    Defaults[<tag>] is set.
output:
  Message in case no defaults are set for the corresponding <tag>.
return:
  With defaults of <tag> replenished settings from <arg>.  ExitCode[2]
  or <arg>, depending on the setting of [ExitCode], is returned in case
  no defaults were set beforehand.
attributes:
  HoldFirst to assure <tag> remains unevaluated and HoldAll for the
  arguments of the dummy function Defaults.
dependencies:
  ExitCode.
symbols:
  Defaults.
tags:
  notset.
version:
  2013-03-27 (usage of Message[...]),
  2013-06-13 (option [ExitCode]).
description:
  Replenishes the settings in <arg> (list of assignment rules) with
  default settings defined by means of the container function Defaults
  and the tagging expression <tag> in Defaults[<tag>] (cf. samples).
notes:
- The container function Defaults with attribute HoldAll is used in
  order to store default settings.
- Works also for single key-value combinations in <arg> or
  Defaults[<tag>].
- Settings without defaults are transferred to the output.
todo:
- Thread over lists in argument(s) of Defaults[...] or Defaults[{...}]?
*)

Options[DefaultList] =
{ExitCode -> True};  (* True, False/... *)

Attributes[Defaults] =
{HoldAll};
Attributes[DefaultList] =
{HoldFirst};

DefaultList::notset = "\
No Defaults[`1`] set!";

DefaultList[tag_, arg_] :=
    Module[{ec, def, res},

           ec = OptionValue[ExitCode] // TrueQ;

           def = Defaults[tag];

           (* if no defaults are set *)
           If[def[[0]] === Defaults,
              Message[DefaultList::notset, tag];
              If[ec,
                 Return[ExitCode[2]],
                 Return[arg]]];

           (* single default value *)
           def = Flatten[{def}];

           (* select from defaults keys and values missing in <arg> *)
           res = Select[def, FreeQ[arg, #[[1]] -> _] & ];

           (* append defaults and merge *)
           res = Union[Flatten[{arg, res}]];

           Return[res]
       ];

(* sample input/calls:
Defaults[test] = a -> 1;

Print[DefaultList[test, a -> 2]];
Print[DefaultList[test, b -> 2]];
Print[DefaultList[test, {a -> 2}]];
Print[DefaultList[test, {b -> 2}]];

Defaults[test] = {a -> 1, b -> 2};

Print[DefaultList[test, a -> 2]];
Print[DefaultList[test, b -> 3]];
Print[DefaultList[test, {a -> 2}]];
Print[DefaultList[test, {a -> 2, c -> 3}]];
*)

(* sample output:
{a -> 2}
{a -> 1, b -> 2}
{a -> 2}
{a -> 1, b -> 2}

{a -> 2, b -> 2}
{a -> 1, b -> 3}
{a -> 2, b -> 2}
{a -> 2, b -> 2, c -> 3}
*)

(* --- CheckList ---------------------------------------------------- *)

(* CheckList[<tag>, <arg>]
arguments:
  <tag> -- expression acting as a tag and
  <arg> -- settings to be checked against the pattern referred to by
    <tag>.
output:
  Messages in case of a missing Checks[<tag>] object, faulty settings in
  <arg>, or settings which do not appear in Checks[<tag>] .
return:
  ExitCode[0] -- on success,
  ExitCode[1] -- with at least one faulty setting in <arg>, and
  ExitCode[2] -- if Checks[<tag>] is missing.
attributes:
  HoldFirst to make sure <tag> is not evaluated and HoldAll for the
  arguments of the storage functions Checks and CheckMessages.
dependencies:
  CheckList, ExitCode.
symbols:
  Checks, CheckMessages.
tags:
  notset, invalid, unknown.
version:
  2013-03-28 (usage of Message[...]),
  2013-06-13 (mainly commentaries).
description:
  Check the list of settings <arg> (symbol assignment rules) against
  pattern definitions indicated by <tag> and stored in Checks[<tag>].
  If a test fails a message is generated and supplemented optionally by
  text from CheckMessages[<tag>].
notes:
- Two wrapper/container functions are used for holding information:
    Checks -- pattern definitions and
    CheckMessages -- optional error messages.
- Checks[<tag>] must be set, but there does not have to be an entry for
  every setting appearing in <arg>.
- CheckMessages[<tag>] is purely optional.
- The form of Checks and CheckMessages is checked itself by CheckList.
- There is no other option than to use ToString[..., InputForm].
*)

Attributes[Checks] =
{HoldAll};
Attributes[CheckMessages] =
{HoldAll};
Attributes[CheckList] =
{HoldFirst};

CheckList::notset = "\
No Checks[`1`] set!";

CheckList::invalid = "\
Invalid assignment(s) for `1`:`2`";

CheckList::unknown = "\
Unknown setting(s) for `1`.";

Checks[Checks] =
{Checks -> {___Rule}};
CheckMessages[Checks] =
{Checks -> "expecting a list of rules assigning a setting a pattern"};

Checks[CheckMessages] =
{CheckMessages -> {(_ -> _String) ...}};
CheckMessages[CheckMessages] =
{CheckMessages -> "expecting a list of rules assigning a setting a string"};

CheckList[tag_, arg : _Rule | {___Rule}] :=
    Module[{fa,fb,fc,e, pat,msg,def, t, i,u, exc, inv,unk, res},

           (* format settings list *)
           fa = StringJoin[Riffle[
               ("[" <> ToString[#[[1]], InputForm] <> "]") & /@ #,
               ", "]] &;

           (* format message text *)
           fb = (#[[1]] -> If[#[[2]] === "", "", " (" <> #[[2]] <> ")"]) &;

           (* format assignments list *)
           fc = ("  " <> StringJoin[Riffle[
               Function[e, ToString[e, InputForm] <> (e[[1]] /. #2)] /@ #1,
               ",\n  "]] <> ".") &;

           (* -- handle Checks[<tag>] -- *)

           pat = Checks[tag];

           (* if no checks set *)
           If[pat[[0]] === Checks,
              Message[CheckList::notset, tag];
              (*Return[ExitCode[0]]*)
              Return[ExitCode[2]]];

           (* if single pattern rule *)
           pat = Flatten[{pat}];

           (* if pattern rules not in right form *)
           If[FreeQ[{Checks, CheckMessages}, tag],       (* if not in 1st recursion *)
              res = CheckList[Checks, {Checks -> pat}];  (* check form of Check[<tag>] *)
              If[res === ExitCode[1],                    (* translate return value *)
                 Return[ExitCode[2]]]];

           (* -- handle CheckMessage[<tag>] -- *)

           msg = CheckMessages[tag];

           (* if no messages set *)
           If[msg[[0]] === CheckMessages,
              msg = {}];

           (* if single message *)
           msg = Flatten[{msg}];

           (* if messages not in right form *)
           If[FreeQ[{CheckMessages, Checks}, tag],                     (* if not in 1st recursion *)
              res = CheckList[CheckMessages, {CheckMessages -> msg}];  (* check form of CheckMessages[<tag>] *)
              If[res === ExitCode[1],                                  (* translate return value *)
                 msg = {}]];

           (* if messages missing *)
           def = Select[pat, FreeQ[msg, #[[1]] -> _] & ];  (* find settings w/o messages *)
           def = (#[[1]] -> "") & /@ def;                  (* assign them empty values *)
           msg = Union[Flatten[{msg, def}]];               (* merge with given messages *)

           (* -- apply Checks[<tag>] -- *)

           (* check values *)
           exc = Reap[(t = #[[1]] /. pat;               (* select by key from <arg> the pattern in <pat> *)
                       If[t === #[[1]],                 (* if no pattern is provided *)
                          Sow[#, u],                    (* sow/throw unknown setting *)
                          If[! MatchQ[#[[2]], t],       (* compare value from <arg> to pattern *)
                             Sow[#, i]]];               (* sow/throw invalid setting *)
                       ) & /@ Flatten[{arg}], {i, u}];  (* reap/catch exceptions from mapping over <arg> *)

           (* extract invalid and unknown *)
           {inv, unk} = Flatten /@ exc[[2]];

           (* pattern(s) not matching *)
           If[inv =!= {},
              Message[CheckList::invalid, fa[inv], fc[inv, fb /@ msg]]];

           (* pattern(s) not provided *)
           If[unk =!= {},
              Message[CheckList::unknown, fa[unk]]];

           (* exit status *)
           res = If[inv =!= {}, 1, 0];
           res = ExitCode[res];

           Return[res]
       ];

(* sample input/calls:
Checks[test] = {arg1 -> _Integer,
                arg2 -> _String,
                arg3 -> {_Integer, _String}};

CheckMessages[test] =
    arg3 -> "a pair of an integer and a string is required";

testargs = {arg1 -> 1,
            arg2 -> "a",
            arg3 -> {},
            arg4 -> 2};

CheckList[test, arg2 -> 1];
CheckList[test, testargs];
*)

(* sample output:
CheckList::invalid: Invalid assignment(s) for [arg2]:  arg2 -> 1.

CheckList::invalid: Invalid assignment(s) for [arg3]:
      arg3 -> {} (a pair of an integer and a string is required).

CheckList::unknown: Unknown setting(s) for [arg4].
*)

(* --- DateText ----------------------------------------------------- *)

(* DateText[[opts]]
options:
  [Depth] -- Integer between 1 and 6 for the depth or level for the date
    to be given.
return:
  Gives a string, formatted like "yyyy-mm-dd hh:mm:ss", for the current
  date and time.
version:
  2013-06-14 (renamed and option added).
notes:
- If the value of [Depth] does not match the specifications the maximum
  6 is taken.
*)

Options[DateText] = {Depth -> 6};

DateText[opts : OptionsPattern[]] :=
    Module[{dp, dt},

           (* handle option *)
           dp = OptionValue[Depth];
           If[! IntegerQ[dp] || dp < 1 || dp > 6, dp = 6];

           (* list of rounded date components *)
           dt = ToString /@ Round /@ DateList[];

           (* fix number of digits to mininmally 2 *)
           dt = If[StringLength[#] >= 2, #, "0" <> #] & /@ dt;

           (* apply formatting style to each component*)
           dt = MapThread[(#1 <> #2) &, {{"", "-", "-", " ", ":", ":"}, dt}];

           (* join up to certain depth *)
           dt = StringJoin[dt[[;; dp]]];

           Return[dt]
       ];

(* sample calls:
Print[DateText[]];
Print[DateText[Depth -> 3]];
Print[DateText[Depth -> 5]];
Print[DateText[Depth -> -1]];
Print[DateText[Depth -> 100]];
Print[DateText[Depth -> Infinity]];
Print[DateText[Depth -> All]];
Print[DateText[Depth -> "All"]];
*)

(* sample output:
2013-06-14 10:35:14
2013-06-14
2013-06-14 10:35
2013-06-14 10:35:14
2013-06-14 10:35:14
2013-06-14 10:35:14
2013-06-14 10:35:14
2013-06-14 10:35:14
*)

(* --- Status ------------------------------------------------------- *)

(* Status[<stp>, <msg>, [opts]]
arguments:
  <stp> -- symbolic flag, either >Console< or >Frontend<, indicating the
    session type and
  <msg> -- sequence of expressions containing the information for the
    status message.
options:
  [Automatic] -- switch permitting deactivation for the detection of the
    session type (e.g. errors can also be written to a stream from
    within the frontend),
  [Streams] -- list containing handlers for the desired output streams
    (e.g. "stderr", "stdout", or self-defined ones), and
  [Temporary] -- flag for temporary/persistent message printing.
output:
  <msg> in form of a status message and an error message if the <stp>
  has no correct value.
return:
  Usually ExitCode[0], ExitCode[1] if the session type <stp> was not
  defined properly.
attributes:
  HoldAll in order to enable dynamic output to the frontend.
symbols:
  Console, FrontEnd.
tags:
  session.
version:
  2013-06-14 (almost complete rewrite).
description:
  Dynamically updated printing of status messages to both, the frontend
  or the console/terminal.
notes:
- $BatchInput is False in interactive kernel sessions and should not be
  used as a condition.
- A switch for Dynamic actually does not make any sense at all.
- One has to be careful concerning the form of the given messages in
  <msg> when Status is called.  It is to the user to provide for the
  right formatting, the function operates only minimally on <msg>.
todo:
- InputForm -> Function!
- clear with " " until EOL
*)

Options[Status] =
{Automatic -> True,      (* True, False/... *)
 Streams -> {"stdout"},  (* "stdout", "stderr", <stream_handler> *)
 Temporary -> True};     (* True, False/... *)

Attributes[Status] = {HoldAll};

Status::session = "\
Unknown session type selector
  `1`,
use either Console or FrontEnd.";

$StatusClearLine = 0;

Status[stp_, msg___, opts : OptionsPattern[]] :=
    Module[{f,g, at,st,tp, tmp, ltmp, lclr},

           (* deactivate in Emacs *)
           If[Environment["MATHINDENTCOOKIE"] =!= $Failed,
              Return[ExitCode[0]]];

           f = If[#[[0]] === String, #, InputForm[#]] & ;
           g = If[#[[0]] === String, #, ToString[#]] & ;  (* TODO *)

           {at, st, tp} = OptionValue[{Automatic, Streams, Temporary}];
           {at, tp} = TrueQ /@ {at, tp};

           tmp = f /@ {msg};

           (* begin TODO *)
           ltmp = StringLength[StringJoin[g /@ tmp]];
           lclr = Max[$StatusClearLine - ltmp, 0];
           $StatusClearLine = ltmp;
           clr = StringJoin[ConstantArray[" ", lclr]];
           (* end TODO *)

           AppendTo[tmp, clr];
           AppendTo[tmp, If[tp && at, "\r", "\n"]];

           Switch[stp,
                  (* -- All -- *)
                  All,
                  If[$FrontEnd === Null || ! at,
                     WriteString[st, Sequence @@ tmp]];

                  If[$FrontEnd =!= Null,
                     If[tp, PrintTemporary, Print][Dynamic[Row[{msg}]]]]
                  ,
                  (* -- Console -- *)
                  Console,
                  If[$FrontEnd === Null || ! at,
                     WriteString[st, Sequence @@ tmp]]
                  ,
                  (* -- FrontEnd -- *)
                  FrontEnd,
                  If[$FrontEnd =!= Null,
                     If[tp, PrintTemporary, Print][Dynamic[Row[{msg}]]]]
                  ,
                  (* -- wrong switch -- *)
                  _,
                  Message[Status::session, stp];
                  Return[ExitCode[1]]];

           Return[ExitCode[0]]
       ];

(* sample usage/calls:
Status[FrontEnd, "This is i: ", i];     # use outside loops

Do[
    Status[Console, "This is i: ", i];  # use inside loops
    Pause[0.05]
    , {i, 1, 100}];
*)

(* sample output:
This is i: 1
This is i: 2
This is i: 3
# and so on...
This is i: 100
*)

(* --- Progress ----------------------------------------------------- *)

(* Progress[<stp>, <var>, [<max>], [opts]]

arguments:
  <stp> -- symbolic flag, either >Console< or >Frontend<, indicating the
    session type and
  <var> -- number (>Console<), resp. symbolic variable (>FrontEnd<).
(optional):
  <max> -- maximum number value of <var> to be assumed (the progress bar
    oscillates if this is missing).
options:
  [Automatic] -- switch permitting deactivation for the detection of the
    session type (e.g. errors can also be written to a stream from
    within the frontend),
  [Dimensions] -- pair of integers giving the length of the progress bar
    in text mode and the oscillation frequency in case no <max> is
    present,
  [Display] -- switch for either >Percentage< or >Ratio< display
    (anything else acts deactivating),
  [FormatType] -- list of strings formatting the progress bar in text
    mode (cf. format tag),
  [Streams] -- list containing handlers for the desired output streams
    (e.g. "stderr", "stdout", or self-defined ones),
  [Temporary] -- flag for temporary/persistent message printing, and
  Options[ProgressIndicator] (or some of them).
output:
  A progress bar showing the change of <var> and error messages in case
  <var> has no numeric value or <stp> is not used properly.
return:
  Usually ExitCode[0], ExitCode[1] if the session type <stp> was not
  defined properly or <var> has no numeric value.
attributes:
  HoldAll in order to enable dynamic output to the frontend.
symbols:
  Console, FrontEnd.
tags:
  dimensions, format, session, nonumber.
version:
  2013-06-15 (complete rewrite).
description:
  Dynamically updated printing of a progress bar to both, the frontend
  or the console/terminal.
notes:
- Some remarks from Status also apply here.
- DefaultList and CheckList are not used for not creating too many
  dependencies here.
todo:
- Rename Dimensions!
*)

Options[Progress] =
{Automatic -> True,                        (* True, False/... *)
 Dimensions -> {72, 10},                   (* {_Integer, _Integer} *)
 Display -> Percentage,                    (* Percentage, Ratio, None, ... *)
 FormatType -> {"[", "=", "+", "-", "]"},  (* {___String} *)
 Streams -> {"stdout"},                    (* "stdout", "stderr", <stream_handler> *)
 Temporary -> True,                        (* True, False/... *)

 (* -- ProgressIndicator -- *)
 Appearance -> Automatic,
 BaselinePosition -> Automatic,
 BaseStyle -> {},
 ImageMargins -> 0,
 ImageSize -> {500, 10}};

Attributes[Progress] = {HoldAll};

Progress::dimensions = "\
Erroneous settings for [Dimensions] were given by
   Dimensions -> `1`.
A pair of integers is expected, e.g. the default {100, 5} now used.";

Progress::format = "\
Erroneous settings for [FormatType] were given by
  FormatType -> `1`.
The function expects a list of strings, meaning
  FormatType -> {<begin>, <done>, <here>, <todo>, <end>}.
For now the default is used
  FormatType -> {\"[\", \"=\", \"+\", \"-\", \"]\"}.";

Progress::session = "\
Unknown session type selector
  `1`,
use either Console or FrontEnd.";

Progress::nonumber = "\
The passed variable value `1` is not a number.";

Progress[stp_, var_, max_ : Indeterminate, opts : OptionsPattern[]] :=
    Module[{mx, at,ds,dp,ft,st,tp, os,dd,fd,fl, l,p,r,f, tmp},

           mx = If[! NumberQ[max] || max <= 0, Indeterminate, max];

           {at, ds, dp, ft, st, tp} =
               OptionValue[{Automatic, Dimensions, Display,
                            FormatType, Streams, Temporary}];

           (* inherited *)
           os = FilterRules[{opts}, Options[ProgressIndicator]];

           (* defaults *)
           {dd, fd} = {Dimensions, FormatType} /. Options[Progress];

           {at, tp} = TrueQ /@ {at, tp};

           If[! MatchQ[ds, {_Integer, _Integer}],
              Message[Progress::dimensions, ds];
              ds = dd];

           If[! MatchQ[ft, {___String}],
              Message[Progress::format, ft];
              ft = fd];

           (* complete *)
           If[Length[ft] < 5,
              ft = Join[ft, fd[[Length[ft] + 1 ;;]]]];

           (* fixed lengths *)
           fl = StringLength[StringJoin[ft[[{1, 3, 5}]]]];

           (* display forms *)
           l = Table[" ", {#2 - StringLength[#1]}] <> #1 &;
           p = l[ToString[Round[#1/#2*100]], 3] <> "%" &;
           r = l[ToString[#1], IntegerLength[#2]] <> "/" <> ToString[#2] &;

           (* console progress bar *)
           f = StringJoin[{ft[[1]],
                           Table[ft[[2]], {Ceiling[#1*(#2 - fl)]}],
                           ft[[3]],
                           Table[ft[[4]], {Floor[(1 - #1)*(#2 - fl)]}],
                           ft[[5]]}] &;

           Switch[stp,
                  (* -- Console -- *)
                  Console,
                  If[$FrontEnd === Null || ! at,
                     If[! NumberQ[var],
                        Message[Progress::nonumber, var];
                        Return[ExitCode[1]]];
                     tmp = If[mx =!= Indeterminate,
                              f[var/mx, ds[[1]]]
                              ,
                              ft[[2]] = ft[[4]];
                              f[Abs[Mod[var, 2*ds[[2]]]/ds[[2]] - 1], ds[[1]]]];
                     tmp = tmp <>
                         Switch[dp,
                                (* -- Percentage -- *)
                                Percentage,
                                " " <> If[mx =!= Indeterminate, p[var, mx], ToString[var]],
                                (* -- Ratio -- *)
                                Ratio,
                                " " <> If[mx =!= Indeterminate, r[var, mx], ToString[var]],
                                (* -- None -- *)
                                _,
                                ""];
                     tmp = tmp <> If[tp && at, "\r", "\n"];
                     WriteString[st, tmp]]
                  ,
                  (* -- FrontEnd -- *)
                  FrontEnd,
                  If[$FrontEnd =!= Null,
                     If[tp, PrintTemporary, Print]
                     [Row[{ProgressIndicator[Dynamic[var], If[mx =!= Indeterminate, {0, mx}, mx], os],
                           Switch[dp,
                                  (* -- Percentage -- *)
                                  Percentage,
                                  Dynamic[" " <> If[mx =!= Indeterminate, p[var, mx], ToString[var]]],
                                  (* -- Ratio -- *)
                                  Ratio,
                                  Dynamic[" " <> If[mx =!= Indeterminate, r[var, mx], ToString[var]]],
                                  (* -- None -- *)
                                  _,
                                  ""]}]]]
                  ,
                  (* -- wrong switch -- *)
                  _,
                  Message[Progress::session, stp];
                  Return[ExitCode[1]]];

           Return[ExitCode[0]];
       ];

(* sample usage/calls:
Progress[FrontEnd, i, 100];     # use outside loops

Do[
    Progress[Console, i, 100];  # use inside loops
    Pause[0.05]
    , {i, 1, 100}];
*)

(* sample output:
[+--------------------------------------------------]   0%
[=====+---------------------------------------------]  10%
[==========+----------------------------------------]  20%
# and so on...
[==================================================+] 100%
*)

(* --- Report [TODO] ------------------------------------------------ *)

(* Report[<vb>, <vt>, <msg>]
arguments:
  <vb> -- integer or Infinity giving the verbosity level of the
    enclosing function,
  <vt> -- integer stating the verbosity level at which the message <msg>
    should be printed, and
  <msg> -- sequence of objects to be printes as message (string,
    expressions, ...).
output:
  If <vb> >= <vt> the message <msg> is printed preceded by <vt>
  asterisks ("*") indicating the verbosity level <vt>.
version:
  2013-03-29
notes:
- Modify Print[...] in order to (further) modify the behavior.
- To be used in conjunction with VerbosityRules.
*)

Report[vb : _Integer | Infinity, vt_Integer, msg___] :=
    Module[{f, hdr},
           f = If[#[[0]] === String, #, InputForm[#]] &;
           If[vb >= vt,
              hdr = StringJoin[Table["*", {vt}]] <> " ";
              (*Print[hdr, msg]]*)
              WriteString["stderr", hdr, Sequence @@ (f /@ ({msg} /. TableForm[x_List] :> Sequence @@ Riffle[x, "\n"])), "\n"]]
       ];

(* sample calls:
Report[1, 2, "not printed"];
Report[3, 2, "expression: ", x^2];
*)

(* sample output:
                2
** expression: x
*)

(* --- *)

(* provided functions:

- ExitCode
- VerbosityRules
- DefaultList
- CheckList
- DateText
- Status
- Progress

- Report

*)


















































(* TODO:

- sort

- put into own context?

- SwitchContext?
- here: TopoIDGet ("topoid.m") -> ???

*)

(*
ExitCode
VerbosityRules, Verbosity
Report
Defaults, DefaultList::notset, DefaultList
Checks, CheckMessages, CheckList::notset,CheckList::invalid,CheckList::unknown, CheckList
*)





















































(* ---  ------------------------------------------------------------------------------------------- *)

(* provided functions:

  JForm (prev. OutputFunc)
  JSave (prev. SaveFunc)
  ListForm (prev. ListView)

*)

(* ------------------------------------------------------------------ *)

(* OutputFunc[<expr>, [opts]]
arg.s:
  <expr> -- sequence of expressions to be converted to a string.
opt.s:
  [FormatType], [PageWidth], [TotalWidth] -- inherited from ToString.
  [Pretty] -- flag for deleting unnecessary blank lines and
  [Separator] -- string to be added inbetween the expressions in <expr>
return/desc.:
  String build up from the converted expressions.  Therefor only
  non-strings from <expr> are transformed, strings are left as they are.
bugs:
  If <expr> ends on an empty set {}, this isn't included.  A set on the
  end causes an error.  In this case just append an empty string "".
*)

ClearAll[OutputFunc];

Options[OutputFunc] = {FormatType -> InputForm, (* InputForm, OutputForm, ... *)
                       PageWidth -> Infinity,   (* Infinity, _Integer *)
                       Pretty -> True,          (* True/Automatic, False *)
                       Separator -> "",         (* _String *)
                       TotalWidth -> Infinity}; (* Infinity, _Integer *)

OutputFunc[expr___, opts : OptionsPattern[]] :=
    Module[{os, f, p,s, res},
           os = FilterRules[{opts}, Options[ToString]];
           (* only convert non-strings to strings *)
           f = If[#[[0]] === String, #, ToString[InputForm[#], os]] &;
           {p, s} = OptionValue[{Pretty, Separator}];
           p = (p === True || p === Automatic);
           res = StringJoin[Riffle[f /@ {expr}, s]];
           (*If[p, res = StringReplace[res, "\n\n" -> ""]];*)
           If[p, res = StringReplace[res, RegularExpression["(?m)(?s)\\s*$"] -> ""]];
           Return[res]
        ];

(* sample call:
OutputFunc["test", Expand[(1 - x)^5], {a, b, c}, Separator -> ", "]
*)

(* sample output:
"test, 1 - 5*x + 10*x^2 - 10*x^3 + 5*x^4 - x^5, {a, b, c}"
*)




(* ------------------------------------------------------------------ *)

(* SaveFunc[<fn>, [opts]]
arg.s:
  <fn> -- string containing the filename and
  <expr> -- sequence of expressions.
opt.s:
  [Footer], [Header] and [Separators] --
    switching between automatic, manual and no formatting at all by
    assigning the values True/Automatic, a string or something else
    (e.g. False) respectively.
  [Path] -- string for the path to the used directory and
  Options[ToString].
att.s:
  HoldRest to prevent evaluation of <expr>.
output:
  In case of an error during conversion to definitions or writing to
  file a message is generated via ErrorFunc.
return:
  ExitCode[0] -- on success,
  ExitCode[1] -- for an error while writing to file and
  ExitCode[2] -- on failure due to conversion to definitions.
desc.:
  An output file is written containing the definitions for the given
  expressions in <expr>.  Concerning the formatting, especially header,
  separators for expressions, footer and the line width are adjustable.
notes:
  In contrast to the standard Save, an existing file is overwritten.
    A string expression is copied to the file untouched; in this way
  arbitrary executable code (with escapes between the quotes) can be
  inserted.
todo:
  ";" after and "(...)" around each set-like command?
*)

ClearAll[SaveFunc];

Attributes[SaveFunc] = {HoldRest};

Options[SaveFunc] = {Footer -> True,          (* True/Automatic, String, _False *)
                     FormatType -> InputForm, (* InputForm, OutputForm, ... *)
                     Header -> True,          (* True/Automatic, String, _False *)
                     PageWidth -> 100,        (* _Integer (> 5!), Infinity *)
                     Path -> "",              (* _String *)
                     Separators -> True,      (* True/Automatic, String, _False *)
                     TotalWidth -> Infinity}; (* Infinity, _Integer *)

(* Mathematica bug?!
  Default options set by Options[SaveFunc] not explicitely stated in
  <opts>, even though OptionsPattern[SaveFunc] is used.
*)

(* workaround *)
Defaults[SaveFunc] = Options[SaveFunc];

SaveFunc[fn_String, expr___, opts : OptionsPattern[SaveFunc]] :=
    Module[{hdr,str,ftr, pw,pt, os, lin,cmt, ce,we, flg,res, tmp, fh},
           (* options *)
           {hdr, str, ftr} = OptionValue[{Header, Separators, Footer}];
           {pw, pt} = OptionValue[{PageWidth, Path}];
           (* full path *)
           pt = If[pt =!= "",
                   FileNameJoin[{pt, fn}],
                   fn];
           (* options passed *)
           os = DefaultValue[SaveFunc, {opts}];
           os = FilterRules[os, Options[ToString]];
           (* commentaries *)
           lin = ("(* " <> StringJoin[Table["-", {pw - 6}]] <> " *)");
           cmt = ("(* --- " <> # <> " " <>
                  StringJoin[Table["-", {pw - StringLength[#] - 11}]] <>
                  " *)") &;
           (* error strings *)
           ce = "Error during conversion to definitions";
           we = "Writing to \"" <> pt <> "\" not possible!";
           (* return flag and signal *)
           {flg, res} = {False, ExitCode[1]};
           (* definitions *)
           Quiet[Check[
               (* list of hold expressions *)
               tmp = Hold /@ Hold[expr];
               tmp = {ReleaseHold[tmp]};
               (* Hold[...] removed: left-hand sides *)
               tmp = ToString /@ tmp;
               tmp = StringTake[#, {6, -2}] & /@ tmp;
               (* compose definitions *)
               tmp = If[Names[#] === {},
                        #, (* leave strings as they are *)
                        FullDefinition @@ Names[#]] & /@ tmp
                            ,
               flg = True]];
           If[flg,
              ErrorFunc[SaveFunc, "convert", ce];
              Return[res]];
           (* convert non-strings to right format *)
           tmp = If[#[[0]] === String, #, ToString[#, os]] & /@ tmp;
           (* delete trailing whitespace *)
           tmp = StringReplace[#, RegularExpression["(?m)(?s)\\s*$"] -> ""] & /@ tmp;
           (* delete empty definitions *)
           tmp = Select[tmp, (# =!= "") & ];
           (* separators *)
           str = Switch[str,
                        True | Automatic, lin <> "\n\n",
                        _String, str <> "\n\n",
                        _, ""];
           tmp = StringJoin[Riffle[tmp, "\n\n" <> str]];
           (* header and footer *)
           hdr = Switch[hdr,
                        True | Automatic, cmt["File generated on " <> DateFunc] <> "\n\n",
                        _String, hdr <> "\n\n",
                        _, ""];
           ftr = Switch[ftr,
                        True | Automatic, "\n\n" <> cmt["Mathematica " <> $Version],
                        _String, "\n\n" <> ftr,
                        _, ""];
           tmp = (hdr <> tmp <> ftr <> "\n");
           (* write to file *)
           Quiet[Check[
               fh = OpenWrite[pt, os];
               WriteString[fh, tmp];
               Close[fh]
               ,
               flg = True]];
           If[flg,
              ErrorFunc[SaveFunc, "write", we];
              Return[res]];
           res = ExitCode[0];
           Return[res]
       ];


(* ------------------------------------------------------------------ *)

(* TODO: rename Depth-option? Add automatic linebreaks via options *)

ClearAll[ListView];

Options[ListView] = {Depth -> Infinity,
Indentation -> 2};

ListView[lst_List, opts : OptionsPattern[]] :=
    Module[{dpt,idn, f, res, l,i},
           {dpt, ind} = OptionValue[{Depth, Indentation}];
           f = StringJoin[Table[" ", {ind*#}]] &;
           res = ToString[lst, FormatType -> InputForm, PageWidth -> Infinity, TotalWidth -> Infinity];
           (* spaces after commas disturb at the moment *)
           res = StringReplace[res, ", " -> ","];
           res = Characters[res];
           l = 0;
           For[i = 1, i <= Length[res], i++,
               r = res[[i]];
               Switch[r,
                      "{",
                      l++;
                      If[l <= dpt,
                         r = "{" <> "\n" <> f[l]]
                      ,
                      "}",
                      l--;
                      If[l < dpt,
                         r = "\n" <> f[l] <> "}"]
                      ,
                      ",",
                      If[l <= dpt,
                         r = "," <> "\n" <> f[l]]
                  ];
               res[[i]] = r
               ];
           res = StringJoin[res];
           (* restore spaces after commas *)
           res = StringReplace[res, "," -> ", "];
           Return[res]
       ];

(* ------------------------------------------------------------------ *)


(* ------------------------------------------------------------------ *)

(* ListView[<lst>, [opts]]
arguments:
  <lst> -- List to be displayed.
options:
  [Depth] -- Maximal nesting level that is displayed structured and
  [Indentation] -- Numbers of spaces to use for indentation.
return:
  String of a structured representation (with newlines and indentation)
  of a (multiply nested) list.
version:
  2012-09-13
todo:
- Parse the whole parentheses structure (c.f. "TeXTools.m")?
- Close -> {}, Hide -> {}, Open -> {} options for certain keys or
  positions.
- Option how to handle f[...]?
- Option how to indent (see below!)?
- samples
status:
  OPEN
*)

(*

{a,
 b,
 c}

{
  a,
  b,
  c
}

*)

ClearAll[ListForm];

Options[ListForm] = {
                         Close -> {},              (* keys forced to be kept closed *)
                         Hide -> {},               (* keys hidden in the output *)
                         Open -> {},               (* keys forced to be expanded *)


                         Depth -> Infinity,        (* _Integer | Infinity *)
                         FormatType -> InputForm,  (* InputForm | OutputForm | ... *)
                         Indentation -> 2,         (* _Integer *)
                         Offset -> 2,              (* _Integer *)
                         PageWidth -> Infinity     (* _Integer | Infinity *)

                         };

(*
expr: _List | _
rec: _List | Rule | String | _
*)


ListForm[expr_, opts : OptionsPattern[]] :=
    Module[{cls,hid,opn, dpt,fmt,idt,ofs,pgw, CLS,HID,OPN, rls, tmp, rec, str, f,g,h,i,j, x},
           (* option values *)
           {cls, hid, opn} = OptionValue[{Close, Hide, Open}];
           {dpt, fmt, idt, ofs, pgw} = OptionValue[{Depth, FormatType, Indentation, Offset, PageWidth}];
           (* indentation space *)
           f = StringJoin[Table[" ", {#*idt}]] &;
           (* special treatments *)
           rls = Join[(Rule[#, x_] -> Rule[CLS[#], x]) & /@ cls,
                      (Rule[#, x_] -> Rule[HID[#], x]) & /@ hid,
                      (Rule[#, x_] -> Rule[OPN[#], x]) & /@ opn];
           (* apply rules *)
           tmp = expr //. rls;
           (* delete content of hidden keys *)
           tmp = tmp /. {Rule[HID[k_], x_] -> Rule[k, "<<hidden>>"]}; (* //. ? *)
           (* mark lists *)
           tmp = If[#[[0]] === List, II[#], #] & //@ tmp;
           (* mark heads and elements separately *)
           tmp = tmp //. {II[l_] :> IL[IE /@ l]};
           (* determine head level *)
           tmp = tmp //. {IL[x_] :> IL[0, x]};
           tmp = tmp //. {IE[IL[n_, x_]] :> IL[n + 1, IE //@ x]};
           (* determine element level *)
           tmp = tmp //. {IE[x_ /; FreeQ[x, IE]] :> IE[0, x],
                          IE[IE[n_, x_]] :> IE[n + 1, x]};
           Print["\n", tmp, "\n"];
           (* indent elements *)
           tmp = tmp /. {IE[n_, x_] :> f[n + 1] <> ToString[x]};
           (* indent heads *)
           tmp = tmp //. {IL[n_, l_ /; FreeQ[l, IL]] :>
                              (f[n] <> "{\n" <>
                               StringJoin[Riffle[l, ",\n"]] <> "\n" <>
                               f[n] <> "}")};
           Return[tmp]
       ];




(* works for plain lists *)
ListForm2[expr_, opts : OptionsPattern[]] :=
    Module[{cls,hid,opn, dpt,fmt,idt,ofs,pgw, CLS,HID,OPN, rls, tmp, rec, str, f,g,h,i,j, x},
           (* option values *)
           {cls, hid, opn} = OptionValue[{Close, Hide, Open}];
           {dpt, fmt, idt, ofs, pgw} = OptionValue[{Depth, FormatType, Indentation, Offset, PageWidth}];
           (* indentation space *)
           f = StringJoin[Table[" ", {#*idt}]] &;
           (* special treatments *)
           rls = Join[(Rule[#, x_] -> Rule[CLS[#], x]) & /@ cls,
                      (Rule[#, x_] -> Rule[HID[#], x]) & /@ hid,
                      (Rule[#, x_] -> Rule[OPN[#], x]) & /@ opn];
           (* apply rules *)
           tmp = expr //. rls;
           (* delete content of hidden keys *)
           (*tmp = tmp /. {Rule[HID[k_], x_] -> Rule[k, "<<hidden>>"]};*)
           (* mark lists *)
           tmp = If[#[[0]] === List, II[#], #] & //@ tmp;
           (* mark heads and elements separately *)
           tmp = tmp //. {II[l_] :> IL[IE /@ l]};
           (* determine head level *)
           tmp = tmp //. {IL[x_] :> IL[0, x],
                          IE[IL[n_, x_]] :> IL[n + 1, IE /@ x]};
           (* determine element level *)
           tmp = tmp //. {IE[x_ /; FreeQ[x, IE]] :> IE[0, x],
                          IE[IE[n_, x_]] :> IE[n + 1, x]};
           (* indent elements *)
           tmp = tmp /. {IE[n_, x_] :> f[n + 1] <> ToString[x]};
           (* indent heads *)
           tmp = tmp //. {IL[n_, l_ /; FreeQ[l, IL]] :>
                              (f[n] <> "{\n" <>
                               StringJoin[Riffle[l, ",\n"]] <> "\n" <>
                               f[n] <> "}")};
           Return[tmp]
       ];





ClearAll[ListView];

Options[ListView] = {Depth -> Infinity,        (* _Integer | Infinity *)
                     FormatType -> InputForm,  (* InputForm | OutputForm | ... *)
                     Indentation -> 2,         (* _Integer *)
                     Offset -> 2,              (* _Integer *)
                     PageWidth -> Infinity};   (* _Integer | Infinity *)

ListView[lst_List, opts : OptionsPattern[]] :=
    Module[{dpt,frm,idn,ofs,pgw, f, res, l,i},
           {dpt, frm, ind, ofs, pgw} =
               OptionValue[{Depth, FormatType, Indentation, Offset, PageWidth}];
           f = StringJoin[Table[" ", {ind*#}]] &;
           res = ToString[lst, FormatType -> frm,
                          PageWidth -> Infinity, TotalWidth -> Infinity];
           (* spaces after commata disturb for the moment *)
           res = StringReplace[res, ", " -> ","];
           res = Characters[res];
           l = 0;
           res = Switch[#,
                        "{", If[l++ < dpt, # <> "\n" <> f[l], #],   (* open => add indentation *)
                        "}", If[l-- <= dpt, "\n" <> f[l] <> #, #],  (* close => subtract indentation *)
                        ",", If[l <= dpt, # <> "\n" <> f[l], #],    (* next => indent *)
                        "[", dpt = 0; #,                            (* protection begin *)
                        "]", dpt = OptionValue[Depth]; #,           (* protection end *)
                        _, #] & /@ res;
           res = StringJoin[res];
           (* restore spaces after commata *)
           res = StringReplace[res, "," -> ", "];
           res = SplitLines[res, Indentation -> ofs, PageWidth -> pgw];
           Return[res]
       ];

(* sample input/call:
TODO
*)

(* sample output:
TODO
*)

(* ------------------------------------------------------------------ *)

End[];

EndPackage[];

(* ------------------------------------------------------------------ *)
