(* -- variables in context -- *)
If[False,

   nos = Names["Global`*"];
   << ../../inc/syst.m;
   nns = Names["Global`*"];
   Print[Complement[nns, nos]];
   Exit[];

];

(* -- loading -- *)
(*<< "../../aux/init.m";*)
(*<< "../../topoid.m";*)
<< "../../inc/syst.m";
(*<< "../../inc/text.m";*)

(* -- ExitCode -- *)
If[False,

   test[x_] :=
       Module[{},
              If[x[[0]] === Integer,
              Return[EvenQ[x]]];
              Return[ExitCode[1]]
          ];

   Print[test[4]];
   Print[test[3]];
   Print[test[1.5]];

];

(* -- VerbosityRules -- *)
If[False,

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

];

(* -- DefaultList -- *)
If[False,

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

];

(* -- CheckList -- *)
If[False,

   (* -- 1 -- *)
   (*
   Checks[test] = arg -> _Integer;

   CheckList[test, arg -> 1];
   CheckList[test, arg -> "1"];
   CheckList[test, {arg -> 1}];
   CheckList[test, {arg -> "1"}];
   *)
   (* -- 2 -- *)
   (*
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
   (* -- 3 -- *)

   Checks[test] = {arg1 -> _Integer,
                   arg2 -> _String,
                   arg3 -> {_Integer, _String}};

   CheckMessages[test] =
   {arg2 -> "a string is expected",
    arg3 -> "a pair of an integer and a string is required"};

   testargs = {arg1 -> 1,
               arg2 -> x^2,
               arg3 -> {},
               arg4 -> 2};

   CheckList[test, testargs];

];

(* -- DateText -- *)
If[False,

   Print[DateText[]];
   Print[DateText[Depth -> 3]];
   Print[DateText[Depth -> 5]];
   Print[DateText[Depth -> -1]];
   Print[DateText[Depth -> 100]];
   Print[DateText[Depth -> Infinity]];
   Print[DateText[Depth -> All]];
   Print[DateText[Depth -> "All"]];

];

(* -- Status -- *)
If[False,

   Status[FrontEnd, "This is i: ", i];
   (*Status[FrontEnd, "This is i: ", i, Temporary -> True];*)
   (*Status[FrontEnd, "This is i: ", i, Temporary -> False];*)

   Do[
       Status[Console, "This is i: ", i];
       (*Status[Console, "This is i: ", i, Temporary -> True];*)
       (*Status[Console, "This is i: ", i, Temporary -> False];*)
       Pause[0.05]
       , {i, 1, 100}];

];

(* -- Progress -- *)
If[False,

   Progress[FrontEnd, i, 100];
   (*Progress[FrontEnd, i];*)

   Do[
       Progress[Console, i, 100];
       (*Progress[Console, i];*)
       Pause[0.05]
       , {i, 1, 100}];

];

(* -- Report -- *)
If[True,

   Report[];

];






(* -- leaving -- *)
Exit[];












(* ListView *)
test = {a -> 0, b -> 1, c -> {c0 -> 1, c1 -> f[1, 2, 3, 4], c2 -> {0, 1, 2, 3, 4}}, d -> 3, {A, B, C, D}};
testview = ListView[test, Depth -> Infinity]; (* Depth -> 2, PageWidth -> 15, Offset -> 4 *)
Print["-- 1 --\n", testview];


(* ListForm *)
Print["-- 2 --\n", ListView[{a, b, {c, d}, {e, {f, g}}, h, {{{A, B, C}, {D, E, F}}, {1, 2}}}]];
Print["-- 3 --\n", ListForm[{a, b, {c, d}, {e, {f, g}}, h, {{{A, B, C}, {D, E, F}}, {1, 2}}}]];
Print["-- 4 --\n", ListForm[test]];
