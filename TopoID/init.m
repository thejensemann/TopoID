(* -- "init.m": Initialization for TopoID --------------------------- *)

Unprotect["TopoID`*"];
Unprotect["TopoID`*`*"];
Unprotect["TopoID`*`*`*"];
ClearAll["TopoID`*"];
ClearAll["TopoID`*`*"];
ClearAll["TopoID`*`*`"];

Unprotect[$Packages];
$Packages = DeleteCases[$Packages, _?(StringMatchQ[#, RegularExpression["TopoID`.*"]] & )];
Protect[$Packages];

BeginPackage[
  "TopoID`",
  {

     (* -- convenience, utilities -- *)
     "TopoID`Text`",
     "TopoID`System`",
     "TopoID`Common`",

     (* -- data types patterns, atomic operations -- *)
     "TopoID`Setup`",
     "TopoID`Topology`",
     "TopoID`Mapping`",

     (* -- read objects from files -- *)
     "TopoID`Load`",

     (* -- main processing of objects -- *)
     "TopoID`Polynomial`",
     "TopoID`Cuts`",
     "TopoID`Object`",
     "TopoID`Core`",
     "TopoID`Super`",

     (* -- object code generation -- *)
     "TopoID`FORM`",
     "TopoID`Code`",

     (* -- configuration for external tools -- *)
     "TopoID`EXP`",
     "TopoID`Crusher`",

     (* -- calculational tools -- *)
     "TopoID`Calculate`",

     (* -- graphical output, conversion -- *)
     "TopoID`Graph`",
     "TopoID`FeynMF`"

     }];

TopoID::usage = StringJoin[
  "
              _______                      _   ____
 --->---+--- |__   __| ------------------ | | |  _ \\ ---+--->---
         \\      | |   __    ____     __   | | | | \\ \\   |
          +     | |  /  \\  |  _ \\   /  \\  | | | |  \\ \\  |
          |\\    | | / /\\ \\ | | \\ \\ / /\\ \\ | | | |  / /  +--->---
          | \\   | | \\ \\/ / | |_/ / \\ \\/ / | | | |_/ /  /
          |  \\  |_|  \\__/  |  __/   \\__/  |_| |____/  +
          |   \\            | |                       / \\
 --->-----+----+---------- |_| ---------------------+---+--->---
       by J.Hoff & A.Pak         ",
  "v" <> $TopoIDVersion <> " (" <> $TopoIDDate <> ")\n\n"];

Begin["`Private`"];

(* ------------------------------------------------------------------ *)

End[];

EndPackage[];

SetOptions[ParallelMap, DistributedContexts -> Automatic];

WriteString["stdout", TopoID::usage];

(* ------------------------------------------------------------------ *)
