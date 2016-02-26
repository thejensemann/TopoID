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

",

  "PACKAGE:\n",
  "  TopoID -- [Topo]logy [ID]entification\n",
  "VERSION:\n",
  "  " <> $TopoIDVersion <> " (" <> $TopoIDDate <> ")\n",
  "AUTHORS:\n",
  "  Jens Hoff & Alexey Pak\n",
  "MAILTO:\n",
  "  [jens.hoff@kit.edu]\n",
  "DESCRIPTION:\n",

  "\
- Starting from Feynman diagrams, the underlying generic topologies are
  identified and their set is minimized.  These are decomposed into
  (linearly independent) Laporta topologies.
- FORM code can be generated in order to map diagrams onto them and to
  process both types of topologies.
- Afterwards emerging sets of master integrals can be minimized. This
  includes base changes.
- Usage information is available through ?TopoID, for instance.
  Provided functions and used symbols are listed in the variables
    $TopoIDFunctions and $TopoIDSymbols.
- The debugging mode can be enabled with
    $TopoIDDebug = True;
  before loading the package.\n\n"];

Begin["`Private`"];

(* ------------------------------------------------------------------ *)

End[];

EndPackage[];

SetOptions[ParallelMap, DistributedContexts -> Automatic];

WriteString["stdout", TopoID::usage];

(* ------------------------------------------------------------------ *)
