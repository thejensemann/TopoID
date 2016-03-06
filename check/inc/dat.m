<< "../../aux/init.m";
(*<< "../../topoid.m";*)
<< "../../inc/syst.m";
<< "../../inc/dat.m";

Print["
(* --- ScalarProducts -------------------------------------------- *)"];

(* without constraints *)

ScalarProducts[{k1}] // Print;
ScalarProducts[{p1}, {k1}] // Print;
ScalarProducts[{k1, k2}] // Print;
ScalarProducts[{p1, p2}, {k1}] // Print;
ScalarProducts[{p1}, {k1, k2}] // Print;

ScalarProducts[{k1, k2, k3}] // Print;
ScalarProducts[{p1, p2}, {k1, k2}] // Print;

ScalarProducts[{p1}, {k1, k2, k3}] // Print;
ScalarProducts[{p1, p2}, {k1, k2, k3}] // Print;

(* with constraints *)

ScalarProducts[{p1, p2, p3, p4}, {k1}, {p3 -> p1, p4 -> p2}] // Print;

ScalarProducts[{k1, k2, k3}, {k1^2 -> 0}] // Print;

Print["
(* --- Setup ----------------------------------------------------- *)"];

?s;
?m;

?SetupPattern;

SetupPattern[] // Print;

?SetupQ;

SetupQ[set] // Print;

?Externals;
?Internals;
?Masses;
?Constants;
?Constraints;
?Rules;

set =
{Externals -> {p1, p2, p3, p4},
 Internals -> {k1},
 Masses -> {"gh" -> 0, "gl" -> 0, "hb" -> mh,
            "qd" -> 0, "qu" -> 0, "si" -> 0},
 Constants -> {s, mh^2},
 Constraints -> {p3 -> p1, p4 -> p2,
                 p1^2 -> 0, p2^2 -> 0,
                 p1*p2 -> -s/2},
 Rules -> {mh^2 -> s*x}};

Print[set];

set = Setup[set];

Print[set];

SetupQ[set] // Print;

Setup[
  Externals -> {p1, p2, p3, p4},
  Internals -> {k1},
  Masses -> {"gh" -> 0, "gl" -> 0, "hb" -> mh,
             "qd" -> 0, "qu" -> 0, "si" -> 0},
  Constants -> {s, mh^2},
  Constraints -> {p3 -> p1, p4 -> p2,
                  p1^2 -> 0, p2^2 -> 0,
                  p1*p2 -> -s/2},
  Rules -> {mh^2 -> s*x}] // Print;

(* messages *)

Setup[
  Externals -> Null,
  Internals -> {k1},
  Masses -> {"gh" -> 0, "gl" -> 0, "hb" -> mh,
             "qd" -> 0, "qu" -> 0, "si" -> 0},
  Constants -> {s, mh^2},
  Constraints -> {p3 -> p1, p4 -> p2,
                  p1^2 -> 0, p2^2 -> 0,
                  p1*p2 -> -s/2},
  Rules -> {mh^2 -> s*x}] // Print;

Setup[
  Externals -> {p1, p2, p3, p4},
  Internals -> Null,
  Masses -> {"gh" -> 0, "gl" -> 0, "hb" -> mh,
             "qd" -> 0, "qu" -> 0, "si" -> 0},
  Constants -> {s, mh^2},
  Constraints -> {p3 -> p1, p4 -> p2,
                  p1^2 -> 0, p2^2 -> 0,
                  p1*p2 -> -s/2},
  Rules -> {mh^2 -> s*x}] // Print;

Setup[
  Externals -> {p1, p2, p3, p4},
  Internals -> {k1},
  Masses -> Null,
  Constants -> {s, mh^2},
  Constraints -> {p3 -> p1, p4 -> p2,
                  p1^2 -> 0, p2^2 -> 0,
                  p1*p2 -> -s/2},
  Rules -> {mh^2 -> s*x}] // Print;

Setup[
  Externals -> {p1, p2, p3, p4},
  Internals -> {k1},
  Masses -> {"gh" -> 0, "gl" -> 0, "hb" -> mh,
             "qd" -> 0, "qu" -> 0, "si" -> 0},
  Constants -> Null,
  Constraints -> {p3 -> p1, p4 -> p2,
                  p1^2 -> 0, p2^2 -> 0,
                  p1*p2 -> -s/2},
  Rules -> {mh^2 -> s*x}] // Print;

Setup[
  Externals -> {p1, p2, p3, p4},
  Internals -> {k1},
  Masses -> {"gh" -> 0, "gl" -> 0, "hb" -> mh,
             "qd" -> 0, "qu" -> 0, "si" -> 0},
  Constants -> {s, mh^2},
  Constraints -> Null,
  Rules -> {mh^2 -> s*x}] // Print;

Setup[
  Externals -> {p1, p2, p3, p4},
  Internals -> {k1},
  Masses -> {"gh" -> 0, "gl" -> 0, "hb" -> mh,
             "qd" -> 0, "qu" -> 0, "si" -> 0},
  Constants -> {s, mh^2},
  Constraints -> {p3 -> p1, p4 -> p2,
                  p1^2 -> 0, p2^2 -> 0,
                  p1*p2 -> -s/2},
  Rules -> Null] // Print;

Print["
(* --- Elective -------------------------------------------------- *)"];

?Elective;

Elective[pat] // Print;

Print["
(* --- Objects --------------------------------------------------- *)"];

?p;
?d;
?n;

?in;
?out;

?FactorPattern;

FactorPattern[] // Print;

?GroupPattern;

GroupPattern[] // Print;

?VertexPattern;

VertexPattern[] // Print;

?EdgePattern;

EdgePattern[] // Print;

Print["
(* --- DenominatorSymbolQ, NumeratorSymbolQ ---------------------- *)"];

?DenominatorSymbolQ;

DenominatorSymbolQ[p[1]] // Print;
DenominatorSymbolQ[d[1]] // Print;
DenominatorSymbolQ[p1] // Print;
DenominatorSymbolQ[d1] // Print;

DenominatorSymbolQ[n[1]] // Print;
DenominatorSymbolQ[x] // Print;

?NumeratorSymbolQ;

NumeratorSymbolQ[n[1]] // Print;
NumeratorSymbolQ[s[1]] // Print;
NumeratorSymbolQ[n1] // Print;
NumeratorSymbolQ[s1] // Print;

NumeratorSymbolQ[p[1]] // Print;
NumeratorSymbolQ[x] // Print;

Print["
(* --- Topology -------------------------------------------------- *)"];

?TopologyPattern[];

TopologyPattern[] // Print;
TopologyPattern["Solve"] // Print;
TopologyPattern["Solve", "Alpha"] // Print;
TopologyPattern[All] // Print;
TopologyPattern[Full] // Print;
TopologyPattern["Alpha", Only] // Print;

TopologyPattern["Dog"] // Print;

?TopologyQ;

TopologyQ[top] // Print;

?Topology;

top = Topology[];

Print[top];

(* initialize *)
TopologyQ[top] // Print;

(* hidden case: strings *)

TopoID`Topology`Private`$Topology[] // Print;
TopoID`Topology`Private`$Topology["Solve"] // Print;
TopoID`Topology`Private`$Topology["Alpha"] // Print;
TopoID`Topology`Private`$Topology["Solve", "Alpha"] // Print;

TopoID`Topology`Private`$Topology["Wuff"] // Print;
$Topology["Solve", "Wuff"] // Print;

(* hidden case: topology, strings *)

$Topology[Topology[]] // Print;
TopoID`Topology`Private`$Topology[Topology[], "Solve", "Alpha"] // Print;

TopoID`Topology`Private`$Topology[Topology[], "Solve", "Wuff"] // Print;

(* hidden case: rules *)

$Topology[name -> "Dog", hist -> {}, facs -> {}] // Print;

$Topology[name -> "Dog", hist -> {}, facs -> {}, bone -> {}] // Print;

(* hidden case: topology, rules *)

$Topology[Topology[], name -> "Dog"] // Print;
TopoID`Topology`Private`$Topology[Topology[], name -> "", name -> "Dog"] // Print;

TopoID`Topology`Private`$Topology[Topology[], name -> "Dog"] // Print;

TopoID`Topology`Private`$Topology[Topology[], name -> "Dog", bone -> {}] // Print;

(* main: topology, strings, rules *)

Topology["Solve"] // Print;
Topology[name -> "Dog"] // Print;
Topology["Solve", name -> "Dog"] // Print;
Topology[name -> "Dog", "Solve"] // Print;

Topology["Wuff", bone -> {}] // Print;
Topology[name -> Dog] // Print;

(* overload: lists, topologies *)
Topology[name -> "Dog", {"Solve", "Alpha"}] // Print;

(* shortcut: all entries allowed *)
Topology[All] // Print;

(* shortcut: keys as keywords *)
Topology[scps, rels] // Print;

(* trap *)
Topology[Solve] // Print;

(* >hist<: append, clear *)
Topology[hist -> 1, name -> "Dog", hist -> 2] // Print;
Topology[hist -> 1, name -> "Dog", hist -> None, hist -> 2] // Print;

(* else: overwrite, clear *)
Topology["Solve", rels -> {0}] // Print;
Topology["Solve", rels -> {0}, rels -> {1}] // Print;
Topology["Solve", rels -> {0}, rels -> None] // Print;
Topology["Solve", rels -> {0}, scps -> None, rels -> None] // Print;

Topology[rels -> fff, scps -> ex^2]

TopologyQ[Topology[]] // Print;
TopologyQ[Topology["Solve"]] // Print;
TopologyQ[Topology[], "Solve"] // Print;
TopologyQ[Topology["Solve"], "Solve"] // Print;

(* --- *)

Exit[];
