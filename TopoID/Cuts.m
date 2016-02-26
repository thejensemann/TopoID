(* -- "Cuts.m": Inspect Possible Cuts of Objects -------------------- *)

(* --- provided functions:

- Cut[s] -- gives the list of possible cuts of a diagram or a topology
    with embedded graph information,

- Cut[s]Symbols -- returns the list of cuts expressed not via the
   positions of corresponding factors but in terms of the symbols of
   factors (not for diagrams), and

- Cut[s]Q, NoCut[s]Q -- query if an object has any cuts at all, or not.

*)

(* --- N.B.:

- A graph is given by a set of lines denoted by
    <label>, <from-vertex> and <to-vertex>.
- Assign in-going and out-going legs to source (vertex 1) and sink
  (vertex 2).
- Classify lines by numbers (minimum and maximum) of allowed cuts or by
    can ('C'), cannot ('N'), and must ('M')
  be cut internally.
- Build adjacency matrix of simple graph by imposing merging conditions,
  cf. below.  External lines cannot be cut by definition.
- Classify vertices in all possible ways in order to find the cuts,
  cf. below.
- Find cut lines in each case and impose counting conditions.  A cut
  line has two differently classified vertices (< 0 and > 0), the
  counting conditions must hold for the complete set of cut lines.

------------------------------------------------------------------------
Implementation: Do not rewrite loops as mappings, b/c of interdependency
  of iterations.
------------------------------------------------------------------------

For all possible combinations of "", "C", "M" and "N" one can apply
these rules for merging:
  [1] (" ","X") -> do nothing (leave "X"),
  [2] ("C"," ") -> "C",
  [3] ("C","X") -> adopt "X" ("X" = "C","M","N"),
  [4] ("M","X") -> adopt "M" ("X" = " ","C","M"),
  [5] ("N","X") -> adpot "N" ("X" = " ","C","N"), and
  [6] ("M","N"), ("N","M") -> error: conflict,
or slightly rewritten:
  [2-5] ("X","Y") -> "X" ("Y" = " ","C"), and
  [4,5] ("X","X") -> leave "X" ("X" = "M","N").

------------------------------------------------------------------------
The vertex classes are defined via:
  0                 -- undefined (no constraints),
  i: 1 < i <= <n>   -- same as the vertex (i - 1),
  i: -1 > i >= -<n> -- opposite to the vertex (-i + 1),
  <n> + 2           -- class of the source, and
  -<n> - 2          -- class of the sink,
where <n> gives the number of vertices in the graph.

*)

(* --- package begin ------------------------------------------------ *)

Unprotect["TopoID`Cuts`*"];

ClearAll["TopoID`Cuts`*", "TopoID`Cuts`Private`*"];

BeginPackage[
  "TopoID`Cuts`",
  {"TopoID`Common`", "TopoID`System`",  (* TODO *)
   "TopoID`Topology`"}];

{$CutClasses, $CutRules,$Cuts, Cuts,CutsComponents, CutsSymbols, CutsQ};

Begin["`Private`"];

(* --- $CutClassQ --------------------------------------------------- *)

$CutClasses = {"", "C", "M", "N"};

$Cuts::class = "\
Unknown edge class in adjacency matrix at position {`1`, `2`}: `3`.";

$CutClassQ[e_] :=
    MemberQ[$CutClasses, e];

(* --- $CutsIncrement ----------------------------------------------- *)

(* N.B.:
- Increments list <v> within minima <mn>, maxima <mx> and indices from
  <b> to <e>, optionally.
- All parameters must be (composed of) integers.
- Returns True on success, False otherwise.
*)

Attributes[$CutsIncrement] =
{HoldAll};

$CutsIncrement[v_, mn_, mx_, b_:0, e_:0, vb_:0] :=
    Module[{vp, bp,ep, i},

           vp = vb /. VerbosityRules;

           Report[vp, 1, "[$CutsIncrement]"];

           (* -- check input -- *)

           If[!MatchQ[{v, mn, mx, b, e},
                      {{__Integer}.., _Integer, _Integer}],
              Message[$CutsIncrement::usage];
              Abort[]];

           (* -- apply defaults -- *)

           {bp, ep} = {b, e};

           bp = If[bp <= 0, 1, bp];
           ep = If[ep <= 0, Length[v], ep];

           If[bp > ep, {bp, ep} = {ep, bp}];

           ep = Min[ep, Length /@ {v, mn, mx}] + 1;

           Report[vp, 2, "Index Range: ", {bp, ep}, "."];

           (* find component to increment *)
           For[i = bp, i < ep && v[[i]] >= mx[[i]], i++];

           Report[vp, 2, "Component to increment: ", i, "."];

           (* nothing left to increment *)
           If[i == ep, Return[False]];

           (* reset every previous one *)
           v[[bp ;; i - 1]] = mn[[bp ;; i - 1]];

           (* increment found component *)
           v[[i]]++;

           Return[True]];

(* --- $CutsApply --------------------------------------------------- *)

(* N.B.:
- Applies constraints (i.e. propagates recursively) by adjacency matrix
  <m> on already known vertex classes <v>, starting from vertex <k>.
- The adjacency matrix must be (composed of) strings.
- All remaining parameters must be (composed of) integers.
- Returns True on success, False otherwise.
*)

Attributes[$CutsApply] =
{HoldAll};

Options[$CutsApply] =
{Verbosity -> False};  (* cf. VerbosityRules *)

$CutsApply[m_, v_, k_, opts:OptionsPattern[]] :=
    Module[{vb, n, i, l, f},

           vb = OptionValue[Verbosity] /. VerbosityRules;
           n = Length[m];

           Report[vb, 1, "[$CutsApply]: ", n, " vertices."];

           (* -- check input -- *)

           If[(!MatrixQ[m] || n != Length[m[[1]]] (* quadratic matrix *)
               || n != Length[v]                (* same length vector *)
               || k < 1 || k > n                (* index within range *)
               || !MatchQ[{m, v, k},                  (* all integers *)
                          {{{__String}..}, {__Integer}, _Integer}]),
              Message[$CutsApply::usage];
              Abort[]];

           (* -- treat every connected index -- *)

           Report[vb, 2, "Constrain links adjacent to ", k, ":\n  ",
                  v, "."];

           For[i = 1, i <= Length[m], i++,

               l = m[[i, k]];
               Report[vb, 3, {i, k} -> l, "."];

               (* no constraint *)
               If[MemberQ[{"", "C"}, l], Continue[]];

               (* set flag *)
               f = l /. {"N" -> 1, "M" -> -1};

               (* unknown class *)
               If[!$CutClassQ[l],
                  Message[$Cuts::class, k, i, l];
                  Abort[]];

               (* [1]: k unclassified *)
               If[v[[k]] === 0,
                  Report[vb, 4, "Vertex ", k, " is unclassified."];

                  (* [1.a]: i unclassified *)
                  If[v[[i]] === 0,
                     Report[vb, 5, "Vertex ", i, " is unclassified."];

                     v[[i]] = f*(k + 1);
                     Report[vb, 5, "Assigned class to vertex ", i, ": ",
                            v[[i]], "."];

                     If[!$CutsApply[m, v, i, opts], Return[False]];
                     Continue[]];

                  (* [1.b]: i classified *)
                  If[v[[i]] =!= f*(k + 1),
                     Report[vb, 5, "Vertex ", i, " has wrong class: ",
                            v[[i]], "."];
                     Return[False]];

                  Report[vb, 5, "Vertex ", i, " has right class: ",
                         v[[i]], "."];
                  Continue[],

                  (* [2]: k classified *)
                  Report[vb, 4, "Vertex ", k, " is classified: ",
                         v[[k]], "."];

                  (* [2.a]: i unclassified *)
                  If[v[[i]] === 0,
                     Report[vb, 5, "Vertex ", i, " is unclassified."];

                     (* k depends on i *)
                     If[v[[k]] === f*(i + 1),
                        Report[vb, 6, "Vertex ", k, " depends on ",
                               i, "."];
                        Continue[]];

                     v[[i]] = f*v[[k]];
                     Report[vb, 6, "Assigned class to vertex ", i, ": ",
                            v[[i]], "."];

                     If[!$CutsApply[m, v, i, opts], Return[False]];
                     Continue[]];

                  (* [2.b]: i classified *)
                  If[v[[i]] =!= f*v[[k]],
                     Report[vb, 5, "Vertex ", i, " has wrong class: ",
                            v[[i]], "."];
                     Return[False]];

                  Report[vb, 4, "Vertex ", i, " has right class: ",
                         v[[i]], "."]
              ]];

           Return[True]];

(* --- $CutsConnect ------------------------------------------------- *)

(* N.B.:
- Maps the connected component reachable from vertex <k> to the same
  class with adjacency matrix <m> and already known vertex classes <v>.
- The adjacency matrix must be (composed of) strings.
- All remaining parameters must be (composed of) integers.
- Returns True on success, False otherwise.
*)

Attributes[$CutsConnect] =
{HoldAll};

Options[$CutsConnect] =
{Verbosity -> False};  (* cf. VerbosityRules *)

$CutsConnect[m_, v_, k_, opts:OptionsPattern[]] :=
    Module[{vb, n, i, l},

           vb = OptionValue[Verbosity] /. VerbosityRules;
           n = Length[m];

           Report[vb, 1, "[$CutsConnect]: ", n, " vertices."];

           (* -- check input -- *)

           If[(!MatrixQ[m] || n != Length[m[[1]]] (* quadratic matrix *)
               || n != Length[v]                (* same length vector *)
               || k < 1 || k > n                (* index within range *)
               || !MatchQ[{m, v, k},                  (* all integers *)
                          {{{__String}..}, {__Integer}, _Integer}]),
              Message[$CutsConnect::usage];
              Abort[]];

           (* -- treat every connected vertex -- *)

           Report[vb, 2, "Map component connected to ", k, ":\n  ",
                  v, "."];

           (* k not restricted *)
           If[v[[k]] === 0,
              Report[vb, 3, "No class to propagate."];
              Return[True]];

           For[i = 1, i <= Length[m], i++,

               l = m[[i, k]];
               Report[vb, 3, {i, k} -> l, "."];

               If[!$CutClassQ[l],
                  Message[$CutConnect::class, i, k, l];
                  Abort[]];

               (* no link *)
               If[l === "", Continue[]];

               (* i not restricted *)
               If[v[[i]] === 0,
                  v[[i]] = v[[k]];
                  Report[vb, 4, "Mapped vertex ", i, " to vertex ", k,
                         ": ", v[[i]], "."];
                  If[!$CutsConnect[m, v, i, opts], Return[False]];
                  Continue[]];

               (* conflict *)
               If[v[[i]] =!= v[[k]],
                  Report[vb, 4,"Conflicting classes: ",
                         i, ", ", k, "."];
                  Return[False]]
           ];

           Return[True]];

(* --- $CutsClassify ------------------------------------------------ *)

(* N.B.:
- Finds the cuts of a graph between vertices 1 (source) and 2 (sink).
- The adjacency matrix must be (composed of) strings.
- Returns a list of vertex classifications for possible cuts.
*)

Attributes[$CutsClassify] =
{HoldAll};

Options[$CutsClassify] =
{Verbosity -> False};  (* cf. VerbosityRules *)

$CutsClassify[m_, opts:OptionsPattern[]] :=
    Module[{vb, n, ci, v, cs, i, mn,mx, t, f, r, k, w, u, f0, f1},

           vb = OptionValue[Verbosity] /. VerbosityRules;
           n = Length[m];
           cs = {};

           Report[vb, 1, "[$CutsClassify]: ", n, " vertices."];

           (* -- check input -- *)

           (* quadratic matrix of integers *)
           If[!MatrixQ[m] || n != Length[m[[1]]]
              || !MatchQ[m, {{__String}..}],
              Message[$CutsClassify::usage];
              Abort[]];

           (* initialize vertex classes *)
           ci = {n + 2, -n - 2};
           v = Join[ci, Table[0, {n - 2}]];
           Report[vb, 2, "Initial assignment of classes:\n  ", v, "."];

           (* [1]: impose initial conditions *)
           For[i = 1, i <= n, i++,
               Report[vb, 3, "Propagate classes from vertex ", i, "."];
               If[!$CutsApply[m, v, i, Verbosity -> vb - 1],
                  Report[vb, 4, "Conflicting classes."];
                  Return[cs]]];
           Report[vb, 2, "Imposed initial conditions on classes:\n  ",
                  v, "."];

           (* [2]: range for independent vertices *)
           mn = Table[0, {n}];
           mx = v /. {0 -> 1, _Integer -> 0};
           Report[vb, 2, "Range for independent vertices:\n  ",
                  mn, ",\n   ", mx, "."];

           (* [3]: loop over possible assignments *)
           For[t = mn; f = True, f,
               f = $CutsIncrement[t, mn, mx, 0, 0, vb - 1],
               Report[vb, 3, "Test hypothesis:\n  ", t, "."];

               (* [3.a.i]: fix independent vertices *)
               For[i = 1; r = v, i <= n, i++,
                   If[r[[i]] =!= 0, Continue[]];
                   r[[i]] = If[t[[i]] =!= 0, ci[[1]], ci[[2]]]];
               Report[vb, 4, "Fixed independent vertices:\n  ", r, "."];

               (* [3.a.ii]: fix dependent vertices *)
               For[i = 1, i <= n, i++,
                   k = r[[i]];
                   If[MemberQ[ci, k], Continue[]];
                   If[k > 0, r[[i]] = r[[k - 1]]; Continue[]];
                   If[k < 0, r[[i]] = -r[[-k - 1]]; Continue[]]];
               Report[vb, 4, "Fixed dependent vertices:\n  ", r, "."];

               (* [3.a.iii]: apply to adjacency matrix *)
               For[i = 1; w = m, i <= n, i++,
                   For[j = i + 1, j <= n, j++,
                       If[r[[i]] === r[[j]], Continue[]];
                       w[[i, j]] = w[[j, i]] = ""]];
               Report[vb, 5, "Adjacency matrix after cut:\n",
                      MatrixForm[w], "."];

               (* initialize vertex classes *)
               u = Join[ci, Table[0, {n - 2}]];

               (* [3.b.i]: first-connected component of 1 *)
               For[i = 3; f0 = 2, i <= n, i++,
                   If[w[[i, 1]] =!= "",
                      f0 = i;
                      u[[i]] = u[[1]];
                      Break[]]];
               Report[vb, 4, "Connected to 1 (non-existing if 2): ",
                      f0, "."];

               (* [3.b.ii]: first-connected component of 2 *)
               For[i = 3; f1 = 1, i <= n, i++,
                   If[w[[i, 2]] =!= "",
                      f1 = i;
                      u[[i]] = u[[2]];
                      Break[]]];
               Report[vb, 4, "Connected to 2 (non-existing if 1): ",
                      f1, "."];

               (* [3.c.i]: color component of 1 *)
               If[!$CutsConnect[w, u, f0, Verbosity -> vb - 1],
                  Report[vb, 4, "Could not color component 1."];
                  Return[cs]];

               (* [3.c.ii]: color component of 2 *)
               If[!$CutsConnect[w, u, f1, Verbosity -> vb - 1],
                  Report[vb, 4, "Could not color component 2."];
                  Return[cs]];

               (* [3.c.iii]: find uncolored vertices *)
               If[MemberQ[u, 0],
                  Report[vb, 4, "Found uncolored vertices:\n  ",
                         u, "."];
                  Continue[]];

               (* save the cut *)
               AppendTo[cs, r];
               Report[vb, 3, "Saved a cut:\n  ", r, "."]
           ];

           Return[cs]];

(* --- $Cuts -------------------------------------------------------- *)

(* vertex label -> vertex class *)
$CutRules["vertex"] =
{in[_Integer] -> 1,
 out[_Integer] -> 2,
 (* -- *)
 i_Integer -> i + 2 };

(* diagram: line label -> line class *)
$CutRules["dia"] =
{"gh" -> {0, Infinity},
 "gl" -> {0, Infinity},
 "hb" -> {1, Infinity},
 "qd" -> {0, Infinity},
 "qu" -> {0, Infinity},
 "si" -> {0, 0},
 (* -- *)
 All -> {1, Infinity}};

(* topology: line label -> line class *)
$CutRules["top"] =
{0 -> {0, Infinity},
 Global`mh -> {1, 1},
 (* -- *)
 All -> {1, Infinity}};

(* line label -> line class *)
$CutRules["edge"] =
  Union[$CutRules["dia"], $CutRules["top"]];

Options[$Cuts] =
{Verbosity -> False,  (* cf. VerbosityRules *)
 Method ->            (* lists of rules *)
 {"vertex" -> $CutRules["vertex"], "edge" -> $CutRules["edge"]}};

$Cuts::incompatible = "\
Incompatible classifications for edge {`1`, `2`}: \"`3`\", \"`4`\".";

$Cuts[obj:DiagramPattern[], noc_Integer:0,
  opts:OptionsPattern[]] :=
  Module[
    {vb,me, nod, es, crs,nrs, n, amtx, e,l,
     cuts, fs,gs, cnts, cut,c,cnt, lhs,rhs, i,f},

    {vb, me} = OptionValue[{Verbosity, Method}];
    vb = vb /. VerbosityRules;
    me = Cases[me, _Rule];

    nod = noc /. 0 -> Infinity;

    Report[vb, 1, "[Cuts]: ",
           nod, " cuts for \"", name /. obj, "\"."];

    (* -- check input -- *)

    If[nod < 0,
       Message[Cuts::usage];
       Abort[]];

    (* -- classify vertices and edges -- *)

    (* all edges *)
    es = Join[legs /. obj, pros /. obj];

    (* delete pure numerators *)
    es = DeleteCases[es, {Null, __}];

    (* vertex and line classification, omitt momenta *)
    es = {#[[3]] /. ("vertex" /. me), #[[4]] /. ("vertex" /. me),
          #[[1]] /. ("edge" /. me)} & /@ es;

    Report[vb, 2, "Externally classified edges:\n",
           TableForm[es], "."];

    (* consecutive vertex numbers *)
    nrs = Union[Flatten[#[[{1, 2}]] & /@ es]];
    nrs = MapIndexed[#1 -> #2[[1]] & , nrs];

    (* internal classes *)
    crs = {{_, 0} -> "N",
           {0, _} -> "C",
           {_, _} -> "M"};

    (* external lines cannot be cut by definition *)
    es = {#[[1]] /. nrs, #[[2]] /. nrs,
          If[Min[#[[{1, 2}]]] <= 2, "N", #[[3]] /. crs]} & /@ es;

    Report[vb, 2, "Internally classified edges:\n",
           TableForm[es], "."];

    (* -- build adjacency matrix -- *)

    (* maximum vertex label *)
    n = Max[#[[{1, 2}]] & /@ es];

    Report[vb, 2, "Number of vertices: ", n, "."];

    (* initialize empty matrix *)
    amtx = Table["", {n}, {n}];

    (* treat all edges *)
    Do[l = amtx[[e[[1]], e[[2]]]];
       If[MemberQ[{"", "C"}, l], l = e[[3]]];
       If[Complement[{"N", "M"}, {l, e[[3]]}] === {},
          Message[Cuts::incompatible,
            e[[1]], e[[2]], e[[3]], l];
          Return[{}]];
       amtx[[e[[1]], e[[2]]]] = amtx[[e[[2]], e[[1]]]] = l,
       {e, es}];

    Report[vb, 2, "Adjacency matrix:\n", MatrixForm[amtx], "."];

    (* -- classify connectivity components -- *)

    cuts = $CutsClassify[amtx, Verbosity -> vb - 1];

    Report[vb, 2, "Obtained ", Length[cuts],
           " vertex classifications:\n", TableForm[cuts], "."];

    (* -- filter by line specifications -- *)

    (* only internal edges with pure numerators *)
    fs = {#[[3]] /. ("vertex" /. me) /. nrs,
          #[[4]] /. ("vertex" /. me) /. nrs,
          #[[1]]} & /@ (pros /. obj);

    (* counting reference *)
    gs = "edge" /. me;
    If[FreeQ[OptionValue[Method], All, 1],
       gs = FilterRules[gs, Append[Last /@ fs, All]]];
    Report[vb, 2, "Counting reference:\n", TableForm[gs], "."];

    cnts = {};

    (* treat all cuts *)
    Do[If[Length[cnts] >= nod, Break[]];
       (* initialize counters *)
       ClearAll[c];
       c[_] = 0;
       cnt = {};
       lhs = rhs = {};
       (* treat all edges *)
       For[i = 1, i <= Length[fs], i++,
           f = fs[[i]];
           If[f[[3]] === Null, Continue[]];
           (* increment counters for cut line *)
           If[cut[[f[[1]]]]*cut[[f[[2]]]] < 0,
              c[f[[3]]]++;
              c[All]++;
              AppendTo[cnt, i]];
           If[cut[[f[[1]]]] < 0 && cut[[f[[2]]]] < 0,
              AppendTo[rhs, i]];
           If[cut[[f[[1]]]] > 0 && cut[[f[[2]]]] > 0,
              AppendTo[lhs, i]]];
       (* accept a valid cut *)
       If[And @@ (#[[2, 1]] <= c[#[[1]]] &&
                  c[#[[1]]] <= #[[2, 2]] & /@ gs),
          (* in terms of factor positions *)
          AppendTo[cnts, {Sort[cnt], Sort[lhs] -> Sort[rhs]}];
          Report[vb, 3, "Accepted cut:\n  ", cut, "."]],
       {cut, cuts}];

    cnts = SortBy[cnts, First];
    Return[cnts]];

(* --- Cuts --------------------------------------------------------- *)

Options[Cuts] =
  Options[$Cuts];

Cuts::usage = "\
TODO";

(* redirect *)
Cuts[
  obj:DiagramPattern[], noc_Integer:0, opts:OptionsPattern[]] :=
  First /@ $Cuts[obj, noc, opts];

(* trap *)
Cuts[___] :=
  (Message[Cuts::usage];
   Abort[]);

(* aliases *)

Cuts[
  obj:DiagramPattern[], All | Automatic | Full | Infinity,
    opts:OptionsPattern[]] :=
  Cuts[obj, 0, opts]

Cut[args___] :=
  Cuts[args];

(* --- CutsComponents ----------------------------------------------- *)

Options[CutsComponents] =
  Options[$Cuts];

CutsComponents::usage = "\
TODO";

(* redirect *)
CutsComponents[
  obj:DiagramPattern[], noc_Integer:0, opts:OptionsPattern[]] :=
  Last /@ $Cuts[obj, noc, opts];

(* trap *)
CutsComponents[___] :=
  (Message[CutsComponents::usage];
   Abort[]);

(* aliases *)

CutsComponents[obj:DiagramPattern[],
  All | Automatic | Full | Infinity, opts:OptionsPattern[]] :=
  CutsComponents[obj, 0, opts]

CutComponents[args___] :=
  CutsComponents[args];

(* --- CutsSymbols -------------------------------------------------- *)

Options[CutsSymbols] =
  Options[$Cuts];

CutsSymbols::usage = "\
TODO";

CutsSymbols[
  top:TopologyPattern["Graph"], noc_Integer:0, opts:OptionsPattern[]] :=
  Sort[Sort[Part[First /@ (facs /. top), #]] & /@ Cuts[top, noc, opts]];

(* trap *)
CutsSymbols[___] :=
  (Message[CutsSymbols::usage];
   Abort[]);

(* aliases *)

CutsSymbols[
  top:TopologyPattern["Graph"], All | Automatic | full | Infinity,
    opts:OptionsPattern[]] :=
  CutsSymbols[top, 0, opts];

CutSymbols[args___] :=
  CutsSymbols[args];

(* --- CutsQ -------------------------------------------------------- *)

Options[CutsQ] = Options[NoCutsQ] =
  Options[Cuts];

CutsQ::usage = NoCutsQ::usage  = "\
TODO";

CutsQ[
  obj:DiagramPattern[], opts:OptionsPattern[]] :=
  (Cuts[obj, 1, opts] =!= {});

(* trap *)
CutsQ[___] :=
  (Message[CutsQ::usage];
   Abort[]);

(* alias *)
CutQ[args___] :=
  CutsQ[args];

(* --- package end -------------------------------------------------- *)

Protect["TopoID`Cuts`*"];

Scan[
  SetAttributes[#, {ReadProtected}] & ,
  Select[Symbol /@ Names["TopoID`Cuts`*"], Head[#] === Symbol & ]];

Unprotect[$CutRules];

End[];

EndPackage[];

(* ------------------------------------------------------------------ *)



(* --- TODO:

- Cuts: Use CheckOptions.
- Cuts: overload with defaults ("vertex" /. (Method /. Options[Cuts])).

- remove CutQ? export?

*)
