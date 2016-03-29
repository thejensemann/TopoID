(* -- "Crusher.m": Interface to Crusher ----------------------------- *)

(* --- provided functions:

TODO

*)

(* --- package begin ------------------------------------------------ *)

Unprotect["TopoID`Crusher`*"];

ClearAll["TopoID`Crusher`*", "TopoID`Crusher`Private`*"];

BeginPackage[
  "TopoID`Crusher`",
  {"TopoID`Common`", "TopoID`System`",  (* TODO *)

   "TopoID`Setup`",
   "TopoID`Topology`",

   "TopoID`Mapping`"}];  (* TODO: -> Polynomial? *)

(* functions *)
{CrusherConfig, WriteCrusherConfig,
 CrusherSymmetries, WriteCrusherSymmetries,
 CrusherList, WriteCrusherList};

(* wrappers *)
{CrusherComment, CrusherSetting,
 CrusherRelation, CrusherRelations,
 CrusherFile};

Begin["`Private`"];

(* --- CrusherConfig ------------------------------------------------ *)

CrusherConfig::usage = "\
CrusherConfig[<top>, <set>, [opts]] generates a Crusher config file \
for topology <top> in the kinematic setup <set>.  See \
Options[CrusherConfig] for the available options.";

Options[CrusherConfig] =
{"symb" -> Automatic,        (* {(String | Symbol)...} *)
 "powsymb" -> Automatic,     (* {(String | Symbol)...} *)
 "inv" -> Automatic,         (* _List *)

 "ext" -> Automatic,         (* {(String | Symbol)...} *)
 "limp" -> Automatic,        (* {(String | Symbol)..} *)
 "userrep" -> Automatic,     (* {___Rule} *)

 "prop" -> Automatic,        (* {{_, _}..} *)

 "toptopo" -> Automatic,     (* {{(0 | 1)..}...} *)
 "topo" -> Automatic,        (* {{(0 | 1)..}...} *)
 "zerotopo" -> Automatic,    (* {{(0 | 1)..}...} *)

 "useintsym" -> Automatic,   (* 0 | 1 *)
 "usesym" -> Automatic,      (* 0 | 1 *)
 "extsymfile" -> Automatic,  (* _String *)

 "elim" -> "PROP",           (* "PROP" | "SP" *)
 "basis" -> "NEG",           (* "NEG" | "POS" *)
 "get_master" -> 0,          (* 0 | 1 *)

 "seeds" -> {1, 1},          (* {_Integer?Positive, _Integer?Positve} *)
 "minseeds" -> {1, 1},       (* {_Integer?Positive, _Integer?Positve} *)
 "adaptive" -> 0,            (* 0 | 1 *)

 "intname" -> Automatic,     (* _String | _Symbol *)
 "delim" -> None,            (* _String | _Symbol *)
 "program" -> 1,             (* 0 | 1 | 2 *)

 "dir" -> Automatic,         (* _String *)
 "debug" -> 2,               (* 0 | 1 | 2 | 3 *)
 "threads" -> 12,            (* _Integer?Positive *)

 OutputForm -> Automatic};   (* cf. Defaults[CrusherConfig] *)

(* #1: <top>, #2: <setup> *)
Defaults[CrusherConfig] =
{

   "symb" -> (Join[
     ps /. #2, ks /. #2, Variables[
       Join[xs /. #2, ms^2 /. #2, Last /@ (rs /. #2)]
       /. $PowerToSymbol], {"d"}] & ),

   "powsymb" ->
     (Function[i, "a" <> ToString[i]] /@ Range[Length[facs /. #]] & ),

   "inv" -> (Append[Variables[
     Join[xs /. #2, ms^2 /. #2, Last /@ (rs /. #2)]
     /. $PowerToSymbol], "d"] & ),

   "ext" ->
     (Transpose[{ps /. #2, ps^2 /. #2 //. (cs /. #2)}] & ),

   "limp" ->
     (ks /. #2 & ),

   "userrep" ->
     (rs /. #2 /. $PowerToSymbol & ),

   "prop" -> (Transpose[
     {TopologyMomentaFlows[##], TopologyMasses[##]
      /. $PowerToSymbol}] & ),

   "toptopo" | "topo" -> (Which[
     (* <subt> *)
     (subt /. #) =!= subt, DeleteDuplicates[GroupToSector[
       Join @@ Select[subt /. #, Function[
         s, Length[First[s]] === Length[Part[subt /. #, 1, 1]]]],
       Length[facs /. #]]],
     (* <pros> *)
     (pros /. #) =!= pros, Function[
       p, If[p[[1]] === Null || p[[3]] === p[[4]] === 0, 0, 1]
     ] /@ (pros /. #1),
     (* default: <facs> *)
     True, Function[
       f, If[DenominatorSymbolQ[First[f]], 1, 0]] /@ (facs /. #1)
     ] & ),

   "zerotopo" ->
     (GroupToSector[zero /. # /. zero -> {}, Length[facs /. #]] & ),

   "extsymfile" ->
     ("symm_" <> (name /. #) <> ".dat" & ),

   "intname" ->
     (name /. # & ),

   "dir" ->
     ("data_" <> (name /. #) <> ".dir" & ),

   OutputForm -> (CrusherFile[
     CrusherComment[2, "Crusher config file"], "\n",
     CrusherComment[3, "symbol declaration"], "\n",
     CrusherComment["symbols used in input file (and external IBPs)"],
     CrusherSetting["symb", "symb" /. #], "\n",
     CrusherComment["symbols to be substituted for powers (optional)"],
     CrusherSetting["powsymb", "powsymb" /. #], "\n",
     CrusherComment["invariants of the problem; including the dimension d (for Fermat)"],
     CrusherSetting["inv", "inv" /. #], "\n",
     CrusherComment[3, "kinematic setup"], "\n",
     CrusherComment["list: independent external momenta p, values of p^2"],
     Function[ex, CrusherSetting["ext", ex]] /@ ("ext" /. #), "\n",
     CrusherComment["list: loop momenta"],
     Function[li, CrusherSetting["limp", li]] /@ ("limp" /. #), "\n",
     CrusherComment["relations among the invariants (optional)"],
     Function[us, CrusherSetting["userrep", us]] /@ ("userrep" /. #), "\n",
     CrusherComment[3, "topology definition"], "\n",
     CrusherComment["list: propagator momenta and masses"],
     Function[pr, CrusherSetting["prop", pr]] /@ ("prop" /. #), "\n",
     CrusherComment["list: toplevel topologies (as ordering mask)"],
     Function[to, CrusherSetting["toptopo", to]] /@ ("toptopo" /. #),
     CrusherComment["(not allowed with external symmetries)"], "\n",
     CrusherComment["list: starting topologies (to be solved)"],
     Function[to, CrusherSetting["topo", to]] /@ ("topo" /. #), "\n",
     CrusherComment["list: vanishing topologies (optional)"],
     Function[ze, CrusherSetting["zerotopo", ze]] /@ ("zerotopo" /. #), "\n",
     CrusherComment[3, "symmetries"], "\n",
     CrusherComment["constrained to sector: 0/1 (optional; default 1)"],
     CrusherSetting["useintsym", "useintsym" /. #], "\n",
     CrusherComment["among different sectors: 0/1 (optional; default 1)"],
     CrusherSetting["usesym", "usesym" /. #], "\n",
     CrusherComment["from external file (optional)"],
     CrusherSetting["extsymfile", "extsymfile" /. #],
     CrusherComment["(not allowed with external symmetries)"], "\n",
     CrusherComment[3, "master integrals"], "\n",
     CrusherComment["integrals to be eliminated first: PROP/SP (optional; default SP)"],
     CrusherSetting["elim", "elim" /. #], "\n",
     CrusherComment["basis to be used: NEG/POS (optional; default NEG)"],
     CrusherSetting["basis", "basis" /. #], "\n",
     CrusherComment["reveal master integrals (no insertion of subtopologies): 0/1 (optional; default 0)"],
     CrusherSetting["get_master", "get_master" /. #], "\n",
     CrusherComment[3, "seed integrals"], "\n",
     CrusherComment["seeds to be generated for starting topology: dots and scalar products"],
     CrusherSetting["seeds", "seeds" /. #], "\n",
     CrusherComment["minimal seeds to be generated (optional; default 1,1)"],
     CrusherSetting["minseeds", "minseeds" /. #], "\n",
     CrusherComment["adaptive mode for integral from file on command line: 0/1 (optional; default 0)"],
     CrusherSetting["adaptive", "adaptive" /. #], "\n",
     CrusherComment[3, "substitution list"], "\n",
     CrusherComment["integral name (optional; default Int)"],
     CrusherSetting["intname", "intname" /. #], "\n",
     CrusherComment["deliminator between propagators and scalar products (optional; default none)"],
     CrusherSetting["delim", "delim" /. #], "\n",
     CrusherComment["output format: 0/1/2 (FORM/Mathematica/Maple) (optional; default 0)"],
     CrusherSetting["program", "program" /. #], "\n",
     CrusherComment[3, "program parameters"], "\n",
     CrusherComment["data directory (optional; default ./data)"],
     CrusherSetting["dir", "dir" /. #], "\n",
     CrusherComment["debugging/verbosity level: 0/1/2/3 (optional; default 1)"],
     CrusherSetting["debug", "debug" /. #], "\n",
     CrusherComment["number of threads"],
     CrusherSetting["threads", "threads" /. #], "\n",
     CrusherComment[2]] & ),

   None -> ""};

(* main *)
CrusherConfig[                                                                                  (* TODO: clean up -v *)
  top:TopologyPattern[], set:SetupPattern[], opts:OptionsPattern[]] :=
  Module[
    {optr, keys, optt, optu, opf},

    optr = Options[CrusherConfig];

    keys = Cases[First /@ optr, _String];

    (* handle symmetries *)
    optt = If[
      (symm /. top) =!= symm,
      {"powsymb" -> Automatic,
       "useintsym" -> 0,
       "usesym" -> 0,
       "extsymfile" -> Automatic},
      {"powsymb" -> {},
       "useintsym" -> 1,
       "usesym" -> 1,
       "extsymfile" -> None}];

    optu = MapThread[Rule, {keys, keys /. {opts} /. optr}];

    (* apply defaults *)
    optu = optu /. Rule[k_, Automatic] :> Rule[k, k /. optt];
    optu = optu /. Rule[k_, Automatic] :> Rule[k, k /. Defaults[CrusherConfig]];
    optu = optu /. Rule[k_, v_] :> Rule[k, v /. Defaults[CrusherConfig]];

    (* apply functions *)
    optu = optu /. Rule[k_, v_Function] :> Rule[k, v[top, set]];

    (* handle symmetries *)
    optu = optu /. Rule["symb", v_] :> Rule["symb", Join[v, "powsymb" /. optu]];

    (* option: output form *)
    opf = OptionValue[OutputForm];
    opf = opf /. Automatic -> (OutputForm /. Defaults[CrusherConfig]);

    (* result *)
    opf[optu]];

(* overload: plural *)
CrusherConfig[
  tops_?TopologyListQ, set:SetupPattern[], opts:OptionsPattern[]] :=
  CrusherConfig[#, set, opts] & /@ tops;

(* trap *)
CrusherConfig[___] :=
  (Message[CrusherConfig::usage];
   Abort[]);

(* NOTE:

- Option values Automatic are inferred from input, but can also be given
  directly.

- "toptopo" and "topo" via <subt> should be ready for multigraphs, but
  "pros" still needs the change "/@" -> "@@@".

*)

(* TODO:

- <setp> alias

*)

(* --- CrusherSymmetries -------------------------------------------- *)

CrusherSymmetries::usage = "\
CrusherSymmetries[<top>] returns symmetry relations of the topology \
<top> suitable for Crusher.";

CrusherSymmetries::symbol = "\
The topology name \"`1`\" cannot be converted into a valid symbol.";

$CrusherSymmetriesMethods =
{"Comments"};

Options[CrusherSymmetries] =
{Method -> {}}

(* overload: plural *)
CrusherSymmetries[tops_?TopologyListQ] :=
  CrusherSymmetries /@ tops;

(* main *)
CrusherSymmetries[
  top:TopologyPattern["Symm"], opts:OptionsPattern[]] :=
  Module[
    {me,cp, ts, is, p,r, f,g, res,cmt},

    (* check: Method *)
    me = Flatten[{OptionValue[Method]}];
    cp = Complement[me, $CrusherSymmetriesMethods];
    If[cp =!= {},
       Message[Method::invalid, cp, $CrusherSymmetriesMethods];
       me = Complement[me, cp]];

    (* generate symbol name *)
    ts = Quiet[Check[Symbol[name /. top], Null]];

    (* check: symbol name *)
    If[ts === Null,
       Message[CrusherSymmetries::symbol];
       Abort[]];

    (* generate index symbols *)
    is = MapIndexed[Symbol["a" <> ToString[#2[[1]]]] & , facs /. top];

    (* helper: permute single index; #1: {<current_arguments>,
    <remaining_symbols>} (e.g. {{a1, a2, 0, 0}, {a4}}), #2: next
    position (e.g. 3) *)
    p = {ReplacePart[#1[[1]], #2 -> First[#1[[2]]]], Rest[#1[[2]]]} & ;

    (* helper: replacement for all indices; #1: source (e.g. {1, 2, 3}),
    #2: target (e.g. {1, 2, 4}) *)
    r = ts @@ First[Fold[p, {0 & /@ is, is[[#1]]}, #2]] & ;

    (* helper: single symmetry relation; #1: preferred representation
    (e.g. {1, 2, 3}), #2: symmetric one (e.g. {1, 2, 4}) *)
    f = CrusherRelation[r[#1, #1], -r[#1, #2]] & ;

    (* helper: all symmetry relations from a group; #1: group of
    symmetric representations ({{1, 2, 4}, {1, 2, 3}}) *)
    g = CrusherRelations @@ Function[s, f[First[#], s]] /@ Rest[#] & ;

    (* format all relations from all groups *)
    res = g /@ Reverse /@ Sort /@ (symm /. top);

    (* Method: "Comment" *)
    cmt = CrusherComment[Total[Length /@ res], " relations\n\n"];
    If[MemberQ[me, "Comments"],
       res = Prepend[res, cmt]];

    (* result *)
    CrusherFile @@ res];

(* trap *)
CrusherSymmetries[___] :=
  (Message[CrusherSymmetries::usage];
   Abort[]);

(* --- wrappers ----------------------------------------------------- *)

CrusherComment::usage = "\
CrusherComment[[n], <text(s)>] is a wrapper for a commentary line in a \
Crusher config file.  The number [n] controls the indentation level of \
<text(s)>.";

Format[CrusherComment[n_Integer:0, str___]] :=
  StringJoin[
    "#", If[n < 0, "", {" ", Table["-", {n}], If[n > 0, " ", ""]}],
    $ToString[str], "\n"];

CrusherSetting::usage = "\
CrusherSetting[<var>, <val(s)>] is a wrapper for a Crusher input \
setting, where <var> denotes the set variable and <val(s)> its \
corresponding value(s).";

(* alias: list *)
CrusherSetting[var_, a___, b_List, c___] :=
  CrusherSetting[var, a, Sequence @@ b, c];

CrusherSetting[var_, a___, b_Rule, c___] :=
  CrusherSetting[var, a, $ToString[First[b], "==", Last[b]], c];

Format[CrusherSetting[var_, vals___]] :=
  StringJoin[First[#], "=", Riffle[Rest[#], ","], "\n"] & @
  StringReplace[$ToString /@ {var, vals}, " " -> ""];

CrusherRelation::usage = "\
CrusherRelation[<term(s)>] is a wrapper for a relation equal to zero \
made up of term(s) <term(s)>.  Each term is a coefficient times an \
integral of the form <top>[<inds>], where <top> is a topology symbol \
and <inds> a sequence of indices.";

CrusherRelation::format = "\
The format of \"`x`\" is not
  <coeff>*<top>[<inds>]
where <coeff> is a coefficient, <top> a topology symbol and <inds> a \
sequence of integer or symbolic indices.";

(* alias: list *)
CrusherRelation[a___, b_List, c___] :=
  CrusherRelation[a, Sequence @@ b, c];
                                                                        (* TODO: Riffle[] instead of /@ ? *)
Format[CrusherRelation[expr___]] :=
  StringJoin[#, "\n"] & @
  (# /.
   {c_.*ts_Symbol[is__] /; MatchQ[{is}, {(_Integer | _Symbol)..}] :>
    {ToString[ts], "[", Riffle[ToString /@ {is}, ","], "]\n",
     ToString[c, InputForm], "\n"},
    x_ :>
      (Message[CrusherRelation::format, x];
       "!!! PARSING ERROR !!!\n")} & /@ {expr});

CrusherRelations::usage = "\
CrusherRelations[<rel(s)>] is a wrapper for relation(s) <rel(s)> \
wrapped itself in CrusherRelation.";

(* alias: list *)
CrusherRelations[a___, b_List, c___] :=
  CrusherRelations[a, Sequence @@ b, c];

Format[CrusherRelations[expr___]] :=
  StringJoin[ToString /@ {expr}, "\n"];

CrusherFile::usage = "\
CrusherFile[<conts>] is a wrapper for the contents <conts> of a file \
suitable for parsing by Crusher.";

(* alias: list *)
CrusherFile[a___, b_List, c___] :=
  CrusherFile[a, Sequence @@ b, c];

Format[CrusherFile[expr___]] :=
  StringJoin[ToString /@ {expr}];

(* --- package end -------------------------------------------------- *)

Protect["TopoID`Crusher`*"];

Scan[
  SetAttributes[#, {ReadProtected}] & ,
  Select[Symbol /@ Names["TopoID`Crusher`*"], Head[#] === Symbol & ]];

End[];

EndPackage[];

(* ------------------------------------------------------------------ *)



(* --- TODO:

-

*)










(*
WriteCrusherList[fn_String, l_List] := Module[
  {s, fs},
  s = l /. Int[x_, ___] -> x /. _Symbol[x__Integer] -> {x};
  s = Cases[s, {__Integer}, {0, Infinity}];
  s = StringJoin[Riffle[ToString /@ #, ","]] & /@ s;
  s = StringJoin[Riffle[s, "\n"]];
  fs = OpenWrite[fn];
  WriteString[fs, s];
  Close[fs]];
*)
