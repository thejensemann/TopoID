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
   "TopoID`Topology`"}];

(* functions *)
{(*$CrusherConfigFunction,*) CrusherConfig, WriteCrusherConfig,
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

$CrusherConfigFunction["extsymfile"] =
  "symm_" <> (name /. #) <> ".dat" & ;

$CrusherConfigFunction["intname"] =
  name /. # & ;

$CrusherConfigFunction["dir"] =
  "data_" <> (name /. #) <> ".dir" & ;

$CrusherConfigFunction[OutputForm] = CrusherFile[
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
  CrusherComment[2]] & ;

Options[CrusherConfig] =
{

   (* inferred from input, but can be overwritten:

   "symb" -> {},
   "powsymb" -> {},
   "inv" -> {},

   "ext" -> {},
   "limp" -> {},
   "userrep" -> {},

   "prop" -> {},

   "toptopo" -> {},
   "topo" -> {},
   "zerotopo" -> {},

   *)

   "useintsym" -> 1,
   "usesym" -> 1,
   "extsymfile" -> Automatic,

   "elim" -> "PROP",
   "basis" -> "NEG",
   "get_master" -> 0,

   "seeds" -> {1, 1},
   "minseeds" -> {1, 1},
   "adaptive" -> 1,

   "intname" -> Automatic,
   "delim" -> "",
   "program" -> 1,

   "dir" -> Automatic,
   "debug" -> 2,
   "threads" -> 12,

   OutputForm -> Automatic

};

(* TODO: move in Common.m *)
$PowersToSymbols =
{s_^p_?Negative :> Symbol[$ToString[s] <> "i" <> ToString[-p]],
 s_^p_ :> Symbol[$ToString[s] <> $ToString[p]]};

(* main *)
CrusherConfig[
  top:TopologyPattern[], set:SetupPattern[], opts:OptionsPattern[]] :=
  Module[
    {optr, tmp},

    optr = {opts} /. Rule[s_String, Automatic] :> Rule[s, $CrusherConfigFunction[s]];
    optr = optr /. Rule[s_String, f_Function] :> Rule[s, f[top]];

    keys = {};


    tmp =
    {"symb" -> ("symb" /. optr /. "symb" :> ),


     "ext" -> ("ext" /. optr /. "ext")
    ];

    {
       "ext" :> {#, #^2 //. (cs /. set) //. (rs /. set) /. $PowersToSymbols} & /@ (ps /. set),
       "limp" :> (ks /. set),
     };

    {sy, ze} = {symm /. zero} /. top;

    If[sy =!= symm,
       Sow["useintsym" -> 0];
       Sow["usesym" -> 0];
       Sow["extsymfile" -> ("extsymfile" /. optr)],
       Sow["extsymfile" -> ""]];

    If[ze =!= zero,
       ze = ;                                                                              (* TODO: subset to sector *)
       Sow["zerotopo" -> ze]];


    {"symb" -> {},*)  (*set*)
   (*"powsymb" -> {},*)  (*top*)
   (*"inv" -> {},*)  (*set*)

   (*"ext" -> {}(*)  (*set*)
   (*"limp" -> {}(*)  (*set*)
   (*"userrep" -> {}*)  (*set*)

   (*"prop" -> {}*)  (*top*)

   (*"toptopo" -> {}*)  (*top*)
   (*"topo" -> {}*)  (*top*)
   (*"zerotopo" -> {}*)  (*top*)



    off = OptionValue[OutputForm] /. Automatic -> $CrusherConfigFunction[OutputForm];
    off[tmp]];




(*
Rules -> {x_^i_ :> ToString[x] <> ToString[i]},

CrusherConfig[
  top:TopologyPattern[], set:SetupPattern[], opts:OptionsPattern[]] :=
  Module[
    {m2r, f, g, trs, um, ss, zs},

    (* helper: rewrite invariants *)
    m2r =
    f = # //. (cs /. set) //. (rs /. set) /. m2r & ;
    (* helper: strip spaces *)
    g = StringReplace[ToString[#], " " -> ""] & ;

    trs = OptionValue[{Transformations}];

    (* needed and vanishing sectors *)
    um = 0 & /@ (facs /. top);

    ss = subt /. top;
    If[(pros /. top) === pros,
       ss = {If[DenominatorSymbolQ[#], 1, 0] & /@ First /@ (facs /. top)}];
    (* TODO: else ... *)

    zs = Function[z, ReplacePart[um, # -> 1 & /@ z]] /@ (zero /. top);

    "symb", Join[ks /. set, ps /. set, {"d"}, Variables[f[(ms /. set)^2]], Variables[f[xs /. set]]],
    (* TODO: if symm add a1, ..., a<Length[facs]>, add. symbols *)

    "inv", Join[{"d"}, Variables[f[(ms /. set)^2]], Variables[f[xs /. set]]],


MapThread[
  CrusherSetting["prop", g[#1], f[#2]] & ,
        {TopologyMomentaFlows[top, set], TopologyMasses[top, set]}],

      CrusherSetting["zerotopo", #] & /@ zs,

"extsym" ->
{CrusherSetting["powsymb", {"a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9"}],
 CrusherSetting["extsymfile", "symmA"},

      OptionValue[Settings][top, set]
      ]];
*)











Defaults[CrusherConfig] =
{

   "elim" -> Null

   }

Checks[CrusherConfig] =
{};

CheckMessages[CrusherConfig] =
{};












(* overload: plural *)
CrusherConfig[
  tops_?TopologyListQ, set:SetupPattern[], opts:OptionsPattern[]] :=
  CrusherConfig[#, set, opts] & /@ tops;

(* alias *)
CrusherConfig[top:TopologyPattern["Setup"]] :=
  CrusherConfig[top, setp /. top];

(* trap *)
CrusherConfig[___] :=
  (Message[CrusherConfig::usage];
   Abort[]);







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
CrusherComment[<text(s)>] is a wrapper for a commentary line in a \
Crusher config file.";

Format[CrusherComment[str___]] :=
  StringJoin["#", $ToString[str], "\n"];

CrusherSetting::usage = "\
CrusherSetting[<var>, <val(s)>] is a wrapper for a Crusher input \
setting, where <var> denotes the set variable and <val(s)> its \
corresponding value(s).";

(* alias: list *)
CrusherSetting[var_, a___, b_List, c___] :=
  CrusherSetting[var, a, Sequence @@ b, c];

Format[CrusherSetting[var_, vals___]] :=
  StringJoin[First[#], "=", Riffle[Rest[#], ","], "\n"] & @
  StringReplace[$ToString /@ {var, vals}, " " -> ""];

                                                                        (* TODO: make an alias *)
Format[CrusherSetting[var_, lhs_ -> rhs_] :=
  StringJoin[Part[#, 1], "=", Part[#, 2], "==", Part[#, 3], "\n"] & @
  StringReplace[$ToString /@ {var, lhs, rhs}, " " -> ""];

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

- CrusherComment[3, ]
- CrusherComment[0, ]

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
