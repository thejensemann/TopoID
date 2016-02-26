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
{CrusherConfig, CrusherSymmetries};

(* wrappers *)
{CrusherComment, CrusherSetting,
 CrusherRelation, CrusherRelations,
 CrusherFile};

Begin["`Private`"];

(* --- CrusherConfig ------------------------------------------------ *)

CrusherConfig::usage = "\
TODO: [opts]

CrusherConfig[<top>, <set>, [opts]] generates a Crusher config file for \
topology <top> in the kinematic setup <set>.";

(*

Options[CrusherConfig] =
{

   Rules -> {x_^i_ :> ToString[x] <> ToString[i]},

   "intname" -> None,

   "symb" -> {},
   "inv" -> {},

   "toptopo" -> {},
   "topo" -> {},

   "minseeds" -> {1, 1},
   "seeds" -> {},

   Settings -> CrusherFile[

     CrusherComment["-- topology --"],
     "\n",

     CrusherComment["symbols used in input file (and external IBPs)"],
     CrusherSetting["symb", "symb" /. #],
     "\n",

     CrusherComment["invariants appearing (including dimension d)"],
     CrusherSetting["inv", "inv" /. #],
     "\n",

     CrusherComment["loop momenta"],
     Function[x, CrusherSetting["limp", x]] /@ ("limp" /. #),
     "\n",

     If[("ext" /. #) =!= "ext",
        {CrusherComment["external momenta p (independent), value of p^2"],
         Function[x, CrusherSetting["ext", x]] /@ ("ext" /. #),
         "\n"},
        {}],

     CrusherComment["propagators: momenta, mass"],
     Function[x, CrusherSetting["prop", x]] /@ ("prop" /. #),
     "\n",

     If[("userrep" /. #) =!= "userrep",
        {CrusherComment["relations among invariants (optional)"],
         Function[x, CrusherSetting["userrep", x]] /@ ("userreps" /. #),
         "\n"},
        {}],

     If[("zerotopo" /. #) =!= "zerotopo",
        {CrusherComment["vanishing topologies"],
         Function[x, CrusherSetting["zerotopo", x]] & /@ ("zerotopo" /. #),
         "\n"},
        {}],

     CrusherComment["automatic symmetrization (internal -- same sector)"],
     CrusherSetting["useintsym", "useintsym" /. #],
     CrusherSetting["usesym", "usesym" /. #],
     "extsym" /. #,
     "\n",


     CrusherComment["-- integrals --"],
     "\n",

     CrusherComment["topology masks (specify only propagators)"],
     Function[x, CrusherSetting["toptopo", x]] /@ ("toptopo" /. #),
     "\n",

     CrusherComment["starting topologies (specify only propagators)"],
     Function[x, CrusherSetting["topo", x]] /@ ("topo" /. #),
     "\n",

     CrusherComment["minimal seeds (optional, default: 1,1)"],
     CrusherSetting["minseeds", "minseeds" /. #],
     "\n",

     CrusherComment["seeds for starting topologies"],
     CrusherSetting["seeds", "seeds" /. #],
     "\n",

      CrusherComment["-- program --"],
      "\n",

      CrusherComment["operation mode"],
     CrusherSetting["adaptive", 0],
     "\n",

     CrusherComment["integrals eliminated first: PROP/SP (optional, default: SP)"];
     CrusherSetting["elim", "SP"],
     "\n",

     CrusherComment["basis used: NEG/POS (optional, default: NEG)"],
     CrusherSetting["basis", "NEG"],
     "\n",

     CrusherComment["omit insertion of subtopologies (optional, default: 0)"],
     CrusherSetting["get_master", 0],
     "\n",



     CrusherComment["-- notation --"],
     "\n",

     If[("intname" /. #) =!= "intname",
        {CrusherComment["integral name (optional, default: Int)"],
         CrusherSetting["intname", "intname" /. #],
         "\n"},
        {}],

     CrusherComment["deliminator between propagators and scalar products (optional, default: none)"],
     CrusherSetting["delim", "X"],
     "\n",

     CrusherComment["output format: 0 -- FORM, 1 -- Mathematica, 2 -- Maple (optional, default: 0)"],
     CrusherSetting["program", 1],
     "\n",



     CrusherComment["-- resources --"],
     "\n",

     If[("intname" /. #) =!= "intname",
        {CrusherComment["data directory (optional, default: ./data)"],
         CrusherSetting["dir", "./data_" <> ("intname" /. #)],
         "\n"},
        {}],

     CrusherComment["debugging/verbosity level: 0 - 3 (optional, default: 1)"],
     CrusherSetting["debug", 2];
     "\n",

     CrusherComment["number of Fermat threads"],
     CrusherSetting["threads", 12],
     "\n"

     ] &

     };


Defaults[CrusherConfig] =
{

   "elim" ->

 }

Checks[CrusherConfig] =
{};

CheckMessages[CrusherConfig] =
{};







(* overload: plural *)
CrusherConfig[
  tops_?TopologyListQ, set:SetupPattern[], opts:OptionsPattern[]] :=
  CrusherConfig[#, set, opts] & /@ tops;



(* main *)
CrusherConfig[
  top:TopologyPattern[], set:SetupPattern[], opts:OptionsPattern[]] :=
  Module[
    {m2r, f, g, trs, um, ss, zs},

    (* helper: rewrite invariants *)
    m2r = x_^i_ :> ToString[x] <> ToString[i];
    f = # //. (cs /. set) //. (rs /. set) /. m2r & ;
    (* helper: strip spaces *)
    g = StringReplace[ToString[#], " " -> ""] & ;


    trs = OptionValue[{Transformations}];

    (* TODO: check trs *)




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

    "limp", (ks /. set),

    "ext", #, f[#^2]] & /@ (ps /. set),

MapThread[
  CrusherSetting["prop", g[#1], f[#2]] & ,
        {TopologyMomentaFlows[top, set], TopologyMasses[top, set]}],

CrusherSetting["userrep", g[f[ToString[Equal @@ #, InputForm]]]] & /@ trs,

      CrusherSetting["zerotopo", #] & /@ zs,


"extsym" ->
{CrusherSetting["powsymb", {"a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9"}],
 CrusherSetting["extsymfile", "symmA"},

      OptionValue[Settings][top, set]

      ]];



*)



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
  StringJoin[
    "#", If[Head[#] === String, #, ToString[#]] & /@ {str}, "\n"];

CrusherSetting::usage = "\
CrusherSetting[<var>, <val(s)>] is a wrapper for a Crusher input \
setting, where <var> denotes the set variable and <val(s)> its \
corresponding value(s).";

(* alias: list *)
CrusherSetting[var_, a___, b_List, c___] :=
  CrusherSetting[var, a, Sequence @@ b, c];

Format[CrusherSetting[var_, vals___]] :=
  StringJoin[First[#], "=", Riffle[Rest[#], ","], "\n"] & @
  (If[Head[#] === String, #, ToString[#]] & /@ {var, vals});

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
