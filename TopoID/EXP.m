(* -- "EXP.m": Generate Diagram Mapping Configurations -------------- *)

BeginPackage[
  "TopoID`EXP`",
  {"TopoID`Common`", "TopoID`System`",  (* TODO *)

   "TopoID`Setup`",
   "TopoID`Topology`",
   "TopoID`Mapping`",

   "TopoID`Text`",
   "TopoID`FORM`"}];


ClearAll[RegConfigHead];

ClearAll[RegConfigBody, $RegConfigBodyLoadingFunction];

ClearAll[RegConfig];

ClearAll[EXPConfig];

Clear[EXPTopology];

Clear[EXPGroup];

Clear[EXPFile];

{DiagramEntry,DiagramFile, DiagramToFile};


{Maximum, Minimum, Required, Small, LoadingFunction, Transformations,
 EXPOptions};


RegConfigHead::usage = "";
RegConfigBody::usage = "";
$RegConfigBodyLoadingFunction::usage = "";
RegConfig::usage = "";

EXPConfig::usage = "\
EXPConfig[<top(s)>, [opts]] produces a string that can be used in the \
configuration file of the program \"EXP\" (by Thorsten Seidensticker) \
in order to map Feynman diagrams on the topology object(s) given by \
<top(s)>.";

EXPTopology::usage = "";
EXPGroup::usage = "";
EXPFile::usage = "";



Begin["`Private`"];

(* --- REG (by Alexey Pak) ------------------------------------------ *)

(* N.B.:
  "REG" is an abbrevation for regions.
*)

(* ---- head -------------------------------------------------------- *)

(* RegionsConfigHead                                                    TODO
arg.s: List <stp> containing rules for a valid, initialized setup.
(opt.): The only argument is optional. If no setup is given, the global
  setup $TopoIDSetup is assumed.
opt.s:
  [Required] - "masking" expression beeing required to occur in each
    diagram at least once, e.g. mh^2 (Null by default),
  [Minimum] and [Maximum] - integer minimum and maximum vector component
    values assumed for loop momenta transformations (-1 and 1 by
    default).
return: Head of an REG configuration file with all necessary statements.
notes: -1, 0 and 1 are the usually appearing coefficients in
  transformations of the loop momenta.
todo: Version incorporating asymptotic expansion.
*)

(* TODO:
- 'Rq' may have changed in future version of REG
- e.g. mh^2
- Td via option
*)

Options[RegConfigHead] =
{Maximum -> +1,       (* _Integer *)
 Minimum -> -1,       (* _Integer *)
 Required -> "mh^2",  (* _ *)
 Small -> {},         (* _ *)
 Temporary -> Null};  (* _ *)  (* TODO *)

RegConfigHead[set:SetupPattern[], opts:OptionsPattern[]] :=
    Module[{f,g,h, mx,mn, rq, sm, tp, cfg},

           (* -- helper functions -- *)

           f = "'" <> FORMForm[ToDotProduct[#, vs /. set]] <> "'" & ;
           g = "[" <> StringJoin[Riffle[f /@ #, ","]] <> "]" & ;
           h = "(" <> StringJoin[Riffle[ToString /@ #, ","]] <> ")" & ;

           (* -- options -- *)

           {mx, mn, rq, sm, tp} = OptionValue[
               {Maximum, Minimum, Required, Small, Temporary}];                    (* TODO: check options *)

           rq = rq /. False | None | Null | Off | 0 -> "";
           rq = If[Head[rq] === List, g, f][rq];

           sm = sm /. False | None | Null | Off | 0 -> {};
           sm = Flatten[{sm}];

           (* -- compile data -- *)

           cfg = StringJoin[
               "'Ps' => ", g[ps /. set],
               ",  # external momenta\n",
               "'Ks' => ", g[ks /. set],
               ",  # internal momenta\n",
               "'Xs' => ", g[xs /. set],
               ",  # parameters\n",
               "'Us' => ", g[zs /. set],
               ",  # zero-expressions\n",
               "'Sl' => ", g[sm],
               ",  # single small-scale expressions\n",
               "'Rq' => ", rq,
               ",  # 'required' mask (in at least one resulting line)\n",
               "'Td' => [ map { \"dx$_\" } 1..",
               ToString[If[Head[tp] === Integer, tp, ne /. set]],
               " ]",
               ",  # temporary symbols denoting lines\n",
               "'Mn' => limmx", h[{nv /. set, nk /. set, mn}],
               ",  # matrix of minimal reparametrization\n",
               "'Mx' => limmx", h[{nv /. set, nk /. set, mx}],
               ",  # matrix of maximal reparametrization\n",
               "'Ln' => clon", h[{mn, nv /. set}],
               ",  # minimal allowed vector components\n",
               "'Lx' => clon", h[{mx, nv /. set}],
               "   # maximal allowed vector components"];

           (* -- formatting -- *)

           cfg = TrimLines[AdjustColumns[
               cfg, Offset -> 0, RecordSeparators -> "#"]];

           (* -- complete entry -- *)

           cfg = StringJoin["# general kinematic setup\n\n",
                            "$St = {\n",
                            IndentLines[cfg, 2, " "],
                            "\n};\n"];

           Return[cfg]];

(* ---- body -------------------------------------------------------- *)

(* RegionsConfigBody                                                    TODO
arg.s: List of supplied generic topologies <gts>.
(opt.): If no <gts> is given, the global list $GenTops is used.
opt.s:
  [Flip] - switch to change the order of entries (by default the
    original topologies are listet first and then follow the transformed
    ones, with Flip -> True all transformations are stated directly
    after each original).
  [GenExt] - string for the extension of the files with the generic
    topologies,
  [GenFile] - string for the name of the file containing the generic
    topology definition(s),
  [GenPath] - string for the path where the topology files are placed,
  [Rules] - rules to be applied to the denominator definitions,
  [Transformations] - list of replacement rules for symbols which appear
    symmetrically and
  [Vectors] - list of symbols to be treated as vectors.
return: Body of an REG configuration file listing all available
  topologies with their denominators.
notes: If a [GenFile] is given, the topologies are taken to be stored in
  folds inside this file. Else a separate file for each topology is
  included by the FORM code (constructed by the topology name and values
  of [GenExt] and [GenPath]).  If a topology doesn't change at all by a
  transformation, this case is discarded.
todo: Version incorporating asymptotic expansion.
*)

(* TODO:
- Explain LoadingFunction.
- Rules in Transformations are reversed for the code, thus only symbols
  should appear.
*)

(* TODO: $RegConfigBodyMethods = ... ; *)

$RegConfigBodyLoadingFunction =
    "#include `ROOT'/top/" <> (name /. #) <> ".inc\n" & ;

Options[RegConfigBody] =
{LoadingFunction -> $RegConfigBodyLoadingFunction,
 Method -> {"Originals"},
 (*Method -> {Global`s -> -2*Global`p1.Global`p2, "Shuffle"},*)
 Transformations ->  {{}, {Global`p1 -> Global`p2, Global`p2 -> Global`p1}}};
(* TODO: *)
(* _Function: # -> _String *)
(* "Originals" | "Shuffle" | "Transformed" *)  (* TODO: global rule *)
(* {(_Rule | {___Rule})...} *)

(* -- multiple topologies -- *)

RegConfigBody[
  tops_?TopologyListQ, set:SetupPattern[], opts:OptionsPattern[]] :=
  Module[
    {me,tr, cfgs, cfg},

    (* -- options -- *)

    {me, tr} = OptionValue[{Method, Transformations}];            (* TODO: check options *)

    me = Flatten[{me}];

    (* -- transformations -- *)

    (* apply all to all, generate name *)
    cfgs = Outer[
      (Status[Console, name /. #2];
       {(name /. #2) <> If[#1 =!= {}, " " <> ToString[#1, InputForm], ""],
        RegConfigBody[#2, set, Transformations -> #1, opts]}) & ,
      tr, tops, 1];

    (* -- sorting -- *)

    If[MemberQ[me, "Shuffle"], cfgs = Transpose[cfgs]];
    cfgs = Flatten[cfgs, 1];
    If[MemberQ[me, "Transformed"], cfgs = Reverse[cfgs]];

    (* -- formatting -- *)

    (* single topology entry with comment *)
    cfgs = MapIndexed[TrimLines[JoinColumns[
      {#1[[2]], "  ## " <> ToString[#2[[1]]] <> ": " <> #1[[1]] <> "\n"},
      Offset -> 0, RecordSeparators -> ""]] & , cfgs];

    (* all topologies *)
    cfg = StringJoin[Riffle[cfgs, ",\n"]];
    cfg = TrimLines[AdjustColumns[
      cfg, Offset -> 0, RecordSeparators -> "##"]];
    cfg = StringReplace[cfg, "##" -> "#"];
    cfg = StringJoin[
      "# topologies supported by integrators\n\n",
      "$Ts = [\n",
      IndentLines[cfg, 2, " "],
      "\n];\n"];

    Return[cfg]];

(* -- single topology -- *)

RegConfigBody[top:TopologyPattern[], set:SetupPattern[], opts:OptionsPattern[]] :=
    Module[{lf,tr, fs, fc, ds, cfg},

           (* -- options -- *)

           {lf, me, tr} =
               OptionValue[{LoadingFunction, Method, Transformations}];  (* TODO: check options *)

           me = Flatten[{me}];
           me = Cases[me, _Rule];

           tr = Flatten[{tr}];

           (* -- factors -- *)

           (* apply transformations *)
           fs = facs /. top /. me /. tr;

           (* skip pure numerators *)
           fs = DeleteCases[fs, s[_] | n[_] -> _];

           (* change notation *)
           fs = FORMForm /@ # & /@ ToDotProducts[fs, vs /. set];

           (* skip numerators again *)
           fs = Select[fs, !StringMatchQ[
               #[[1]], RegularExpression["^[sn]\\d+"]] & ];

           (* -- FORM code -- *)

           (* transformation code *)
           tr = If[tr =!= {}, {FORMReplace[ToDotProducts[
               Reverse /@ tr, vs /. set]], "  "}, ""];

           (* compile all code *)
           fc = ToFORMCodeString[FORMCode[
               tr, lf[top],
               FORMComment["differing notations"],
               "  ", FORMReplace[# -> "1/" <> # & /@ (First /@ fs)],
               "  #call " <> (name /. top)]];

           (* rewrite linebreaks *)
           fc = "\"" <> StringJoin[Riffle[
               Lines[fc], "\\n\".\n\""]] <> "\"";

           (* -- formatting -- *)

           ds = StringJoin[Riffle[
               "'" <> #[[1]] <> "' => '" <> #[[2]] <> "'" &
                   /@ fs, ",\n"]];

           (* -- complete entry -- *)

           cfg = StringJoin["{'Nm' =>\n",
                            IndentLines[fc, 3, " "], ",\n",
                            " 'Ds' => {\n",
                            IndentLines[ds, 3, " "], "}\n",
                            "}"];

           Return[cfg]];

(* TODO: rules in Method -> Prolog *)

(* ---- complete ---------------------------------------------------- *)

(* RegionsConfig                                                        TODO
arg.s: List <stp> containing rules for a valid, initialized setup and
  list of supplied generic topologies <gts>.
(opt.): If no <stp> is given the global setup $TopoIDSetup is used. If
  <gts> is left out, $GenTops will be assumed.
opt.s:
  [Required] - "masking" expression beeing required to occur in each
    diagram at least once, e.g. mh^2 (Null by default),
  [Minimum] and [Maximum] - integer minimum and maximum vector component
    values assumed for loop momenta transformations (-1 and 1 by
    default).
  [Flip] - switch to change the order of entries (by default the
    original topologies are listet first and then follow the transformed
    ones, with Flip -> True all transformations are stated directly
    after each original).
  [GenFile] - string for the name of the file containing the generic
    topology definition(s),
  [GenPath] - string for the path where the topology files are placed,
  [Rules] - rules to be applied to the denominator definitions and
  [Symmetries] - list of replacement rules for symbols which appear
    symmetrically.
return: Complete REG configuration file with statements fixing the setup
  and a listing of all provided topologies.
note: The value for the [Vectors] option of RegionsConfigBody is extracted from
  the given setup <stp>.
*)

(* TODO:
- all options as comment
*)

Options[RegConfig] =
  Join[Options[RegConfigHead], Options[RegConfigBody]];

RegConfig[
  tops_?TopologyListQ, set:SetupPattern[], opts:OptionsPattern[]] :=
  StringJoin[
    RegConfigHead[
      set, FilterRules[{opts}, Options[RegConfigHead]]], "\n",
    RegConfigBody[
      tops, set, FilterRules[{opts}, Options[RegConfigBody]]]];

(* --- EXP (by Thorsten Seidensticker) ------------------------------ *)

Options[EXPConfig] =
{EXPOptions
   -> {"copy_scale", "poco_scale"},
 Externals
   -> "q",
 Internals
   -> "p",
 Masses
   -> {"qt" -> "1", "hb" -> "2", "hp" -> "2",
       Global`mt -> "1", Global`mh -> "2", _ -> "0"},
 Transformations
   -> {{out[3] -> out[4], out[4] -> out[3]}}};

(* N.B.:
- EXPOptions expects a list of strings as options, e.g.
    {"copy_scale", "poco_scale"}.
- Transformations is a list of transformation lists applied before
  generating the actual code.
*)

EXPConfig::usage = "\
EXPConfig[<top(s)>, [opts]] produces a string that can be used in the \
configuration file of the program \"EXP\" (by Thorsten Seidensticker) \
in order to map Feynman diagrams on the topology object(s) given by \
<top(s)>.";

(* only perform grouping of <dias> mapping onto the same generics *)             (* TODO *)
(*   respecting the order of generics stated in <tops> *)

EXPConfig[
  dias_?TopologyListQ, maps_?MappingListQ, tops_?TopologyListQ,
    opts:OptionsPattern[]] :=
  Module[
    {gtns, dtms, dtns, dts, res},
    (* generic topology names *)
    gtns = name /. tops;
    gtns = SortBy[Union[to /. maps /. to -> {}], Flatten[Position[#, gtns, {1}, 1]] & ];  (* preserve order defined in <tops> *)
    (* corresponding diagram mappings *)
    dtms = Function[gtn, Select[maps, (to /. #) === gtn & ]] /@ gtns;
    (* corresponding diagram names *)
    dtns = fr /. dtms /. fr -> {};
    (* corresponding diagram topologies *)
    dts = Function[dtn, Select[dias, MemberQ[dtn, name /. #] & ]] /@ dtns;
    dts = DeleteCases[dts, {}];
    (* topologies with fewer lines first *)
    dts = SortBy[#, Length[pros /. #] & ] & /@ dts;                               (* TODO: real props. in pros *)
    (* create entries, join them *)
    EXPFile @@ (EXPConfig[#, opts][[1]] & /@ dts)];



(* only perform grouping of <dias> mapping onto the same generics *)             (* TODO *)
(*   preserving the order of generics mentioned firstly in <maps> *)

EXPConfig[
  dias_?TopologyListQ, maps_?MappingListQ,
    opts:OptionsPattern[]] :=
  Module[
    {gtns, dtms, dtns, dts, res},
    (* generic topology names *)
    gtns = DeleteDuplicates[to /. maps /. to -> {}];
    (* corresponding diagram mappings *)
    dtms = Function[gtn, Select[maps, (to /. #) === gtn & ]] /@ gtns;
    (* corresponding diagram names *)
    dtns = fr /. dtms /. fr -> {};
    (* corresponding diagram topologies *)
    dts = Function[dtn, Select[dias, MemberQ[dtn, name /. #] & ]] /@ dtns;
    dts = DeleteCases[dts, {}];
    (* topologies with fewer lines first *)
    dts = SortBy[#, Length[pros /. #] & ] & /@ dts;                               (* TODO: real props. in pros *)
    (* create entries, join them *)
    EXPFile @@ (EXPConfig[#, opts][[1]] & /@ dts)];



(* TODO: delete identical entries with different names *)

EXPConfig[
  dias_?TopologyListQ, opts:OptionsPattern[]] :=
  Module[
    {ecs},
    ecs =
      (Status[Console, name /. #];
       EXPConfig[#, opts][[1, 1]]) & /@ dias;
    ecs = DeleteDuplicates[ecs, Rest[#1] === Rest[#2] & ];
    ecs = EXPFile[EXPGroup @@ ecs]];



(* todo: rename graph vertices before generating the entry -> identical entries can be identified *)

EXPConfig[
  dia:TopologyPattern["Graph"],
    opts:OptionsPattern[]] :=
  Module[
    {os,es,is,ms,ts, toq, ps,ks,mp, ne,nk,np,nm, dias, fl,fe,fi,fa,       vs, ltmp,ptmp},

    {os, es, is, ms, ts} = OptionValue[
      {EXPOptions, Externals, Internals, Masses, Transformations}];

    (* rename vertices *)
    {ltmp, ptmp} = {legs, pros} /. dia;
    ptmp = DeleteCases[ptmp, {Null, __} | {_, 0, __}];
    vs = Join[ltmp, ptmp];
    vs = Union @@ (#[[{3, 4}]] & /@ vs);
    vs = DeleteCases[vs, in[_] | out [_]];
    vs = MapIndexed[#1 -> #2[[1]] & , vs];
    vs = Join[{in[n_] -> in[n], out[n_] -> out[n]}, vs];
    ltmp = Function[l, MapAt[# /. vs & , l, {{3}, {4}}]] /@ ltmp;
    ptmp = Function[p, MapAt[# /. vs & , p, {{3}, {4}}]] /@ ptmp;

    (* clear pure scalar products *)
    toq = Topology[
      dia, legs -> ltmp, pros -> ptmp];

    (* EXP options *)
    (*os = StringJoin["," <> # & /@ os];*)
    os = Flatten[{os}];

    (* mass assignments *)
    ms = Join[ms, {0 -> "0",  _ -> "x"}];                                       (* TODO: use setup?! *)

    (* apply transformations *)
    (*dias = Prepend[toq /. # & /@ ts, toq];*)
    dias = toq /. # & /@ ts;

    (* mass on each line -> mass pattern *)
    ma = First /@ (pros /. toq);
    mp = # /. ms & /@ ma;
    ma = DeleteCases[Union[ma], 0];

    (* external, internal momenta *)
    {ps, ks} = {legs, pros} /. toq;
    {ps, ks} = Variables[#[[2]] & /@ #] & /@ {ps, ks};
    ks = Complement[ks, ps];

    (* relevant numbers *)
    ne = Length[pros /. toq];
    nk = Length[ks];
    np = Length[legs /. toq] - 1;
    (*nm = Length[Union[Last /@ ms]] - 2;*)  (* account for "0", "x" assignments *)
    nm = Length[Union[DeleteCases[Last /@ ms, "0" | "x"]]];  (* account for "0", "x" assignments *)
    (*{ne, nk, np, nm} = ToString /@ {ne, nk, np, nm};*)

    (* -- compile entries -- *)

    (* helper: line formatting *)
    fl = StringJoin[
      "( ", #1, ToString[#2], " : ",
      ToString[#3[[1]]], ", ", ToString[#3[[2]]], " )"] & ;

    fl = {#1 <> ToString[#2], #3[[1]], #3[[2]]} & ;

    (* helper: external lines *)
    fe[ls_] := Module[
      {va, ve, n,vt, mn,mx, vo, lo, vp, li, vi},
      (* all vertex indices *)
      va = Union @@ (#[[{3, 4}]] & /@ ls);
      (* external vertices *)
      ve = Cases[va, (in | out)[_]];
      (* highest incoming or outgoing *)
      vt = ve /. {in[n_] -> -n, out[n_] -> n};
      {mn, mx} = {Min[vt], Max[vt]};
      vo = If[mx > 0, out[mx], in[-mn]];
      (* corresponding outgoing leg *)
      lo = First[Cases[ls, {__, vo, ___}]];
      (* opposite internal vertex *)
      vp = First[DeleteCases[lo[[{3, 4}]], vo]];
      (* remaining incoming legs *)
      li = DeleteCases[ls, lo];
      (* their internal vertices *)
      vi = First[DeleteCases[#[[{3, 4}]], (in | out)[_]]] & /@ li;
      (* apply notation *)
      MapIndexed[fl[es, #2[[1]], {#1, vp}] & , vi]
      (*StringJoin[MapIndexed[fl[es, #2[[1]], {#1, vp}] & , vi]]*)];

    (* helper: internal lines *)
    fi = MapIndexed[fl[is, #2[[1]], #1[[{3, 4}]]] & , #] & ;

    (* helper: complete entry *)
    fa = StringJoin[
      (* A: name, options *)
      "{ ", name /. toq, os, "; ",
      (* B, C, D, E: numbers of edges, loops, legs, masses *)
      ne, "; ", nk, "; ", np, "; ", nm, "; ",
      (* F: scales (TODO) *)
      "; ",
      (* H: line specifications *)
      fe[legs /. #], fi[pros /. #], "; ",
      (* I: mass pattern *)
      StringJoin[mp], " }\n"] & ;

    fa = EXPTopology[
      name /. toq, os, {ne, nk, np, nm, Null}, Join[fe[legs /. #], fi[pros /. #]] , mp] & ;

    dias = DeleteDuplicates[fa /@ dias];

    EXPFile[EXPGroup @@ dias]];

EXPConfig[___] :=
  (Message[EXPConfig::usage];
   Abort[]);

Format[EXPTopology[
  dn_String, os:{___String}, ps:_List,
  gr:{{_String, _Integer, _Integer}...}, mp:{___String}]] :=
StringJoin[
  "{", dn, ",", Riffle[os, ","], ";", Riffle[
    ToString /@ (ps /. Null -> ""), ";"], ";",
  StringJoin[
    "(", #[[1]], ":", ToString[#[[2]]], ",", ToString[#[[3]]], ")"
  ] & /@ gr, ";", mp, "}\n"];

Format[EXPGroup[ts___]] :=
 StringJoin[ToString /@ {ts}];

Format[EXPFile[gs___]] :=
 StringJoin[Riffle[DeleteCases[ToString /@ {gs}, ""], "\n"]];

(* N.B.:

- Entries in <top_sel_file>:
    { A [,<options>]; B; C; D; E; F; H; I [; I] },
  with:
    A -- name of the topology,
    B -- number of lines,
    C -- number of loops,
    D -- number of external momenta,
    E -- number of masses,
    F -- scales information,
    H -- simplified line specifications,
    I -- mass distribution
  where H:
    ( W : Y, Z ),
  and:
    W -- name of the momentum (q1, p1, p2, ...),
    Y, Z -- numbers of vertices (the momentum flows from Y to Z).

- Convention: only one outgoing momentum, all others incoming.

*)

(* TODO:
option for styling/compactness?
*)

(* --- DiagramToFile ------------------------------------------------ *)

Options[DiagramToFile] =
{Externals -> "q",
 Internals -> "p",
 Naming ->
 {Global`s12 + Global`s13 + Global`s23 -> 0,
  fr[0] | to[0] -> "",
  Global`mt -> "mt",
  0 -> "gl"},
 Transformations -> {in[i_] -> out[i], out[i_] -> in[i]}};

(* main: plural *)
DiagramToFile[
  tops_?TopologyListQ, set:SetupPattern[],
  opts:OptionsPattern[]] :=
  DiagramFile @@ MapIndexed[
    Prepend[#1, First[#2]] & ,
    DiagramToFile[#, set, opts] & /@ tops];

(* main *)
DiagramToFile[
  top:TopologyPattern["Graph"], set:SetupPattern[],
    opts:OptionsPattern[]] :=
  Module[
    {es,is,nrs,trs, lgs,prs, ins, ni,no},

    {es,is, nrs,trs} = OptionValue[{Externals,Internals, Naming,Transformations}];

    nrs = Join[nrs, {fr[l_] -> l, to[l_] -> l}];

    {lgs, prs} = ({legs, pros} /. top) /. trs;

    {ni, no} = Count[lgs, #[_], {2}] & /@ {in, out};

    lgs = MapIndexed[{es <> ToString[#2[[1]]], Last[DeleteCases[#1, in[_] | out[_], {1}]], #1[[1]] //. nrs} & , lgs];

    prs = MapIndexed[{is <> ToString[#2[[1]]], #1[[3]], #1[[4]], fr[#1[[1]]] //. nrs, to[#1[[1]]] //. nrs} & , prs];

    ins = inds /. top;
    If[ins =!= inds,
       pos = Position[ins, 0, {1}];
       {ins, prs} = Delete[#, pos] & /@ {ins, prs};
       ins = ins /. {i_?Positive :> "d" <> ToString[i], i_?Negative :> "n" <> ToString[-i]};
       prs = MapThread[Join[Take[#1, 3], {#1[[4]] <> #2, #1[[5]] <> #2}] & , {prs, ins}]];

    DiagramEntry[
      {Length[pros /. top], nk /. set, ni, no},
      lgs, prs]];

(* DiagramEntry *)

Format[DiagramEntry[
  dn_Integer,
  {np_Integer, nl_Integer, ni_Integer, no_Integer},
  ls:{{_String, _Integer, _String}...},
  ps:{{_String, _Integer, _Integer, _String, _String}...}]] :=
StringJoin[
"{\n",
"diagram            ", ToString[dn], "\n",
"pre_factor         +1\n",
"\n",
"number_propagators ", ToString[np], "\n",
"number_loops       ", ToString[nl], "\n",
"number_legs_in     ", ToString[ni], "\n",
"number_legs_out    ", ToString[no], "\n",
"\n\n",
StringJoin["external_leg       ", #[[1]], "|", ToString[#[[2]]], "|", #[[3]], "\n"] & /@ ls,
"\n\n",
StringJoin["momentum           ", #[[1]], "|", ToString[#[[2]]], ",", ToString[#[[3]]], "|", #[[4]], ",", #[[5]], "\n"] & /@ ps,
"}\n"];

(* DiagramFile *)

Format[DiagramFile[des___DiagramEntry]] :=
StringJoin["\n", ToString[#] <> "\n\n" & /@ {des}];

(* ------------------------------------------------------------------ *)

(* TODO:
- Needs "text.m", "form.m".
- Reg...:
  Global transformation rules, ala  Notation -> {___Rule} | _Rule ?
  Version for tops. w/ embedded setup?
- RegionsConfig...
- samples...
*)






(* ------------------------------------------------------------------ *)

End[];

EndPackage[];

(* ------------------------------------------------------------------ *)
