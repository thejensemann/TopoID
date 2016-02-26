(* -- "FeynMF.m": Graph Conversion to feynMF ------------------------ *)

BeginPackage[
  "TopoID`FeynMF`",
  {"TopoID`Common`",  (* TODO: needed? *)
   "TopoID`Topology`",
   "TopoID`Text`"}];


ClearAll[GraphToFeynMF, ToFeynMF,GraphsToFeynMF];

GraphToFeynMF::usage = "";
ToFeynMF::usage = "";
GraphsToFeynMF::usage = "";










Begin["`Private`"];

(* --- single diagrams ---------------------------------------------- *)

(* ---- defaults ---------------------------------------------------- *)

$FeynMFMargins = { { 10, 10 }, { 20, 10 } } ;

{$FeynMFWidth, $FeynMFHeight} = { 100, 50 } ;

$FeynMFGraphRules = { _ -> "\\fmfstyle\n" } ;

$FeynMFEdgeLabeling = "$" <> StringReplace[
    ToString[Expand[#[[2]]], InputForm],
    {RegularExpression["(\\w+)\[?(\\d+)\]?"] -> "$1_{$2}",
     " " -> "", "*" -> ""}] <> "$" & ;

$FeynMFVertexLabeling = "$" <> StringReplace[
    ToString[#, InputForm],
    {RegularExpression["in\[(\\d+)\]"] -> "i_$1",
     RegularExpression["out\[(\\d+)\]"] -> "o_$1"}] <> "$" & ;

$FeynMFEdgeStyle =        (* -- w/o colour -- *)
{ "gh" -> "dots_arrow",   (* labels -> style (diagrams) *)
  "gl" -> "curly",
  "hb" -> "dashes",
  "qd" -> "plain_arrow",
  "Qd" -> "plain_arrow",
  "qu" -> "plain_arrow",
  "Qu" -> "plain_arrow",
  "si" -> "dots",
  Global`mh -> "dbl_plain",      (* masses -> style (topologies and masters) *)
  (*Global`mh -> "plain,width=thick"*)
  0 -> "plain",
  _Symbol -> "plain",     (* defaults *)
  _ -> "plain" } ;

$FeynMFEdgeStyle =                               (* -- w/ colour -- *)
{ "gh" -> "dots_arrow,fore=(0.93,,0.51,,0.93)",
  "gl" -> "curly,fore=(1.0,,0.0,,0.0)",
  "hb" -> "dashes,fore=(0.0,,0.0,,0.0)",
  "qd" -> "plain_arrow,fore=(0.0,,1.0,,1.0)",
  "Qd" -> "plain_arrow,fore=(0.0,,1.0,,1.0)",
  "qu" -> "plain_arrow,fore=(0.0,,0.0,,0.5)",
  "Qu" -> "plain_arrow,fore=(0.0,,0.0,,0.5)",
  "si" -> "dots,fore=(0.75,,0.75,,0.75)",
  Global`mh -> "dbl_plain,fore=(0.0,,0.0,,0.0)",
  (*Global`mh -> "plain,fore=(0.0,,0.0,,0.0),width=thick",*)
  0 -> "plain,fore=(0.0,,0.0,,0.0)",
  _Symbol -> "plain,fore=(0.0,,0.0,,0.0)",
  _ -> "plain,fore=(0.0,,0.0,,0.0)" } ;

$FeynMFVertexStyle =
{ in -> ("\\fmfleft{" <> StringJoin[Riffle[#, ","]] <> "}\n" & ),
  out -> ("\\fmfright{" <> StringJoin[Riffle[#, ","]] <> "}\n" & ),
  _ -> "d.sh=circle,d.f=full,d.si=2thick" } ;

(* ---- definition -------------------------------------------------- *)

(*
PageWidth
-> Infinity/All/Full, _Integer
ImageMargins
-> Automatic, <number>, {{<left>, <right>}, {<bottom>, <top>}}
ImageSize
-> Automatic, _Integer, {_Integer, _Integer}
PlotLabel
-> False/_, Automatic/On/True, _String
GraphRules
-> {Rule[_, _String]...}
EdgeLabeling
-> False/_, Automatic/Full/True/On, _Function
VertexLabeling
-> False/_, Automatic/Full/True/On, _Function
EdgeStyle
-> {Rule[_, _String]...}
VertexStyle
-> {Rule[_, _String]...}
*)

Options[GraphToFeynMF] =
{ PageWidth -> Infinity,
  ImageMargins -> Automatic,
  ImageSize -> Automatic,
  PlotLabel -> False,
  GraphRules -> Automatic,
  EdgeLabeling -> False,
  VertexLabeling -> False,
  EdgeStyle -> Automatic,
  VertexStyle -> Automatic };

GraphToFeynMF[obj:DiagramPattern[],
  opts:OptionsPattern[]] :=
    Module[{ff, im,is,pl,gr,el,vl,es,vs, iw, lg, fe,fv, vi,vo,vt, tex,
            pros2},                                                     (* TODO *)

           (* translate vertices *)
           ff = # /. {in[i_] :> "i" <> ToString[i],
                      out[o_] :> "o" <> ToString[o],
                      v_Integer :> "v" <> ToString[v],
                      x_ :> "<" <> ToString[x] <> ">"} & ;

           (* -- handle options -- *)

           {im,is, pl, gr, el,vl, es,vs} = OptionValue[
               {ImageMargins,ImageSize, PlotLabel, GraphRules,
                EdgeLabeling,VertexLabeling, EdgeStyle,VertexStyle}];

           (* image margins *)
           If[im === Automatic || !NumberQ[im] &&
                  !(MatchQ[im, {{_, _}, {_, _}}] && And @@ NumberQ
                    /@ Flatten[im]), im = $FeynMFMargins];
           If[NumberQ[im], im = {{im, im}, {im, im}}];
           im = Flatten[im];

           (* image size *)
           If[is === Automatic, is = {Automatic, Automatic}];
           If[MatchQ[is, {Automatic, _}],
              is = {$FeynMFWidth, Last[is]}];
           If[MatchQ[is, {_, Automatic}],
              is = {First[is], $FeynMFHeight}];
           If[!MatchQ[is, {_Integer, _Integer}],
              is = {$FeynMFWidth, $FeynMFHeight}];

           (* plot label *)
           If[MemberQ[{Automatic, On, True}, pl], pl = name /. obj];
           If[!StringQ[pl], pl = ""];

           (* graph rules *)
           (*If[Head[gr] === Rule, gr = {gr}];*)
           If[!MatchQ[gr, {Rule[_, _String]..}],
              gr = $FeynMFGraphRules];

           (* edge, vertex labeling *)
           If[MemberQ[{Automatic, Full, True, On}, el],
              el = $FeynMFEdgeLabeling];
           If[Head[el] =!= Function, el = False];
           If[MemberQ[{Automatic, Full, True, On}, vl],
              vl = $FeynMFVertexLabeling];
           If[Head[vl] =!= Function, vl = False];

           (* edge, vertex style *)
           (*If[Head[es] === Rule, es = {es}];*)
           If[!MatchQ[es, {Rule[_, _String]..}],
              es = $FeynMFEdgeStyle];
           (*If[Head[vs] === Rule, vs = {vs}];*)
           If[!MatchQ[vs, {Rule[_, _String]..}],
              vs = $FeynMFVertexStyle];

           (* -- handle argument -- *)

           (* dimensions *)
           iw = ToString[is[[1]] + im[[1]] + im[[2]]];
           {im, is} = {ToString /@ im, ToString /@ is};

           (* plot label *)
           If[pl =!= "", pl = "{\\tt " <> pl <> "}\n", ""];

           (* labeling flag *)
           lg = el =!= False || vl =!= False;

           (* external vertices *)
           {vi, vo} = Union /@ Cases[Flatten[legs /. obj], #] &
               /@ {in[_], out[_]};

           (* internal vertices *)
           vt = Union[Flatten[
               #[[{3, 4}]] & /@ Join[legs /. obj, pros /. obj]]];

           (* edge drawing function *)
           fe = ("\\fmf{" <> (#[[1]] /. es) <>
                 If[Head[el] === Function, ",label=" <> el[#], ""] <>
                 "}{" <> ff[#[[3]]] <> "," <> ff[#[[4]]] <> "}\n") & ;

           (* vertex drawing function *)
           fv = ("\\fmfv{" <> (# /. vs) <>
                 If[Head[vl] === Function, ",label=" <> vl[#], ""] <>
                 "}{" <> ff[#] <> "}\n") & ;

           pros2 = MapThread[{#1[[1]], #2[[1]], #1[[3]], #1[[4]]} & , {pros /. obj, facs /. obj}];  (* TODO *)
           pros2 = DeleteCases[pros2, {Null, __}];

           (* compile LaTeX code *)
           tex = ("% -- " <> (name /. obj) <> " --\n" <>
                  "\\parbox{" <> iw <> "pt}{\n" <>
                  IndentLines[
                      ("\\centering\n" <> pl <>
                       "\\fmfframe(" <> im[[1]] <> "," <> im[[4]]
                       <> ")(" <> im[[2]] <> "," <> im[[3]] <> "){\n" <>
                       IndentLines[
                           ("\\begin{fmfgraph" <> If[lg, "*", ""] <> "}"
                            <> "(" <> is[[1]] <> "," <> is[[2]]
                            <> ")\n" <>
                            IndentLines[
                                ((name /. obj /. gr) <>
                                 "% external vertices\n" <>
                                 ((in /. vs) @ (ff /@ vi)) <>
                                 ((out /. vs) @ (ff /@ vo)) <>
                                 "% external lines\n" <>
                                 StringJoin[fe /@ (legs /. obj)] <>
                                 "% internal lines\n" <>
                                 StringJoin[fe /@ (pros2 /. obj)] <>
                                 If[vl =!= False, "% all vertices\n" <>
                                        StringJoin[fv /@ vt], ""]),
                                2, " "] <>
                            "\\end{fmfgraph" <> If[lg, "*", ""] <>
                            "}\n"),
                           2, " "]) <> "}\n",
                      2, " "] <> "}\n");

           (* split lines *)
           tex = SplitLines[
               tex, IndentCharacter -> " ", LineIndent -> 2,
               PageWidth -> OptionValue[PageWidth],
               SplitFunction -> SplitLinesAfter,
               WordSeparators -> "\,"];

           Return[tex]];

(* --- multiple diagrams -------------------------------------------- *)

(* ---- defaults ---------------------------------------------------- *)

$FeynMFHeader = "\
\\documentclass{article}
\\usepackage{feynmp}\n
\\def\\fmfstyle{
  \\fmfset{thin}{0.5pt}       % 1pt
  \\fmfset{thick}{1.5thin}    % 1.5thin
  \\fmfset{arrow_ang}{20}     % 15
  \\fmfset{arrow_len}{2.0mm}  % 4mm
  \\fmfset{curly_len}{1.5mm}  % 3mm
  \\fmfset{dash_len}{1.5mm}   % 3mm
  \\fmfset{dot_len}{1.0mm}    % 2mm
  \\fmfset{dot_size}{1.0mm}   % 4thick
}\n
\\begin{document}
\\centering\n
" <> SeparatorLine[50, "% ", "-", ""] <> "\n" ;

$FeynMFFooter = "\
\n" <> SeparatorLine[50, "% ", "-", ""] <> "
\\end{document}\n" ;

$FeynMFPath = "dias/diagram_file" ;
(*$FeynMFPath = "<PATH_TO_MP_FILE>" ;*)

$FeynMFSeparators = { "%\n", "\n" } ;

(* ---- definition -------------------------------------------------- *)

Options[GraphToFeynMF] = Sort[Join[
    Options[GraphToFeynMF],
    { Header -> $FeynMFHeader,              (* _String *)
      Footer -> $FeynMFFooter,              (* _String *)
      Path -> $FeynMFPath,                  (* _String *)
      Separators -> $FeynMFSeparators }]];  (* _String, {___String} *)

GraphToFeynMF[objs:{DiagramPattern[]...},
  opts:OptionsPattern[]] :=
    SplitLines[
        (OptionValue[Header] <>
         "\\begin{fmffile}{" <> OptionValue[Path] <> "}\n\n" <>
         IndentLines[StringJoin[Riffle[
             GraphToFeynMF[#, PageWidth -> Infinity, opts] & /@ objs,
             OptionValue[Separators]]], 2, " "] <> "\n" <>
         "\\end{fmffile}\n" <>
         OptionValue[Footer]),
        IndentCharacter -> " ", LineIndent -> 2,
        PageWidth -> OptionValue[PageWidth],
        SplitFunction -> SplitLinesAfter,
        WordSeparators -> "\,"];

(* aliases *)
ToFeynMF = GraphToFeynMF;
GraphsToFeynMF = GraphToFeynMF;

(* ------------------------------------------------------------------ *)

(* TODO:
- Needs "text.m".
- Documentation.
- Error messages.
- GraphToFeynMF -> DiagramToFeynMF, and so on...

- FeynMF short reference
- tension anyone?
*)




(* ------------------------------------------------------------------ *)

End[];

EndPackage[];

(* ------------------------------------------------------------------ *)
