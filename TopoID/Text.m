(* -- "text.m": Various Operations on Text -------------------------- *)

BeginPackage["TopoID`Text`"];

(* provided functions:
- SeparatorLine -- draw a separator line,
- TrimLines -- remove trailing whitespace from lines,
- IndentLines -- indent lines in a specified way,
- TrimText -- remove leading and trailing newlines from text,
- SqueezeText -- remove repeating newlines from text,
- Lines -- list of lines from a string,
- JoinColumns -- join text as columns,
- AdjustColumns -- adjust columns in text, and
- SplitLines -- insert line breaks into a string.
*)

ClearAll[SeparatorLine];
ClearAll[TrimLines];
ClearAll[IndentLines];
ClearAll[TrimText];
ClearAll[SqueezeText];
ClearAll[Lines];
ClearAll[JoinColumns];
ClearAll[AdjustColumns];
ClearAll[SplitLines, SplitLinesSimple,SplitLinesBefore,SplitLinesAfter];

SeparatorLine::usage = "";
TrimLines::usage = "";
IndentLines::usage = "";
TrimText::usage = "";
SqueezeText::usage = "";
Lines::usage = "";
JoinColumns::usage = "";
AdjustColumns::usage = "";
SplitLines::usage = "";
SplitLinesSimple::usage = "";
SplitLinesBefore::usage = "";
SplitLinesAfter::usage = "";


IndentCharacter::usage = "";
SplitFunction::usage = "";

Begin["`Private`"];

(* --- SeparatorLine ------------------------------------------------ *)

SeparatorLine[w_Integer, b_String:"# ", f_String:"-", e_String:""] :=
    b <> StringJoin[Table[f, {w - StringLength[b <> e]}]] <> e <> "\n";

(* --- TrimLines ---------------------------------------------------- *)

(* ---- description ------------------------------------------------- *)

(* TrimLines[<x>]
arguments:
  <x> -- string, list, sequence or any expression.
output:
  Error message in case no string appears as single argument <x>.
return:
  <x> with whitespace removed from strings contained.
attributes:
  Listable.
tags:
  arguments.
version:
  2013-09-01 (rewrite).
notes:
- Only spaces and tabs are considered whitespace.
*)

(* ---- definition -------------------------------------------------- *)

Attributes[TrimLines] =
{Listable};

TrimLines::arguments = "\
Only strings are accepted as arguments.
  `1`
is no valid input.";

TrimLines[s_String] :=
    StringReplace[s, RegularExpression["(?m)[ \t]+$"] -> ""];

TrimLines[x___] :=
    TrimLines[{x}];

TrimLines[x_] :=
    (Message[TrimLines::arguments, x];
     TrimLines[ToString[x, InputForm]]);

(* ---- examples ---------------------------------------------------- *)

(* sample call:
TrimLines["AAA   \n BBB  \n  CCC "]
*)

(* sample output:
"AAA\n BBB\n  CCC"
*)

(* --- IndentLines -------------------------------------------------- *)

(* ---- description ------------------------------------------------- *)

(* IndentLines[<s>, [<i>], [<c>]]
arguments:
  <s> -- string containing several lines.
(optional):
  [<i>] -- integer amount of [<c>] giving the indentation width, and
  [<c>] -- string being used [<i>] times as indentation.
output:
  Error message in case the arguments were no specified correctly.
return:
  String with each line in <s> indented by [<c>] repeated [<i>] times.
attributes:
  Listable.
tags:
  arguments.
version:
  2013-09-01 (rewrite).
notes:
- Empty lines are not indented.
- Default values of [<i>] and [<c>] are 2 and " " respectively.
- Exclude lines beginning with an asterisk ("*") by adding "[^\\*]" into
  the parentheses (for FORM comments).
*)

(* ---- definition -------------------------------------------------- *)

Attributes[IndentLines] =
{Listable};

IndentLines::arguments = "\
Arguments are not specified correctly:
  `1`.
The function is defined by:
  IndentLines[s_String, i_Integer:2, c_String:\" \"].";

IndentLines[s_String, i_Integer:2, c_String:" "] :=
    StringReplace[s, RegularExpression["(?m)^(.+)$"] ->
                      StringJoin[Table[c, {i}]] <> "$1"];

IndentLines[x___] :=
    (Message[IndentLines::arguments, {x}]; x);

(* ---- examples ---------------------------------------------------- *)

(* sample call:
IndentLines["AAA\nBBB\nCCC"]
*)

(* sample output:
  AAA
  BBB
  CCC
*)

(* --- TrimText ----------------------------------------------------- *)

(* ---- description ------------------------------------------------- *)

(* TrimText[<x>]
arguments:
  <x> -- string, list, sequence or any expression with newlines that
    shall be adjusted.
output:
  Error message in case no string appears as single argument <x>.
return:
  <x> stripped off leading and trailing newlines.
attributes:
  Listable.
tags:
  arguments.
version:
  2013-09-01 (rewrite).
notes:
- All empty lines at the beginning and at the end are deleted, inbetween
  newlines are left untouched.
*)

(* ---- definition -------------------------------------------------- *)

Attributes[TrimText] =
{Listable};

TrimText::arguments = "\
Only strings are accepted as arguments.
  `1`
is no valid input.";

TrimText[s_String] :=
    StringTrim[s, RegularExpression["^\n+|\n+$"]];                            (* TODO: check StringTrim *)

TrimText[x___] :=
    TrimText[{x}];

TrimText[x_] :=
    (Message[TrimText::arguments, x];
     TrimText[ToString[x, InputForm]]);

(* ---- examples ---------------------------------------------------- *)

(* sample call:
TrimText["\n\nAAA\n\n\nBBB\nCCC\n\n"]
*)

(* sample output:
"AAA\n\n\nBBB\nCCC"
*)

(* --- SqueezeText -------------------------------------------------- *)

(* ---- description ------------------------------------------------- *)

(* SqueezeText[<x>]
arguments:
  <x> -- string, list, sequence or any expression.
output:
  Error message in case no string appears as single argument <x>.
return:
  <x> with multiple repeated newlines combined into single ones.
attributes:
  Listable.
tags:
  arguments.
version:
  2013-09-01 (rewrite).
*)

(* ---- definition -------------------------------------------------- *)

Attributes[SqueezeText] =
{Listable};

SqueezeText::arguments = "\
Only strings are accepted as arguments.
  `1`
is no valid input.";

SqueezeText[s_String] :=
    StringReplace[s, RegularExpression["\n\n+"] -> "\n"];

SqueezeText[x___] :=
    SqueezeText[{x}];

SqueezeText[x_] :=
    (Message[SqueezeText::arguments, x];
     SqueezeText[ToString[x, InputForm]]);

(* ---- examples ---------------------------------------------------- *)

(* sample call:
SqueezeText["\n\nAAA\n\n\nBBB\nCCC\n\n"]
*)

(* sample output:
"\nAAA\nBBB\nCCC\n"
*)

(* --- Lines -------------------------------------------------------- *)

(* ---- description ------------------------------------------------- *)

(* Lines[<x>]
arguments:
  <x> -- string, list, sequence or any expression with line substrings
    to be extracted.
output:
  Error message in case no string appears as single argument <x>.
return:
  List of strings representing lines in <x>.
attributes:
  Listable.
tags:
  arguments.
version:
  2013-09-01 (rewrite).
notes:
- In contrast to StringSplit[s_String, "\n"] all empty lines are
  included (i.e. beginning, ending and multiple empty lines).
*)

(* ---- definition -------------------------------------------------- *)

Attributes[Lines] =
{Listable};

Lines::arguments = "\
Only strings are accepted as arguments.
  `1`
is no valid input.";

Lines[s_String] :=
    StringCases[s, RegularExpression["(?m)^.*$"]];

Lines[x___] :=
    Lines[{x}];

Lines[x_] :=
    (Message[Lines::arguments, x];
     Lines[ToString[x, InputForm]]);

(* ---- examples ---------------------------------------------------- *)

(* sample call:
Lines["AAA\nBBB\nCCC"]
*)

(* sample output:
{"AAA", "BBB", "CCC"}
*)

(* --- JoinColumns -------------------------------------------------- *)

(* ---- description ------------------------------------------------- *)

(* JoinColumns[<s>, [opts]]
arguments:
  <s> -- list or sequence of strings to be joined as columns.
options:
  [Offset] -- integer indicating the number of spaces around each
    separator in [RecordSeparators], and
  [RecordSeparators] -- string or list of strings used for separating
    the columns <s>.
output:
  Error messages in case option values are not specified correctly.
return:
  String resulting from joining the strings in <s> as if they were
  columns.  Columns are separated by the strings in [RecordSeparators]
  padded left and right with [Offset] spaces.
dependencies:
  Lines.
tags:
  nointeger, nostrings.
version:
  2013-09-02 (rewrite).
description:
  The number of lines in each column is adjusted to the maximum number
  of lines.  As is the number of characters in each line in order to
  achieve a nice readable layout.
notes:
- Some symbolic values are valid option values for [Offset].
todo:
- [Offset]: left and rigth offsets (_Integer or {_Integer, _Integer})?
*)

(* ---- definition -------------------------------------------------- *)

Options[JoinColumns] =
{Offset -> 1,               (* _Integer, <...> *)
 RecordSeparators -> "#"};  (* _String, {___String} *)

JoinColumns::nointeger = "\
An integer is expected as option value.
  `1`
is no valid option assignment.";

JoinColumns::nostrings = "\
A string or list of strings is expected as option value.
  `1`
is no valid option assignment.";

JoinColumns[s:{__String}, opts:OptionsPattern[]] :=
    Module[{fl,fc, os,rs, r, m},

           (* fill #1 up with " " to length of #2 *)
           fl = #1 <> StringJoin[
               Table[" ", {#2 - StringLength[#1]}]] & ;
           (* fill each line in a column up to maximum length *)
           fc = Function[c, fl[#, Max[StringLength /@ c]] & /@ c];

           (* -- handle options -- *)

           {os, rs} = OptionValue[{Offset, RecordSeparators}];
           os = os /. {On -> 1, Off -> 0, True -> 1, False -> 0,
                       Automatic -> 1, Full -> 1, Null -> 0};
           rs = Flatten[{rs}];

           (* check offset *)
           If[!IntegerQ[os],
              Message[JoinColumns::nointeger, Offset -> os];
              os = 1];

           (* check separators *)
           If[!(And @@ StringQ /@ rs),
              Message[JoinColumns::nostrings, RecordSeparators -> rs];
              rs = {"#"}];

           (* apply offset on both sides *)
           rs = StringJoin[Riffle[Table[" ", {2}, {os}], #]] & /@ rs;

           (* -- handle argument -- *)

           (* split into lines *)
           r = Lines /@ s;
           (* maximum number *)
           m = Max[Length /@ r];

           (* fill up with empty lines *)
           r = Join[#, Table["", {m - Length[#]}]] & /@ r;
           (* duplicate separators *)
           rs = Table[#, {m}] & /@ rs;

           (* fill columns *)
           r = fc /@ r;

           (* insert separators *)
           r = Transpose[Riffle[r, rs]];
           (* insert newlines *)
           r = StringJoin[Riffle[r, "\n"]];

           Return[r]];

JoinColumns[s:__String, opts:OptionsPattern[]] :=
    JoinColumns[{s}, opts];

(* ---- examples ---------------------------------------------------- *)

(* sample call:
JoinColumns[{"AAA\n BBB\n  CCC\n   DDD", "1\n2\n3", "\nb\n\nd"}]
*)

(* sample output:
AAA    # 1 #
 BBB   # 2 # b
  CCC  # 3 #
   DDD #   # d
*)

(* --- AdjustColumns ------------------------------------------------ *)

(* ---- description ------------------------------------------------- *)

(* AdjustColumns[<s>, [opts]]
  <s> -- string with columns to be adjusted and [RecordSeparators]
    separating them.
options:
  [Offset] -- integer indicating the number of spaces around each
    separator in [RecordSeparators], and
  [RecordSeparators] -- string or list of strings used for separating
    the columns <s>.
output:
  Error messages in case option values are not specified correctly.
return:
  String <s> with the columns separated by [RecordSeparators] adjusted
  or realigned.
attributes:
  Listable.
dependencies:
  Lines, JoinColumns.
tags:
  nointeger, nostrings.
version:
  2013-09-02 (rewrite).
description:
  First, <s> is split into rows via Lines and then by [RecordSeparators]
  into columns.  Thereafter, empty columns are inserted in each row at
  the required positions.  Then, the total number of columns is adjusted
  to the maximum in each row.  Last, the table is rearranged into
  strings for the columns and these are subsequently joined via
  JoinColumns.
notes:
- Some symbolic values are valid option values for [Offset].
todo:
- [Offset]: left and rigth offsets (_Integer or {_Integer, _Integer})?
*)

(* ---- definition -------------------------------------------------- *)

Options[AdjustColumns] =
{Offset -> 1,               (* _Integer, <...> *)
 RecordSeparators -> "#"};  (* _String, {___String} *)

Attributes[AdjustColumns] =
{Listable};

AdjustColumns::nointeger = "\
An integer is expected as option value.
  `1`
is no valid option assignment.";

AdjustColumns::nostrings = "\
A string or list of strings is expected as option value.
  `1`
is no valid option assignment.";

AdjustColumns[s_String, opts:OptionsPattern[]] :=
    Module[{fs, os,rs, r, m},

           (* -- helper: insert empty columns -- *)

           (* to be applied for each separator rule *)
           fs[t_, r_] :=
               Module[{m, n},
                      (* maximum position of first occurrence *)
                      m = Position[#, Last[r], 1, 1] & /@ t;
                      m = Max[Flatten[m]];
                      ((* find right position to insert empty strings *)
                       n = Flatten[Position[#, Last[r], 1, 1]];
                       (* check against maximum and insert *)
                       If[Length[n] > 0 && First[n] < m,
                          Flatten[Insert[
                              #, Table["", {m - First[n]}], First[n]]],
                          #]) & /@ t];

           (* -- handle options -- *)

           {os, rs} = OptionValue[{Offset, RecordSeparators}];
           os = os /. {On -> 1, Off -> 0, True -> 1, False -> 0,
                       Automatic -> 1, Full -> 1, Null -> 0};
           rs = Flatten[{rs}];

           (* check offset *)
           If[!IntegerQ[os],
              Message[AdjustColumns::nointeger, Offset -> os];
              os = 1];

           (* check separators *)
           If[!(And @@ StringQ /@ rs),
              Message[AdjustColumns::nostrings, RecordSeparators -> rs];
              rs = {"#"}];

           (* apply offset on both sides *)
           rs = (# -> StringJoin[Riffle[Table[" ", {2}, {os}], #]]) &
               /@ rs;

           (* -- handle argument -- *)

           (* split into rows and columns *)
           r = StringSplit[Lines[s], rs];
           (* apply helper for each separator *)
           r = Fold[fs, r, rs];

           (* maximum number of columns *)
           m = Max[Length /@ r];
           (* adjust each row to maximum number *)
           r = Join[#, Table["", {m - Length[#]}]] & /@ r;

           (* combine row sets into column strings *)
           r = StringJoin[Riffle[#, "\n"]] & /@ Transpose[r];
           (* join strings as columns *)
           r = JoinColumns[r, Offset -> 0, RecordSeparators -> ""];

           Return[r]];

(* ---- examples ---------------------------------------------------- *)

(* sample call:
AdjustColumns["AAA#BBB#CCC\n1#2\n##3"]
*)

(* sample output:
AAA # BBB # CCC
1   # 2
    #     # 3
*)

(* --- SplitLines --------------------------------------------------- *)

(* ---- description ------------------------------------------------- *)

(* SplitLines[<s>, [opts]]
arguments:
  <s> -- string with lines to be split and distributed over multiple
    shorter lines.
options:
  [IndentCharacter] -- string interpreted as indentation given in <s>
    and later also used for indentation in the result,
  [LineIndent] -- integer giving the amount of [IndentCharacter] used
    for indentation of each new line,
  [PageWidth] -- integer or infinity setting the minimum length for a
    new line after splitting into "words",
  [SplitFunction] -- function used in constructing rules involving
    [WordSeparators] for splitting a string, and
  [WordSeparators] -- string or list of strings to be interpreted as
    word separators, depending on [SplitFunction].
output:
  Error messages in case one or more of the option values are defined
  erroneously.
return:
  String <s> with line breaks inserted in such a way that each line is
  not shorter than indicated by [PageWidth], but also not longer than by
  appending more than one additional word.  With each additional line
  break comes an indentation constructed from [IndentCharacter] and
  [LineIndent].
attributes:
  Listable.
dependencies:
  Lines, (TrimText).
symbols:
  SplitLinesSimple, SplitLinesBefore, SplitLinesAfter.
tags:
  nostring, nointeger, splitrules.
version:
  2013-09-02 (rewrite).
description:
  There is no structure parsing and hence no content sensitive
  indentation.  Line splitting is implemented in a really simple way.
  First, lines are split into "words" at [WordSeparators] and new lines
  are constructed by attaching "words" to the current line as long as
  its length is below [PageWidth].
notes:
- See SplitLinesSimple, SplitLinesBefore, SplitLinesAfter for examples
  of functions used for splitting.
- Depending on [SplitFunction], characters in [WordSeparators] may have
  to be escaped.
- SplitRulesSimple should be used with
    WordSeparators -> {"\\+", "\\-", "\\*", "\\/", "\\="}.
- Beware in case of FORM code, especially for commentaries, functions
  and [...]-symbols.
todo:
- [IndentCharacter]: one string to initialize indentation, an other one
  to use for indentation?
- [Exception]: option to handle, e.g., FORM code and comments?
*)

(* ---- definition -------------------------------------------------- *)

(* -- split simply at each separator -- *)
SplitLinesSimple =
    RegularExpression["(" <> # <> ")"] & ;
(* -- separator on same line as word -- *)
SplitLinesBefore =
    RegularExpression["([" <> # <> "][^" <> # <> "]+)"] & ;
(* -- distributed on different lines -- *)
SplitLinesAfter =
    RegularExpression["([^" <> # <> "]+[" <> # <> "]+)"] & ;

Options[SplitLines] =
{IndentCharacter -> " ",             (* _String *)
 LineIndent -> 1,                    (* _Integer *)
 PageWidth -> 100,                   (* _Integer, All/Full/Infinity *)
 SplitFunction -> SplitLinesBefore,  (* _Function *)
 WordSeparators -> "+\-*/="};        (* _String, {___String} *)

Attributes[SplitLines] =
{Listable};

SplitLines::nostring = "\
A string is expected as option value.
  `1`
is no valid option assignment.";

SplitLines::nointeger = "\
An integer is expected as option value.
  `1`
is no valid option assignment.";

SplitLines::splitrules = "\
Something went wrong in constructing the rules for splitting a string.
Check your settings for [SplitFunction] and [WordSeparators].";

SplitLines[s_String, opts:OptionsPattern[]] :=
    Module[{ic,li,pw,sr,ws, in, split, r},

           (* -- handle options -- *)

           {ic, li, pw, sf, ws} =
               OptionValue[{IndentCharacter, LineIndent, PageWidth,
                            SplitFunction, WordSeparators}];

           pw = pw /. {All -> Infinity, Full -> Infinity};

           (* check option value types *)
           Check[
               If[!StringQ[ic],
                  Message[SplitLines::nostring, IndentCharacter -> ic]];
               If[!IntegerQ[li],
                  Message[SplitLines::nointeger, LineIndent -> li]];
               If[!IntegerQ[pw] && pw =!= Infinity,
                  Message[SplitLines::nointeger, PageWidth -> pw]],
               Return[s]];

           (* construct indentation *)
           in = StringJoin[Table[ic, {li}]];

           (* construct and check rules for splitting *)
           Check[ws = sf[#] -> "$1" & /@ Flatten[{ws}];
                 StringSplit["", ws],
                 Message[SplitLines::splitrules]; Return[s]];

           (* -- helper: single line -- *)

           split[l_] :=
               Module[{os, f,g, t},
                      (* obtain offset *)
                      os = StringCases[l, RegularExpression[
                          "^" <> ic <> "*"], 1];
                      os = If[Length[os] > 0, os[[1]], ""];
                      (* depending on string length, join elements as a
                      string or as a list (with indentation) *)
                      f = If[StringLength[#1] < pw - StringLength[os],
                             {#1 <> #2}, {#1, in <> #2}] & ;
                      (* shift elements from #1 to #2 and apply f on the
                      first of #1 and the last of #2 *)
                      g = {Rest[#1],
                           Join[Most[#2], f[Last[#2], First[#1]]]} & ;
                      (* remove offset *)
                      t = StringTrim[l, RegularExpression[                          (* TODO: check StringTrim *)
                          "^" <> ic <> "*"]];
                      (* split into "words" *)
                      t = StringSplit[t, ws];
                      (* recombine into lines *)
                      t = NestWhile[g[#[[1]], #[[2]]] & , {t, {""}},
                                    (Length[#[[1]]] > 0) & ];
                      (* add offset to each line *)
                      t = os <> # & /@ Last[t];
                      (* recombine lines *)
                      t = StringJoin[Riffle[t, "\n"]]];

           (* -- handle all lines -- *)

           (* apply helper on each line *)
           r = split /@ Lines[s];

           (* recombine split lines *)
           r = StringJoin[Riffle[r, "\n"]];

           (* trim resulting text *)
           (*r = TrimText[r];*)

           Return[r]];

(* ---- examples ---------------------------------------------------- *)

(* sample input/call:
test = ToString[Expand[(1 + x + y^2)^4], InputForm];
SplitLines[test, LineIndent -> 2, PageWidth -> 50]
*)

(* sample output:
1 + 4*x + 6*x^2 + 4*x^3 + x^4 + 4*y^2 + 12*x*y^2 + 12
  *x^2*y^2 + 4*x^3*y^2 + 6*y^4 + 12*x*y^4 + 6*x^2*y^4
  + 4*y^6 + 4*x*y^6 + y^8
*)

(* ------------------------------------------------------------------ *)

(* TODO:
- SplitLinesBefore -> $SplitBefore, ...
- SplitFunction -> Method?
- Documentation for SeparatorLine.
*)

(* ------------------------------------------------------------------ *)

End[];

EndPackage[];

(* ------------------------------------------------------------------ *)
