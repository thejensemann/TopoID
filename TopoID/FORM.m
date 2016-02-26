(* -- "FORM.m": Symbolic Generation of FORM Code -------------------- *)

BeginPackage[
  "TopoID`FORM`",
  {"TopoID`Text`"}];

(* provided functions:
- ToDotProduct(s) -- rewrite products of vectors as dot products,
- FORMForm -- FORM readable conversion of an expression,
- FORMEscapeCharacters -- escape special characters,
- FORM<...> -- various FORM code constructs, and
- ToFORMCodeString -- generation of FORM code strings.
*)

(* N.B.:
- First, Mathematica tries to apply definitions for more complex or
  specific patterns.
- If it cannot be decided between various definitions, they are used in
  the order they were placed or read in [*1].
*)

ClearAll[ToDotProduct, ToDotProducts];
ClearAll[FORMForm, FORMSymbol];
ClearAll[FORMEscapeCharacters];

(* used symbols *)
ClearAll[FORMCode, FORMCodeQ, NoFORMCodeQ,
         FORMVariables, FORMVariablesQ, NoFORMVariablesQ,
         FORMExpression, FORMExpressionQ, NoFORMExpressionQ,
         FORMWrapperQ, NoFORMWrapperQ];

ClearAll[FORMFold];
ClearAll[FORMProcedure];
ClearAll[FORMIfDef];
ClearAll[FORMIf];
ClearAll[FORMRepeat];
ClearAll[FORMComment];
ClearAll[FORMMessage];
ClearAll[FORMDefine];
ClearAll[FORMRedefine];
ClearAll[FORMIdentify, FORMCommentIdentify];
ClearAll[FORMReplace];
ClearAll[ToFORMCodeString];


{FORMNoIndent};



ToDotProduct::usage = "";
ToDotProducts::usage = "";
FORMForm::usage = "";
FORMSymbol::usage = "";
FORMEscapeCharacters::usage = "";

FORMCode::usage = "";
FORMCodeQ::usage = "";
NoFORMCodeQ::usage = "";
FORMVariables::usage = "";
FORMVariablesQ::usage = "";
NoFORMVariablesQ::usage = "";
FORMExpression::usage = "";
FORMExpressionQ::usage = "";
NoFORMExpressionQ::usage = "";
FORMWrapperQ::usage = "";
NoFORMWrapperQ::usage = "";

FORMFold::usage = "";
FORMProcedure::usage = "";
FORMIfDef::usage = "";
FORMIfNDef::usage = "";
FORMIf::usage = "";
FORMHIf::usage = "";
FORMRepeat::usage = "";
FORMComment::usage = "";
FORMMessage::usage = "";
FORMDefine::usage = "";
FORMRedefine::usage = "";
FORMIdentify::usage = "";
FORMCommentIdentify::usage = "";
FORMReplace::usage = "";
ToFORMCodeString::usage = "";


{FORMDenominator, FORMDenominatorRule};















Begin["`Private`"];

(* --- ToDotProduct ------------------------------------------------- *)

ToDotProduct[x_, v_List:{}] :=
    Expand[x] /. Union[Flatten[Outer[#1*#2 -> #1 . #2 &, v, v]]];

ToDotProducts = ToDotProduct;

(* --- FORMForm ----------------------------------------------------- *)

Attributes[FORMSymbol] = {HoldAll};

FORMForm[x_] :=
    (If[StringQ[x], x, ToString[x, InputForm]]
     // StringReplace[#, " . " -> "."] &
     // FixedPoint[StringReplace[
         #, {RegularExpression["FORMSymbol\\[([^\\[\\]]*?)\\]"]
             -> "<$1>",
             RegularExpression["(?m)(?s)\\[([^\\[\\]]*?)\\]"]
             -> "($1)"}
             ] & , #] &
     // FixedPoint[StringReplace[#, RegularExpression["<([^<>]*?)>"] :> StringReplace["[$1]", " " -> ""]] & , #] &       (* TODO: <- add option *)
     (*// StringReplace[#, {"<" -> "[", ">" -> "]"}] &*) );

(* N.B.:
- Spaces are removed from dot products.
- "<" and ">" never occur in FORM expressions.  Hence, they can be used
  as intermediate symbols which, moreover, do not have to be escaped in
  regular expressions.
*)

(* --- FORMEscapeCharacters ----------------------------------------- *)

FORMEscapeCharacters[s_String] :=
    StringReplace[s, {"\n" -> "\\n",
                      "\t" -> "\\t",
                      "\"" -> "\\\"",
                      "\b" -> "\\b"}];

(* --- *)





(* --- FORM<...> ---------------------------------------------------- *)

(* ---- Internal Wrappers ------------------------------------------- *)

(* properties: flatten out lists and wrappers *)
FORMCode[fc1___, {fc2___}, fc3___] :=
    FORMCode[fc1, fc2, fc3];
FORMCode[fc1___, FORMCode[fc2___], fc3___] :=            (* TODO: really flatten out? indentation possible *)
    FORMCode[fc1, fc2, fc3];
FORMVariables[fv1___, {fv2___}, fv3___] :=
    FORMVariables[fv1, fv2, fv3];
FORMVariables[fv1___, FORMVariables[fv2___], fv3___] :=
    FORMVariables[fv1, fv2, fv3];
FORMExpression[fe1___, {fe2___}, fe3___] :=
    FORMExpression[fe1, fe2, fe3];
FORMExpression[fe1___, FORMExpression[fe2___], fe3___] :=
    FORMExpression[fe1, fe2, fe3];

(* TODO: delete when done *)
(*FORMExpression[fe1___, FORMCode[fc___], fe2___] :=
    FORMExpression[fe1, fc, fe2];*)
(* property: plural for expressions *)
(*FORMExpression[fe__] := FORMCode[FORMExpression /@ {fe}] /;
Length[{fe}] > 1;*)

(* test functions *)
FORMCodeQ[fc___] := Length[Flatten[{fc}]] > 0 &&
    And @@ (Head[#] === FORMCode & /@ Flatten[{fc}]);
NoFORMCodeQ[fc___] := Length[Flatten[{fc}]] == 0 ||
    And @@ (Head[#] =!= FORMCode & /@ Flatten[{fc}]);
FORMVariablesQ[fc___] := Length[Flatten[{fc}]] > 0 &&
    And @@ (Head[#] === FORMVariables & /@ Flatten[{fc}]);
NoFORMVariablesQ[fc___] := Length[Flatten[{fc}]] == 0 ||
    And @@ (Head[#] =!= FORMVariables & /@ Flatten[{fc}]);
FORMExpressionQ[fc___] := Length[Flatten[{fc}]] > 0 &&
    And @@ (Head[#] === FORMExpression & /@ Flatten[{fc}]);
NoFORMExpressionQ[fc___] := Length[Flatten[{fc}]] == 0 ||
    And @@ (Head[#] =!= FORMExpression & /@ Flatten[{fc}]);
FORMWrapperQ[fw___] := Length[Flatten[{fw}]] > 0 && And @@
    (MemberQ[{FORMCode, FORMVariables, FORMExpression}, Head[#]] &
     /@ Flatten[{fw}]);
NoFORMWrapperQ[fw___] := Length[Flatten[{fw}]] > 0 && And @@
    (FreeQ[{FORMCode, FORMVariables, FORMExpression}, Head[#]] &
     /@ Flatten[{fw}]);

(* N.B.:
- Theses are internal dummy wrappers which are only used to label
  argument types in code structures.
*)

(* ---- External Wrappers ------------------------------------------- *)

(* N.B.:
- These are external wrappers for code constructs in FORM syntax.
*)

(* ----- FORMFold --------------------------------------------------- *)

$FORMFoldDefault = "<FORM_fold_name>";

(* default: no name *)
FORMFold[] := FORMFold[$FORMFoldDefault];
FORMFold[fc1_, fc2___] := FORMFold[$FORMFoldDefault, fc1, fc2] /;
Head[fc1] =!= String;

(* default: no code; wrap: code *)
FORMFold[fn_String, fc___] := FORMFold[fn, FORMCode[fc]] /;
!FORMCodeQ[fc] || Length[{fc}] > 1;

(* ----- FORMProcedure ---------------------------------------------- *)

$FORMProcedureDefault = "<FORM_procedure_name>";

(* default: no name and no code *)
FORMProcedure[] := FORMProcedure[$FORMProcedureDefault, FORMCode[]];

(* default: no name *)
FORMProcedure[pc_String] := FORMProcedure[$FORMProcedureDefault, pc];
FORMProcedure[pcv_, pc___] /; Head[pcv] =!= String :=
    FORMProcedure[$FORMProcedureDefault, pcv, pc];

(* default: no code *)
FORMProcedure[pn_String, pv__FORMVariables] :=
    FORMProcedure[pn, pv, FORMCode[]];

(* seed: variables *)
FORMProcedure[pn_String, Longest[pv__], pc__] :=
    FORMProcedure[pn, FORMVariables[pv], pc] /;
Length[Complement[Union[Head /@ {pv}], {List}]] == 1 &&
    !FORMVariablesQ[pv] && !FORMCodeQ[pv];

(* seed: code *)
FORMProcedure[pn_String, pc_] := FORMProcedure[pn, FORMCode[pc]] /;
!FORMCodeQ[pc] && !FORMVariablesQ[pc];
FORMProcedure[pn_String, pv_, pc_] :=
    FORMProcedure[pn, pv, FORMCode[pc]] /;
!FORMCodeQ[pc] && !FORMVariablesQ[pc];
FORMProcedure[pn_String, pc__] := FORMProcedure[pn, FORMCode[pv]] /;
Length[Union[Head /@ {pc}]] == 1 && !FORMCodeQ[pc];

(* -- 1st try -- *)

(* wrap: from FORMCode until the end in single FORMCode *)
FORMProcedure[pn_String, pv___, pc1_FORMCode, pc2__] :=
    FORMProcedure[pn, pv, FORMCode[pc1, pc2]];

(* wrap: before last FORMCode in FORMVariables, if necessary *)
FORMProcedure[pn_String, pv__, pc_FORMCode] :=
    FORMProcedure[pn, FORMVariables[pv], pc] /;
NoFORMCodeQ[pv] && (!FORMVariablesQ[pv] || Length[{pv}] > 1);

(* -- 2nd try -- *)

(* wrap: from begin until last FORMVariables in single FORMVariables *)
FORMProcedure[pn_String, pv1__, pv2_FORMVariables, pc___] :=
    FORMProcedure[pn, FORMVariables[pv1, pv2], pc];

(* wrap: after first FORMVariables in FORMCode, if necessary *)
FORMProcedure[pn_String, pv_FORMVariables, pc___] :=
    FORMProcedure[pn, pv, FORMCode[pc]] /;
NoFORMVariablesQ[pc] && (!FORMCodeQ[pc] || Length[{pc}] > 1);

(* N.B.:
- General strategy: bring to either one of two the possible forms
    FORMProcedure[_String, _Code], or
    FORMProcedure[_String, _Variables, _Code].
- First, there is a check for missing name and/or missing code.
- Second comes seeding:
  * A beginning sequence of untagged arguments of the same type is
    considered as variables if something else follows them.
  * A single untagged argument as well as the last of exactly two is
    interpreted as code.
  * A sequence from the beginning until the end of untagged arguments is
    taken to be code.
- Last wrappers are tried to be applied.
*)

(* TODO:
- Option to control replacement of arguments by (`...').
*)

(* ----- FORMIfDef -------------------------------------------------- *)

$FORMIfDefDefault = FORMVariables["<FORM_preprocessor_variable>"];

(* default: no variables and no code *)
FORMIfDef[] := FORMIfDef[$FORMIfDefDefault, FORMCode[]];
(* default: no variables *)
FORMIfDef[fc_FORMCode] := FORMIfDef[$FORMIfDefDefault, fc];
(* default: no code *)
FORMIfDef[fv_FORMVariables] := FORMIfDef[fv, FORMCode[]];

(* seed: variables; cf. FORMProcedure *)
FORMIfDef[Longest[fv__], fc__] := FORMIfDef[FORMVariables[fv], fc] /;
Length[Complement[Union[Head /@ {fv}], {List}]] == 1 &&
    !FORMVariablesQ[fv] && !FORMCodeQ[fv];
(* seed: code; cf. FORMProcedure *)
FORMIfDef[fc_] := FORMIfDef[FORMCode[fc]] /;
!FORMCodeQ[fc] && !FORMVariablesQ[fc];
FORMIfDef[fv_, fc_] := FORMIfDef[fv, FORMCode[fc]] /;
!FORMCodeQ[fc] && !FORMVariablesQ[fc];
FORMIfDef[fc__] := FORMIfDef[FORMCode[fc]] /;
Length[Union[Head /@ {fc}]] == 1 && !FORMCodeQ[fc];

(* wrap: variables and code; cf. FORMProcedure *)
FORMIfDef[fv___, fc1_FORMCode, fc2__] :=
    FORMIfDef[fv, FORMCode[fc1, fc2]];
FORMIfDef[fv__, fc_FORMCode] := FORMIfDef[FORMVariables[fv], fc] /;
NoFORMCodeQ[fv] && (!FORMVariablesQ[fv] || Length[{fv}] > 1);
FORMIfDef[fv1__, fv2_FORMVariables, fc___] :=
    FORMIfDef[FORMVariables[fv1, fv2], fc];
FORMIfDef[fv_FORMVariables, fc___] := FORMIfDef[fv, FORMCode[fc]] /;
NoFORMVariablesQ[fc] && (!FORMCodeQ[fc] || Length[{fc}] > 1);

(* ----- FORMIfNDef -------------------------------------------------- *)  (* TODO: copy of FORMIfDef *)

$FORMIfNDefDefault = FORMVariables["<FORM_preprocessor_variable>"];

(* default: no variables and no code *)
FORMIfNDef[] := FORMIfNDef[$FORMIfNDefDefault, FORMCode[]];
(* default: no variables *)
FORMIfNDef[fc_FORMCode] := FORMIfNDef[$FORMIfNDefDefault, fc];
(* default: no code *)
FORMIfNDef[fv_FORMVariables] := FORMIfNDef[fv, FORMCode[]];

(* seed: variables; cf. FORMProcedure *)
FORMIfNDef[Longest[fv__], fc__] := FORMIfNDef[FORMVariables[fv], fc] /;
Length[Complement[Union[Head /@ {fv}], {List}]] == 1 &&
    !FORMVariablesQ[fv] && !FORMCodeQ[fv];
(* seed: code; cf. FORMProcedure *)
FORMIfNDef[fc_] := FORMIfNDef[FORMCode[fc]] /;
!FORMCodeQ[fc] && !FORMVariablesQ[fc];
FORMIfNDef[fv_, fc_] := FORMIfNDef[fv, FORMCode[fc]] /;
!FORMCodeQ[fc] && !FORMVariablesQ[fc];
FORMIfNDef[fc__] := FORMIfNDef[FORMCode[fc]] /;
Length[Union[Head /@ {fc}]] == 1 && !FORMCodeQ[fc];

(* wrap: variables and code; cf. FORMProcedure *)
FORMIfNDef[fv___, fc1_FORMCode, fc2__] :=
    FORMIfNDef[fv, FORMCode[fc1, fc2]];
FORMIfNDef[fv__, fc_FORMCode] := FORMIfNDef[FORMVariables[fv], fc] /;
NoFORMCodeQ[fv] && (!FORMVariablesQ[fv] || Length[{fv}] > 1);
FORMIfNDef[fv1__, fv2_FORMVariables, fc___] :=
    FORMIfNDef[FORMVariables[fv1, fv2], fc];
FORMIfNDef[fv_FORMVariables, fc___] := FORMIfNDef[fv, FORMCode[fc]] /;
NoFORMVariablesQ[fc] && (!FORMCodeQ[fc] || Length[{fc}] > 1);

(* ----- FORMIf ----------------------------------------------------- *)

$FORMIfDefault = FORMExpression["<FORM_conditional_expression>"];

(* default: no expression and no code *)
FORMIf[] := FORMIf[$FORMIfDefault, FORMCode[]];
(* default: no expression *)
FORMIf[fc_FORMCode] := FORMIf[$FORMIfDefault, fc];
(* default: no code *)
FORMIf[fe_FORMExpression] := FORMIf[fe, FORMCode[]];

(* seed: expression; cf. FORMProcedure *)
FORMIf[Longest[fe__], fc__] := FORMIf[FORMExpression[fe], fc] /;
Length[Complement[Union[Head /@ {fe}], {List}]] == 1 &&
    !FORMExpressionQ[fe] && !FORMCodeQ[fe];
(* seed: code; cf. FORMProcedure *)
FORMIf[fc_] := FORMIf[FORMCode[fc]] /;
!FORMCodeQ[fc] && !FORMExpressionQ[fc];
FORMIf[fe_, fc_] := FORMIf[fe, FORMCode[fc]] /;
!FORMCodeQ[fc] && !FORMExpressionQ[fc];
FORMIf[fc__] := FORMIf[FORMCode[fc]] /;
Length[Union[Head /@ {fc}]] == 1 && !FORMCodeQ[fc];

(* wrap: expression and code; cf. FORMProcedure *)
FORMIf[fe___, fc1_FORMCode, fc2__] :=
    FORMIf[fe, FORMCode[fc1, fc2]];
FORMIf[fe__, fc_FORMCode] := FORMIf[FORMExpression[fe], fc] /;
NoFORMCodeQ[fe] && (!FORMExpressionQ[fe] || Length[{fe}] > 1);
FORMIf[fe1__, fe2_FORMExpression, fc___] :=
    FORMIf[FORMExpression[fe1, fe2], fc];
FORMIf[fe_FORMExpression, fc___] := FORMIf[fe, FORMCode[fc]] /;
NoFORMExpressionQ[fc] && (!FORMCodeQ[fc] || Length[{fc}] > 1);

(* ----- FORMHIf ----------------------------------------------------- *)  (* TODO: copy of FORMIf *)

$FORMHIfDefault = FORMExpression["<FORM_conditional_expression>"];

(* default: no expression and no code *)
FORMHIf[] := FORMHIf[$FORMHIfDefault, FORMCode[]];
(* default: no expression *)
FORMHIf[fc_FORMCode] := FORMHIf[$FORMHIfDefault, fc];
(* default: no code *)
FORMHIf[fe_FORMExpression] := FORMHIf[fe, FORMCode[]];

(* seed: expression; cf. FORMProcedure *)
FORMHIf[Longest[fe__], fc__] := FORMHIf[FORMExpression[fe], fc] /;
Length[Complement[Union[Head /@ {fe}], {List}]] == 1 &&
    !FORMExpressionQ[fe] && !FORMCodeQ[fe];
(* seed: code; cf. FORMProcedure *)
FORMHIf[fc_] := FORMHIf[FORMCode[fc]] /;
!FORMCodeQ[fc] && !FORMExpressionQ[fc];
FORMHIf[fe_, fc_] := FORMHIf[fe, FORMCode[fc]] /;
!FORMCodeQ[fc] && !FORMExpressionQ[fc];
FORMHIf[fc__] := FORMHIf[FORMCode[fc]] /;
Length[Union[Head /@ {fc}]] == 1 && !FORMCodeQ[fc];

(* wrap: expression and code; cf. FORMProcedure *)
FORMHIf[fe___, fc1_FORMCode, fc2__] :=
    FORMHIf[fe, FORMCode[fc1, fc2]];
FORMHIf[fe__, fc_FORMCode] := FORMHIf[FORMExpression[fe], fc] /;
NoFORMCodeQ[fe] && (!FORMExpressionQ[fe] || Length[{fe}] > 1);
FORMHIf[fe1__, fe2_FORMExpression, fc___] :=
    FORMHIf[FORMExpression[fe1, fe2], fc];
FORMHIf[fe_FORMExpression, fc___] := FORMHIf[fe, FORMCode[fc]] /;
NoFORMExpressionQ[fc] && (!FORMCodeQ[fc] || Length[{fc}] > 1);

(* ----- FORMRepeat ------------------------------------------------- *)

(* default and wrap *)
FORMRepeat[fc___] /; !FORMCodeQ[fc] || Length[{fc}] > 1 :=
    FORMRepeat[FORMCode[fc]];

(* ---- Verbosity --------------------------------------------------- *)

(* ----- FORMComment ------------------------------------------------ *)

(* flatten out *)
FORMComment[fc1___, {fc2___}, fc3___] :=
    FORMComment[fc1, fc2, fc3];
(*FORMComment[fc1___, FORMComment[fc2___], fc3___] :=
    FORMComment[fc1, fc2, fc3];*)                  (* TODO: -> comment levels *)
(*FORMComment[fc1___, FORMCode[fc2___], fc3___] :=
    FORMComment[fc1, fc2, fc3];*)

(* ----- FORMMessage ------------------------------------------------ *)

(* flatten out *)
FORMMessage[fm1___, {fm2___}, fm3___] :=
    FORMMessage[fm1, fm2, fm3];
FORMMessage[fm1___, FORMMessage[fm2___], fm3___] :=
    FORMMessage[fm1, fm2, fm3];

(* plural *)
FORMMessage[fm__] := FORMCode[FORMMessage[#] & /@ {fm}] /;
Length[{fm}] > 1;

(* ---- Commands ---------------------------------------------------- *)

(* ----- FORMDefine ------------------------------------------------- *)

$FORMDefineDefault = FORMExpression["0"];

(* property: flatten out [*1] *)
FORMDefine[FORMDefine[d1___Rule], fd2___] := FORMDefine[fd1, fd2];

(* default: no variable and no expression *)
FORMDefine[] := FORMCode[];
(* default: no expression *)
FORMDefine[fv_] := FORMDefine[fv, $FORMDefineDefault] /;
Head[fv] =!= Rule && Head[fv] =!= FORMDefine;

(* seed: rewrite sequence as rules *)
FORMDefine[fv_, fe_, fd___] := FORMDefine[fv -> fe, fd] /;
FreeQ[Head /@ {fv, fe}, Rule] && FreeQ[Head /@ {fv, fe}, FORMDefine];

(* wrap: LHS, RHS *)
FORMDefine[fv_ -> fe_] :=
    FORMDefine[FORMVariables[fv] -> fe] /; !FORMVariablesQ[fv];
FORMDefine[fv_FORMVariables -> fe_] :=
    FORMDefine[fv -> FORMExpression[fe]] /; !FORMExpressionQ[fe];

(* property: plural *)
FORMDefine[fd1__, fd2_Rule] :=
    FORMCode[FORMDefine[fd1], FORMDefine[fd2]];
FORMDefine[fd1_Rule, fd2__] :=
    FORMCode[FORMDefine[fd1], FORMDefine[fd2]];

(* ----- FORMRedefine ------------------------------------------------- *)  (* N.B.: copy of above *)

$FORMRedefineDefault = FORMExpression["0"];

(* property: flatten out [*1] *)
FORMRedefine[FORMRedefine[d1___Rule], fd2___] := FORMRedefine[fd1, fd2];

(* default: no variable and no expression *)
FORMRedefine[] := FORMCode[];
(* default: no expression *)
FORMRedefine[fv_] := FORMRedefine[fv, $FORMRedefineDefault] /;
Head[fv] =!= Rule && Head[fv] =!= FORMRedefine;

(* seed: rewrite sequence as rules *)
FORMRedefine[fv_, fe_, fd___] := FORMRedefine[fv -> fe, fd] /;
FreeQ[Head /@ {fv, fe}, Rule] && FreeQ[Head /@ {fv, fe}, FORMRedefine];

(* wrap: LHS, RHS *)
FORMRedefine[fv_ -> fe_] :=
    FORMRedefine[FORMVariables[fv] -> fe] /; !FORMVariablesQ[fv];
FORMRedefine[fv_FORMVariables -> fe_] :=
    FORMRedefine[fv -> FORMExpression[fe]] /; !FORMExpressionQ[fe];

(* property: plural *)
FORMRedefine[fd1__, fd2_Rule] :=
    FORMCode[FORMRedefine[fd1], FORMRedefine[fd2]];
FORMRedefine[fd1_Rule, fd2__] :=
    FORMCode[FORMRedefine[fd1], FORMRedefine[fd2]];

(* ----- FORMIdentify ----------------------------------------------- *)

$FORMIdentifyDefault = FORMExpression["0"];

(* property: flatten out [*1] *)
FORMIdentify[FORMIdentify[fi1___Rule], fi2___] :=
    FORMIdentify[fi1, fi2];

(* default: no variable and no expression *)
FORMIdentify[] := FORMCode[];
(* default: no expression *)
FORMIdentify[fv_] := FORMIdentify[fv, $FORMIdentifyDefault] /;
Head[fv] =!= Rule && Head[fv] =!= FORMIdentify;

(* seed: rewrite sequence as rules *)
FORMIdentify[fv_, fe_, fi___] := FORMIdentify[fv -> fe, fi] /;
FreeQ[Head /@ {fv, fe}, Rule] && FreeQ[Head /@ {fv, fe}, FORMIdentify];

(* wrap: LHS, RHS *)
FORMIdentify[fv_ -> fe_] :=
    FORMIdentify[FORMVariables[fv] -> fe] /; !FORMVariablesQ[fv];
FORMIdentify[fv_FORMVariables -> fe_] :=
    FORMIdentify[fv -> FORMExpression[fe]] /; !FORMExpressionQ[fe];

(* property: plural *)
FORMIdentify[fi1__, fi2_Rule] :=
    FORMCode[FORMIdentify[fi1], FORMIdentify[fi2]];
FORMIdentify[fi1_Rule, fi2__] :=
    FORMCode[FORMIdentify[fi1], FORMIdentify[fi2]];

(* ----- FORMReplace ------------------------------------------------ *)

$FORMReplaceDefault = FORMExpression["0"];

(* property: flatten out [*1] *)
FORMReplace[fr1___, FORMReplace[fr2___Rule], fr3___] :=
    FORMReplace[fr1, fr2, fr3];
FORMReplace[{fr___}] := FORMReplace[fr];

(* default: no expressions *)
FORMReplace[] := FORMCode[];

(* default: no 2nd expression *)
FORMReplace[fr___Rule, fe_] :=
    FORMReplace[fr, fe -> $FORMReplaceDefault] /;
Head[fe] =!= Rule && Head[fe] =!= FORMReplace;
FORMReplace[fr1___Rule, fe_, fr2_Rule, fr3___] :=
    FORMReplace[fr1, fe -> $FORMReplaceDefault, fr2, fr3] /;
Head[fe] =!= Rule && Head[fe] =!= FORMReplace;

(* seed: rewrite sequence as rules *)
FORMReplace[fr1___Rule, fe1_, fe2_, fr2___] :=
    FORMReplace[fr1, fe1 -> fe2, fr2] /;
FreeQ[Head /@ {fe1, fe2}, Rule] &&
FreeQ[Head /@ {fe1, fe2}, FORMReplace];

(* wrap: LHSs, RHSs *)
FORMReplace[fr1___, fv_ -> fe_, fr2___] /; !FORMExpressionQ[fv] :=
    FORMReplace[fr1, FORMExpression[fv] -> fe, fr2];
FORMReplace[fr1___, fv_FORMExpression -> fe_, fr2___] :=
    FORMReplace[fr1, fv -> FORMExpression[fe], fr2] /;
!FORMExpressionQ[fe];

(* --- *)













(* --- ToFORMCodeString --------------------------------------------- *)

(* global variables *)
$FORMComment = "*";
$FORMSeparator = ",";
$FORMSeparatorSpace = ", ";

$FORMMessage = "#message";
(*$FORMMessage = "#write";*)

$FORMNoIndent =
{FORMFold, FORMProcedure, FORMComment, FORMNoIndent};

(* TODO: cf. "MT.m" *)
$RecursionLimit = 1000;
$IterationLimit = 1000;

Options[ToFORMCodeString] =
{LineIndent -> 2,
 PageWidth -> 100,
 SplitFunction -> SplitLinesBefore,
 WordSeparators -> "+\-*/="};

(* ---- end points and related rules -------------------------------- *)

(* FORMFold *)
ToFORMCodeString[HoldPattern[
    FORMFold[fn_String, fc_FORMCode] ], opts:OptionsPattern[]] ^:=
    $FORMComment <> "--#[ " <> fn <> " :\n" <>
    ToFORMCodeString[fc, opts] <>
    $FORMComment <> "--#] " <> fn <> " :\n";

(* FORMProcedure: no variables *)
ToFORMCodeString[HoldPattern[
    FORMProcedure[pn_String, pc_FORMCode] ], opts:OptionsPattern[]] ^:=
    "#procedure " <> pn <> "\n" <>
    ToFORMCodeString[pc, opts] <>
    "#endprocedure\n";

(* FORMProcedure: with variables *)
ToFORMCodeString[HoldPattern[
    FORMProcedure[pn_String, pv_FORMVariables, pc_FORMCode] ],
                 opts:OptionsPattern[]] ^:=
    "#procedure " <> pn <> "(" <> ToFORMCodeString[pv] <> ")\n" <>
    StringReplace[ToFORMCodeString[pc, opts],                                                      (* TODO *)
(*(RegularExpression["(?m)(^[^*].*DELETETHIS)(?<!\\w)(" <> # <> ")(?!\\w)"] -> "$1(`$2')") &*)
(*(RegularExpression["(?<!\\w)(" <> # <> ")(?!\\w)"] -> "(`$1')") &*)
(RegularExpression["(?<![\\w`])(" <> # <> ")(?![\\w'])"] -> "(`$1')") &
    /@ ToFORMCodeString /@ FORMVariables /@ List @@ pv] <>
    "#endprocedure\n";

(* N.B.:
- Regex replace var. string separated by non-word chars.
*)

(* FORMIfDef *)
ToFORMCodeString[HoldPattern[
    FORMIfDef[fv_FORMVariables, fc_FORMCode] ],
                 opts:OptionsPattern[]] ^:=
    "#ifdef `" <> ToFORMCodeString[fv] <> "'\n" <>
    ToFORMCodeString[fc, opts] <>
    "#endif\n";

(* FORMIfNDef *)  (* TODO: copy of FORMIfDef *)
ToFORMCodeString[
  HoldPattern[FORMIfNDef[fv_FORMVariables, fc_FORMCode]],
  opts:OptionsPattern[]] ^:=
  "#ifndef `" <> ToFORMCodeString[fv] <> "'\n" <>
  ToFORMCodeString[fc, opts] <>
  "#endif\n";

(* FORMIf: single instruction *)
ToFORMCodeString[HoldPattern[
    FORMIf[fe_FORMExpression, fc:FORMCode[_]] ],
               opts:OptionsPattern[]] ^:=
    "if (" <> ToFORMCodeString[fe] <> ") " <>
    ToFORMCodeString[fc, LineIndent -> 0]; (* TODO: LineIndent -> 8? *)

(* FORMIf: multiple instructions *)
ToFORMCodeString[HoldPattern[
    FORMIf[fe_FORMExpression, fc_FORMCode] ], opts:OptionsPattern[]] ^:=
    "if (" <> ToFORMCodeString[fe] <> ");\n" <>
    ToFORMCodeString[fc, opts] <>
    "endif;\n";

(* FORMHIf *)
ToFORMCodeString[
  HoldPattern[FORMHIf[fe_FORMExpression, fc_FORMCode]],
  opts:OptionsPattern[]] ^:=
  "#if (" <> ToFORMCodeString[fe] <> ")\n" <>
  ToFORMCodeString[fc, opts] <>
  "#endif\n";


(* FORMRepeat: single instruction *)
ToFORMCodeString[HoldPattern[
    FORMRepeat[fc:FORMCode[_]] ], opts:OptionsPattern[]] ^:=
    "repeat " <> ToFORMCodeString[fc, LineIndent -> 0]; (* TODO: LineIndent -> 10? *)

(* FORMRepeat: multiple instructions *)
ToFORMCodeString[HoldPattern[
    FORMRepeat[fc_FORMCode] ], opts:OptionsPattern[]] ^:=
    "repeat;\n" <>
    ToFORMCodeString[fc, opts] <>
    "endrepeat;\n";

(* FORMComment *)
ToFORMCodeString[HoldPattern[
    FORMComment[fc___String] ], opts:OptionsPattern[]] ^:=
    StringJoin[(StringReplace[
        #, {RegularExpression["(?m)^([" <> $FORMComment <> "]+ )"]
            -> $FORMComment <> "$1",
            RegularExpression["(?m)^(.+)(?<![" <> $FORMComment <> "] )"]
            -> $FORMComment <> " $1",
            RegularExpression["\n+\\Z"]
            -> ""}] <> "\n") & /@ {fc}];
(* N.B.:
- First, treat things already commented.
- Second, introduce new comments.
- Third, delete multiple trailing newlines.
*)

(* FORMMessage *)
ToFORMCodeString[HoldPattern[
    FORMMessage[fm___String] ], opts:OptionsPattern[]] ^:=
    StringJoin[($FORMMessage <> " " <> # <> "\n") &
                   /@ {fm}];
(*StringJoin[($FORMMessage <> " " <> FORMEscapeCharacters[#] <> "\n") &
                   /@ {fm}];*)

ToFORMCodeString[FORMNoIndent[x___], opts:OptionsPattern[]] ^:= ToFORMCodeString[x];                     (* <- TODO *)

(* FORMDefine *)
ToFORMCodeString[HoldPattern[
    FORMDefine[FORMVariables[fv___] -> fe_FORMExpression] ],
                 opts:OptionsPattern[]] ^:=
    StringJoin[("#define " <> ToFORMCodeString[FORMVariables[#]] <>
                " \"" <> ToFORMCodeString[fe] <>"\"\n") & /@ {fv}];

(* FORMRedefine *)
ToFORMCodeString[HoldPattern[
  FORMRedefine[FORMVariables[fv___] -> fe_FORMExpression] ],
                 opts:OptionsPattern[]] ^:=
  StringJoin[("#redefine " <> ToFORMCodeString[FORMVariables[#]] <>
              " \"" <> ToFORMCodeString[fe] <>"\"\n") & /@ {fv}];

(* FORMCommentIdentify *)
ToFORMCodeString[HoldPattern[
    FORMCommentIdentify[fv_FORMVariables -> fe_FORMExpression] ],
                 opts:OptionsPattern[]] ^:=
    ToFORMCodeString[fv] <> " = " <> ToFORMCodeString[fe];

(* FORMIdentify *)
ToFORMCodeString[HoldPattern[
    FORMIdentify[FORMVariables[fv___] -> fe_FORMExpression] ],
                 opts:OptionsPattern[]] ^:=
    StringJoin[("id " <> ToFORMCodeString[FORMVariables[#]] <>
                " = " <> ToFORMCodeString[fe] <> ";\n") & /@ {fv}];

(* TODO: option for placing (...) around args.?! Strip " " from args.?! *)
(* FORMReplace *)
ToFORMCodeString[
  HoldPattern[FORMReplace[fr:(_FORMExpression -> _FORMExpression)...]],
  opts:OptionsPattern[]] ^:=
  StringJoin["mu replace_(", Riffle[
    Function[r, StringJoin[Riffle[
      ToFORMCodeString /@ Join @@
        ({FORMExpression[#], Last[r]} & /@ List @@ First[r]),
      $FORMSeparator]]] /@ {fr},
    $FORMSeparatorSpace], ");\n"];

(* N.B.:
- FORMCode[], FORMVariables[], FORMExpression[] defined as "".
- Exclude FORMFold, FORMProcedure, and FORMComment wrappers from
  indentation.
*)

(* ------------------------------------------------------------------ *)

ToFORMCodeString[HoldPattern[
    FORMCode[] ], opts:OptionsPattern[]] ^:=
    "";
(*ToFORMCodeString[HoldPattern[
    FORMVariables[] ], opts:OptionsPattern[]] ^:=
    "";*)                                         (* TODO: needed? *)
ToFORMCodeString[HoldPattern[
    FORMExpression[] ], opts:OptionsPattern[]] ^:=
    "";


ToFORMCodeString[HoldPattern[(*GENRULE*)
    FORMComment[fc___] ], opts:OptionsPattern[]] ^:=
    ToFORMCodeString[FORMComment[ToFORMCodeString[#, opts] & /@ {fc}]];

ToFORMCodeString[HoldPattern[(*GENRULE*)
    FORMMessage[fm___] ], opts:OptionsPattern[]] ^:=
    ToFORMCodeString[FORMMessage[ToFORMCodeString[#, opts] & /@ {fm}]];

(* intentionally only one level *)
ToFORMCodeString[HoldPattern[(*GENRULE*)
    FORMComment[fc1___, fi_FORMIdentify, fc2___] ],
                 opts:OptionsPattern[]] ^:=
    ToFORMCodeString[FORMComment[fc1, FORMCommentIdentify @@ fi, fc2]];  (*TODO:opts*)

(* ------------------------------------------------------------------ *)

(* FORMCode *)
(* Master rule: FORMCode *)
ToFORMCodeString[HoldPattern[(*GENRULE*)
    FORMCode[fc__, os:OptionsPattern[]] ], opts:OptionsPattern[]] ^:=

    SplitLines[

        StringJoin[Function[
            (* check for objects not to be indented *)
            s, If[FreeQ[$FORMNoIndent, Head[s]],                          (* TODO: FORMDefine not working?! *)
                  (*IndentLines[#, OptionValue[LineIndent]] &, # &]*)
                  (*IndentLines[#] &, # &]*)
                  IndentLines[#, (LineIndent /. {os} /. {opts} /. LineIndent -> 0)] &, # &]
            @ ToFORMCodeString[s, opts]] /@ {fc}]

        , IndentCharacter -> " ", opts];


                                                                         (*TODO:wrapper*)
(* FORMVariables *)
ToFORMCodeString[HoldPattern[
    FORMVariables[fv___] ], opts:OptionsPattern[]] ^:=
    StringJoin[Riffle[(If[FORMWrapperQ[#], ToFORMCodeString, FORMForm] @ # &  (* TODO: FreeQ -> Expr., Code.? *)
                       /@ {fv}), $FORMSeparator]];




ToFORMCodeString[HoldPattern[
    FORMExpression[fe___] ], opts:OptionsPattern[]] ^:=
    StringJoin[Riffle[(If[FORMWrapperQ[#], ToFORMCodeString, FORMForm] @ # &  (* TODO: FreeQ -> Expr., Code.? *)
                       /@ {fe}), $FORMSeparator]];



(* FORMExpression *)
(*ToFORMCodeString[HoldPattern[(*GENRULE*)
    FORMExpression[fe_] ], opts:OptionsPattern[]] ^:=
    If[FORMWrapperQ[fe], ToFORMCodeString[fe], FORMForm[fe]];*)

(*ToFORMCodeString[HoldPattern[(*GENRULE*)
    FORMExpression[fe__] ], opts:OptionsPattern[]] /;
Length[{fe}] > 1 ^:=
    StringJoin[ToFORMCodeString /@ {fe}];*)
(*ToFORMCodeString[FORMCode[ToFORMCodeString /@ {fe}]];*)










(* ---- properties -------------------------------------------------- *)

(*ToFORMCodeString[fc___, opts:OptionsPattern[]] := ToString[fc];*)
ToFORMCodeString[s___String, opts:OptionsPattern[]] :=
    SplitLines[StringJoin[s], opts];

ToFORMCodeString[fc___, opts:OptionsPattern[]] :=
    ToFORMCodeString[FORMCode[fc], opts] /;
!FORMCodeQ[fc] || Length[{fc}] > 1;



(* ------------------------------------------------------------------ *)




(*ToFORMCodeString[FORMCode[s_String]] ^:= s;
ToFORMCodeString[FORMCode[fc__]] := (*/; FreeQ[{fc}, FORMCodeString] ^:=*)
    StringJoin[FORMCodeString /@ {fc}];*)



(* ##################################################


FORMCodeString[FORMCodeString[fc___]] := FORMCodeString[fc];
FORMCodeString[{fc___}] := FORMCodeString[fc];
FORMCodeString[] := "";
FORMCodeString[_[]] := "";
FORMCodeString[s_String] := s;

FORMCodeString[fc__] := ##/; FreeQ[{fc}, FORMCodeString] :=##
    StringJoin[FORMCodeString /@ {fc}];
################################################## *)



(* ------------------------------------------------------------------ *)


(* TODO:

- Execute FORM code (cf. Takahiro's "FORM.m" accompanying "MT.m").
- Separate into preprocessor and compiled commands? Probably not.

- Proper documentation and samples.
- Add error messages?

- Needs "text.m".

- Add options to various objects (e.g. flag to disable variable search
  in FORMProcedure).

- Further formatting (apply PageWidth).
- Make everything consistent.

---

- Replace, Identify: how to handle "{}", "->"?
- Identify: Vars.[Expr.[...]] OK, Expr.[Code[...]] OK, e.g. "if(count(...))"
- Vars., Code, Expr.:

    Code  --->  Vars.

      \          /
       \        /
        _|    |_

          Expr.

- no indent in FORMIf <- single command version

*)

(* ------------------------------------------------------------------ *)

(* TODO *)

FORMDenominator[x_, a_, b_, n_] := Module[
  {f, s = Sign[a], d = GCD[a, b]},
  ((s*d*f[Abs[a]/d + s*b/d*x])^n) /. f -> FORMSymbol];

FORMDenominatorRule[x_] := Module[
  {a, b, n},
  (a_ + b_.*x)^n_:1 /; Head[a] =!= Plus :> FORMDenominator[x, a, b, n]];

(* ------------------------------------------------------------------ *)

End[];

EndPackage[];

(* ------------------------------------------------------------------ *)
