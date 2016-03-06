(* check: in batch mode *)
If[$FrontEnd === Null,

   (* -- set output options -- *)
   SetOptions[
     {"stdout", "stderr"},
     {PageWidth -> 100,
      TotalWidth -> Infinity,
      FormatType -> OutputForm}];

   (* -- redefine print: no quotes around strings -- *)
   Unprotect[Print];
   Print[expression___] := WriteString[
     "stdout", Sequence @@
       (If[Head[#] === String, #, InputForm[#]] &
        /@ ({expression} /. TableForm | MatrixForm -> Identity)),
     "\n"];
   Protect[Print]];
