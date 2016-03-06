<< "../../aux/init.m";
(*<< "../../topoid.m";*)
<< "../../inc/text.m";

(* --- TrimLines --- *)
Print[InputForm[ TrimLines["AAA   \n BBB  \n  CCC "] ]];
Print[InputForm[ TrimLines[{"AAA   ", "BBB   "}] ]];
Print[InputForm[ TrimLines["AAA   ", "BBB   "] ]];
Print[InputForm[ TrimLines[] ]];
Print[InputForm[ TrimLines[x] ]];
Print[InputForm[ TrimLines[{"AAA   ", x}] ]];

(* --- IndentLines --- *)
Print[InputForm[ IndentLines["AAA\nBBB\nCCC"] ]];
Print[InputForm[ IndentLines["AAA\nBBB\nCCC", 4] ]];
Print[InputForm[ IndentLines["AAA\nBBB\nCCC", "_"] ]];
Print[InputForm[ IndentLines["AAA\nBBB\nCCC", 4, "_"] ]];
Print[InputForm[ IndentLines[{"AAA", "BBB", "CCC"}, {"+", "-", "_"}] ]];
Print[InputForm[ IndentLines[{"AAA", "BBB", "CCC"}, {2, 4, 6}] ]];
Print[InputForm[ IndentLines[{"AAA", "BBB", "CCC"},
                             {2, 4, 6}, {"+", "-", "_"}] ]];
Print[InputForm[ IndentLines[x] ]];
Print[InputForm[ IndentLines[] ]];

(* --- TrimText --- *)
Print[InputForm[ TrimText["AAA\n\n\nBBB"] ]];
Print[InputForm[ TrimText["\n\n\nAAA"] ]];
Print[InputForm[ TrimText["AAA\n\n\n"] ]];
Print[InputForm[ TrimText["\n\nAAA\n\n\nBBB\nCCC\n\n"] ]];

(* --- SqueezeText --- *)
Print[InputForm[ SqueezeText["AAA\n\n\nBBB"] ]];
Print[InputForm[ SqueezeText["\n\n\nAAA"] ]];
Print[InputForm[ SqueezeText["AAA\n\n\n"] ]];
Print[InputForm[ SqueezeText["\n\nAAA\n\n\nBBB\nCCC\n\n"] ]];

(* --- Lines --- *)
Print[ Lines["AAA\nBBB\nCCC"] ];
Print[ Lines["AAA\nBBB\nCCC\n"] ];

(* --- JoinColumns --- *)
Print[ JoinColumns[{"AAA\nBBB\nCCC", "1\n2\n3"}] ];
Print[ JoinColumns["AAA\nBBB\nCCC", "1\n2\n3"] ];
Print[ JoinColumns[{"AAA\nBBB\nCCC", "1\n2\n3\n4"}] ];
Print[ JoinColumns[{"AAA\nBBB\nCCC\nDDD", "1\n2\n3"}] ];
Print[ JoinColumns[{"\nBBB\nCCC", "1\n2\n3"}] ];
Print[ JoinColumns[{"AAA\nBBB\nCCC", "\n2\n3"}] ];
Print[ JoinColumns[{"AAA\n BBB\n  CCC\n   DDD", "1\n2\n3", "\nb\n\nd"}] ];
Print[ JoinColumns[{"AAA\n BBB\n  CCC\n   DDD", "1\n2\n3", "\nb\n\nd"},
                   Offset -> 0, RecordSeparators -> {" ||", " |"}]      ];
Print[ JoinColumns[{"AAA\n BBB\n  CCC\n   DDD", "1\n2\n3", "\nb\n\nd"},
                   Offset -> x, RecordSeparators -> {" ||", " |"}]      ];
Print[ JoinColumns[{"AAA\n BBB\n  CCC\n   DDD", "1\n2\n3", "\nb\n\nd"},
                   Offset -> 0, RecordSeparators -> True]               ];

(* --- AdjustColumns --- *)
Print[ AdjustColumns["AAA#BBB#CCC\n1#2#3\naa#bb#cc"] ];
Print[ AdjustColumns["AAA#BBB#CCC\n1#2\n##3"] ];
Print[ AdjustColumns["AAA#BBB#CCC*XXX\n1#2*YYY\n##3\naaa\n*#eee",
                     RecordSeparators -> {"#", "*"}]              ];

(* --- SplitLines --- *)
test = ToString[Expand[(1 + x + y^2)^4], InputForm];
Print[ test ];
Print[ SplitLines[test, LineIndent -> 2, PageWidth -> 50] ];
test = ToString[Expand[(1 + x + y^2 + z^3)^4], InputForm];
Print[ test ];
Print[ SplitLines[test] ];
Print[Sequence @@ SplitLines[{test, "\n", test}] ];
Print[ SplitLines[test, IndentCharacter -> "_"] ];
Print[ SplitLines[test, LineIndent -> 2] ];
Print[ SplitLines[test, PageWidth -> 72] ];
Print[ SplitLines[test, PageWidth -> Infinity] ];
Print[ SplitLines[test,
                  WordSeparators -> "+\-*/=",
                  SplitFunction -> SplitLinesAfter] ];
Print[ SplitLines[test,
                  WordSeparators -> "+\-*/=",
                  SplitFunction -> SplitLinesBefore] ];
Print[ SplitLines[test,
                  WordSeparators -> {"\\+", "\\-", "\\*", "\\/", "\\="},
                  SplitFunction -> SplitLinesSimple]                     ];
Print[ SplitLines[test, IndentCharacter -> 1] ];
Print[ SplitLines[test, LineIndent -> " "] ];
Print[ SplitLines[test, PageWidth -> True] ];
Print[ SplitLines[test, SplitFunction -> SplitLinesSimple] ];

(* --- *)
Exit[];
