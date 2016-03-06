<< "../../aux/init.m";
(*<< "../../topoid.m";*)
<< "../../inc/text.m";
<< "../../inc/form.m";

(* --- ToDotProduct --- *)
Print[InputForm[ ToDotProduct[a*b, {}] ]];
Print[InputForm[ ToDotProduct[a*b, {a}] ]];
Print[InputForm[ ToDotProduct[a*b, {a, b}] ]];
Print[InputForm[ ToDotProduct[a*b*c, {a, b}] ]];
Print[InputForm[ ToDotProduct[(a + b + c)^2, {a, b}] ]];
Print[InputForm[ ToDotProduct[(a + b + c)^3, {a, b, c}] ]];
Print[InputForm[ ToDotProducts[a*b, {a, b}] ]];

(* --- FORMForm --- *)
Print[FORMForm[ ToDotProduct[(a + b)^2, {a, b}] ]];
Print[FORMForm[ FORMSymbol[1/x] ]];
Print[FORMForm[ FORMSymbol[1/FORMSymbol[1 - x]] ]];
Print[FORMForm[ f[a + f[b + f[c + d]]] ]];
Print[FORMForm[ f[x + f[FORMSymbol[1/x] + f[x + FORMSymbol[1/FORMSymbol[1 - x]]]]] ]];
Print[FORMForm[ f[x + f[FORMSymbol[1/x] + f[x + FORMSymbol[f[x]/FORMSymbol[1 - x]]]]] ]];

(* --- FORMEscape --- *)
Print[InputForm[ FORMEscape["\n qsq qs\" qqs"] ]];


InputForm[  (# -> "\\" <> #) & /@ {"\n", "\t"}  ]


(* --- FORMIndent --- *)
FORMIndent["aaa\nbbb\nccc"]
FORMIndent["aaa\n*bbb\nccc"]
FORMIndent["*aaa\n*bbb\nccc"]
FORMIndent["#aaa\nbbb\nccc"]


(* --- FORMCode (FORMVariables, FORMExpression alike) --- *)
Print[InputForm[ FORMCode[{a, b}] ]];
Print[InputForm[ FORMCode[FORMCode[a]] ]];
Print[InputForm[ FORMCode[a, FORMCode[b]] ]];
Print[InputForm[ FORMCode[FORMCode[a], b] ]];
Print[InputForm[ FORMCode[a, FORMCode[b, FORMCode[c, {d, e}]]] ]];
Print[ FORMCodeQ[] ];
Print[ FORMCodeQ[FORMCode] ];
Print[ FORMCodeQ[FORMCode[]] ];
Print[ FORMCodeQ[FORMCode[a], b] ];
Print[ FORMCodeQ[FORMCode[a], FORMCode[b]] ];
Print[ FORMCodeQ[{FORMCode[a], FORMCode[{b, c}]}]];
Print[ FORMCodeQ[{FORMCode[a], FORMCode[{b, c}], d}]];

(* --- FORMWrapperQ --- *)
Print[ FORMWrapperQ[FORMVariables[a], FORMExpression[b]] ];
Print[ FORMWrapperQ[FORMVariables[a], b, FORMExpression[c]] ];
Print[ FORMWrapperQ[] ];

(* --- NoFORMCodeQ --- *)
Print[ NoFORMCodeQ[FORMCode[a], FORMCode[b]] ];
Print[ NoFORMCodeQ[a, FORMCode[b]] ];
Print[ NoFORMCodeQ[a, b] ];
Print[ NoFORMCodeQ[a, f[FORMCode[b]]] ];

(* --- FORMFold --- *)
Print[ FORMFold[] ];
Print[ FORMFold["fold"] ];
Print[ FORMFold[FORMCode[]] ];
Print[ FORMFold["fold", FORMCode["id a = 0;\n"], FORMCode["id b = 0;\n"]] ];
Print[ FORMFold["fold", "id a = 0;\n", FORMCode["id b = 0;\n"]] ];
Print[ FORMFold["fold", "id a = 0;\n", "id b = 0;\n"] ];
Print[ FORMFold[FORMCode["id a = 0;\n"], "id b = 0;\n"] ];

(* --- FORMProcedure --- *)
Print[ FORMProcedure[] ];
Print[ FORMProcedure["id a = 0;\n"] ];
Print[ FORMProcedure[FORMCode[]] ];
Print[ FORMProcedure[FORMVariables[]] ];
Print[ FORMProcedure[FORMCode[a], b] ];
Print[ FORMProcedure[FORMVariables[a], FORMCode[b]] ];
Print[ FORMProcedure[FORMVariables[a], b, FORMCode[c], d] ];
Print[ FORMProcedure[a, b, FORMCode[c], d] ];
Print[ FORMProcedure[a, FORMVariables[b], FORMCode[c], d] ];
Print[ FORMProcedure[a, FORMVariables[b], FORMCode[c], FORMVariables[d]] ];
Print[ FORMProcedure[FORMVariables[a, b]] ];
Print[ FORMProcedure[{a, b, c}, "id a = 0;\n"] ];
Print[ FORMProcedure["qq", a, b, c, {"id a = 0;\n"}] ];
Print[ FORMProcedure["id a = 0;\n"] ];
Print[ FORMProcedure[a, "id a = 0;\n"] ];
Print[ FORMProcedure[a, b, c, FORMCode[]] ];
Print[ FORMProcedure["a", "b", "c", "d", FORMVariables[]] ];
Print[ FORMProcedure[{a, b, c}] ];
Print[ FORMProcedure[v, {a, b, c}] ];
Print[ FORMProcedure[{u, v}, {a, b, c}] ];
(* TODO: sort *)

(* --- FORMIfDef --- *)
Print[ FORMIfDef[] ];
Print[ FORMIfDef[FORMCode[]] ];
Print[ FORMIfDef[a, b, c, {code}] ];
Print[ FORMIfDef[a, b, FORMVariables[c], {code}] ];
Print[ FORMIfDef[FORMVariables[a, b, c], {code}] ];
Print[ FORMIfDef[FORMCode[a, b, c], {code}] ];
Print[ FORMIfDef[a, "id a =  0;\n"] ];
Print[ FORMIfDef[{a, b, c}, {"id a = 0;\n"}] ];
Print[ FORMIfDef[a, b, c, "d", FORMVariables[], FORMCode[]] ];
Print[ FORMIfDef["lala"] ];
Print[ FORMIfDef[lala] ];
Print[ FORMIfDef[{a}, {b}, {c}] ];

(* --- FORMRepeat --- *)
<< ../../inc/form.m;
Print[ FORMRepeat[] ];
Print[ FORMRepeat["id a = 0;\n"] ];
Print[ FORMRepeat["id a = 0;\n", "id b = 0;\n"] ];
Print[ FORMRepeat["id a = 0;\n", FORMCode["id b = 0;\n"]] ];
Print[ FORMRepeat[FORMCode["id a = 0;\n"], FORMCode["id b = 0;\n"]] ];


FORMRepeat[ddd, ddd -> 2]


ToFORMCodeString[ FORMRepeat["id a = 0;\n"] ]
Print[ToFORMCodeString[ FORMRepeat["id a = 0;\n"] ]];

Print[ToFORMCodeString[
    FORMRepeat["id a = 0;\n", FORMRepeat["id c = 1;\n", "id d = 2;\n"], "id b = 0;\n"]
    , LineIndent -> 2]];

FORMRepeat["id a = 0;\n", FORMRepeat["id c = 1;\n", "id d = 2;\n", LineIndent -> 2], "id b = 0;\n"]

(* --- FORMCommentIdentify --- *)

FORMComment[FORMIdentify[a -> b, c -> d]]
FORMComment[FORMIdentify[{a -> b, c -> d}]]


(* --- FORMIdentify --- *)
Print[ FORMIdentify[] ];
Print[ FORMIdentify[a] ];
Print[ FORMIdentify[a -> b] ];
Print[ FORMIdentify[a, b, c, d, e] ];
Print[ FORMIdentify[a -> b, f, g, c -> d] ];
Print[ FORMIdentify[a -> b, f, g, h, c -> d] ];
FORMIdentify[{a, b}, c]
FORMIdentify[a, b, {c}]
FORMIdentify[a, b, FORMIdentify[f, g], e]
FORMIdentify[a, b, FORMIdentify[f -> g], e]

FORMDefine[a -> 1 + x]

ToFORMCodeString[%]


(* --- FORMReplace --- *)
FORMReplace[]
FORMReplace[a]
FORMReplace[a, b]
FORMReplace[a, b, c]
FORMReplace[a, b, c, d]
FORMReplace[a, b, c, d -> e, f]
FORMReplace[a, b -> c]
FORMReplace[{a,b,c},d]
FORMReplace[a, {b,c}]
FORMReplace[a, {b,c}, FORMReplace[]]
FORMReplace[a, {b,c}, FORMReplace[b, d, c]]
FORMReplace[a, {b,c}, d, FORMReplace[s]]
FORMReplace[bbb, ccc]
<< "../../inc/form.m";
ToFORMCodeString[%]

(* --- FORMComment --- *)
FORMComment["ss", "qsq\n"]
FORMComment[FORMDefine[a, 1]]
FORMComment[FORMIdentify[a, 1]]
FORMComment["sss"]
FORMComment["AAA\nBBB\nCCC"]
FORMComment["sss\n"]
FORMComment["sss\n\n"]
FORMComment["*"]
FORMComment["*1"]
FORMComment["* ddd"]
FORMComment[FORMRepeat[FORMIdentify[a, 1], "do something\n"]]
ToFORMCodeString[%] // FullForm

    FullForm[StringReplace["qsq\nddd\n\n", RegularExpression["([^\n])$"] -> "$1\n"]]

    FORMComment[FORMIdentify[{a,b,c},0]]

ToFORMCodeString[
    FORMComment["test comment"],
    FORMComment["another comment",
                FORMComment["2nd comment level",
                            FORMComment["3rd level"]],
                FORMComment["ddd"],
                FORMIdentify[a -> b]]
]



(* --- FORMMessage --- *)
FORMMessage["ss qsq\"s\n\t\tqs"]
FORMMessage[{a, b, c}, d, e, {f}]
FORMMessage[{a, b, c}, FORMMessage[ddd]]
ToFORMCodeString[%]

(* --- TextIndent --- *)
TextIndent["aaa\nbbb\n\nccc", 5, "#"]

(* --- *)

FORMRepeat[FORMCode["AAA\n"], FORMRepeat["wdwd;\n"], FORMCode["BBB\n"]]

FORMRepeat[FORMCode["AAA\n"], FORMRepeat["dwdw\n", "wdwd\n"], FORMCode["BBB\n"]]

FORMRepeat["line\n", "line\n", "line\n"]

ToFORMCodeString[%, LineIndent -> 2]

FORMFold["fold", FORMIdentify[a,b,c,d]];

ToFORMCodeString[%, LineIndent -> 4]

FORMComment["haha"]
FORMComment[ss]
ToFORMCodeString[%]


FORMExpression[a, b]

FORMExpression[a, {b, c}, f, FORMExpression[g, h]]


FORMMessage["sss"]
ToFORMCodeString[%]

(* -------------------- *)

ToFORMCodeString[FORMVariables[a, b, c]]
ToFORMCodeString[FORMVariables[a[1], b[2], c[3], FORMSymbol[cc/(1-s)]]]

ToFORMCodeString[FORMExpression[1 + x^2]]

ToFORMCodeString[FORMIdentify["a" -> 0]]

ToFORMCodeString[FORMComment["one comment", "two comments"]]

ToFORMCodeString[FORMComment["bsbsb", FORMVariables[a]]]

FORMIdentify[a -> 0, b -> 0]

FORMDefine[a -> b]
ToFORMCodeString[FORMDefine[a -> 0]]



FORMCode[{a, b, c}]
FORMCode[FORMCode["test"]]

FORMCodeQ[FORMCode["test"]]
FORMCodeQ[{FORMCode["test"]}]
FORMCodeQ["test"]



FORMVariables[{a, b, c}]
FORMVariables["a", "b", "c"]
FORMVariables[a, "b", "c"]
FORMVariables["a", b, "c"]
FORMVariables["a", "b", c]
FORMVariables[a, b, c]
FORMVariables[{"a", b, "c", d}]



ToFORMCodeString[FORMVariables[{"a", b, "c", d}]]
ToFORMCodeString[FORMVariables[]]




ToFORMCodeString[FORMFold["fold1",
                          FORMCode["id a = 0;\n"]]]

ToFORMCodeString[FORMFold["fold1",
                        "id a = 0;\n"]]

ToFORMCodeString["id a = 0;\n"]


Print[ToFORMCodeString[ FORMProcedure["func1", FORMVariables[a, b], "id a = 0;\n"] ]];
Print[ToFORMCodeString[ FORMProcedure["func1", a, b, FORMCode["id a = 0;\n"]] ]];
Print[ToFORMCodeString[ FORMProcedure["func1", FORMCode["id a = 0;\n"]] ]];
Print[ToFORMCodeString[ FORMProcedure["func1", FORMVariables["a"], FORMCode["id a = 0;\n"]] ]];
Print[ToFORMCodeString[ FORMProcedure["func1", FORMVariables["a"], FORMCode["id a = 0;\n"]] ]];


ToFORMCodeString[FORMProcedure["func1", {a, b, c}, {"id a = 1;\n"}]]
ToFORMCodeString[FORMProcedure["func1", {a, b, c}, "id a = 1;\n"]]
ToFORMCodeString[FORMProcedure["func1", a, b, c, {"id a = 1;\n"}]]



ToFORMCodeString[
    FORMFold["fold1",
             FORMComment["this is a comment"],
             FORMProcedure["func1",
                           FORMVariables[a],
                           FORMComment[{"1st comment", "2nd comment"}],
                           FORMMessage["hello!"],
                           FORMIdentify[FORMVariables[{a, b, c}] -> FORMExpression[1 + x^2]]
                       ]
         ]
]



FORMRepeat["id a = 1;\n"]

FORMRepeat[FORMCode["id a = 0;\n"]] // ToFORMCodeString
FORMRepeat["id a = 0;\n"] // ToFORMCodeString

FORMRepeat[FORMCode["id a = 0;\n", "id b = 0;\n"]] // ToFORMCodeString
FORMRepeat[{"id a = 0;\n", "id b = 0;\n"}] // ToFORMCodeString
FORMRepeat["id a = 0;\n", "id b = 0;\n"] // ToFORMCodeString
FORMRepeat["id a = 0;\n", FORMCode["id b = 0;\n"]] // ToFORMCodeString
FORMRepeat[FORMCode["id a = 0;\n"], "id b = 0;\n"] // ToFORMCodeString



FORMIfDef[a, "ddd"]

ToFORMCodeString[FORMIfDef[FORMVariables[a], ToFORMCodeString["ddd;\n"]]]
ToFORMCodeString[FORMIfDef[FORMVariables[a],"ddd;\n"]]
ToFORMCodeString[FORMIfDef[a, ToFORMCodeString["ddd;\n"]]]



FORMCode[FORMComment["1st"], FORMComment["2nd"]] // ToFORMCodeString



FORMIdentify[FORMVariables["a"] -> FORMExpression[(1 + x)^2]] // ToFORMCodeString
FORMIdentify["a" -> FORMExpression[(1 + x)^2]] // ToFORMCodeString
FORMIdentify["a" -> 1]



FORMIdentify["a" -> 1]
FORMIdentify["a" -> 1] // ToFORMCodeString


(**)
FORMIdentify[{FORMVariables[a] -> FORMExpression[1], FORMVariables[b] -> FORMExpression[0]}]
FORMIdentify[{FORMVariables[a] -> FORMExpression[1], FORMVariables[b] -> FORMExpression[0]}] // ToFORMCodeString
FORMIdentify[{a -> b, b -> c, c -> a}] // ToFORMCodeString


(**)
FORMDefine[FORMVariables[u, v] -> FORMVariables[{a, b, c}]] // ToFORMCodeString
FORMDefine[FORMVariables[u, v] -> {a, b, c}] // ToFORMCodeString
FORMDefine[{u, v} -> {a, b, c}] // ToFORMCodeString
FORMDefine[{u -> {a, b, c}, v -> {d, e, f}}] // ToFORMCodeString


<< "../../inc/form.m";
(**)
ToFORMCodeString[FORMReplace[FORMVariables[a, b, b, c, c, d, e]]]
FORMReplace[{a -> b, b -> c, c -> d}]
ToFORMCodeString[FORMReplace[{a -> b, b -> c, c -> d}]]
ToFORMCodeString[FORMReplace[a -> b, b -> c, c -> d]]
FORMReplace[{{a -> b}, b, c, c, {d, e}}]
ToFORMCodeString[%]
ToFORMCodeString[FORMReplace[{{a -> b}, b, c, c, {d, e}}]]


(**)
ToFORMCodeString[FORMIf[FORMExpression["dd"], FORMCode["hahaha;\n"]]]//Print;
ToFORMCodeString[FORMIf[FORMExpression["dd"], FORMCode["hahaha;\n", "hshhs;\n"]]]


Print[ToFORMCodeString[
    FORMComment["this is a test"],
    FORMFold["testfold",
             FORMProcedure["func1", b,
                 FORMComment["comment in fold"],
                 FORMIdentify[a -> 0],
                           FORMRepeat[FORMIdentify[b -> 1]],
                           FORMRepeat[
                               FORMComment["tsst"],
                               FORMIf[FORMExpression["count(a,1) == 1"],
                                      FORMCode["p +s;\n"],
                                      FORMReplace[a -> b]]
                           ]
                       ]
         ]
]]



ToFORMCodeString[
    FORMFold["f1",
        FORMFold["f2",
                 FORMFold["f3",
                          FORMIdentify[a -> FORMExpression[(1 + x)^2]],
                          FORMCode["p;\n"]
                      ]
             ]
    ]
]


ToFORMCodeString["id a = 1;\n"]
ToFORMCodeString[{"id a = 1;\n", "id b = 1;\n"}]


ToFORMCodeString[
    FORMVariables["s"]
]


test1 = ToFORMCodeString["id a = 1;\n"];
ToFORMCodeString[test1]

test2 = FORMFold["fold1", FORMCode[ToFORMCodeString["id a = 1;\n"]]];
ToFORMCodeString[test2]

test7 = FORMProcedure["func1", FORMVariables[{a, "b", c}], FORMCode[ToFORMCodeString["id a = 1;\n"]]];
ToFORMCodeString[test7]

test3 = FORMProcedure["func1", FORMVariables["a", "b", "c"], FORMCode[ToFORMCodeString["id a = 1;\n"]]];
ToFORMCodeString[test3]

test4 = FORMProcedure["func1", FORMVariables["a"], FORMCode[ToFORMCodeString["id a = 1;\n"]]];
ToFORMCodeString[test4]

test5 = FORMProcedure["func1", FORMVariables[], FORMCode[ToFORMCodeString["id a = 1;\n"]]];
ToFORMCodeString[test5]
test6 = FORMFold["fold1", "\n", FORMCode[
    FORMProcedure["func1", FORMVariables["a"], FORMCode[
        "id a = 1;\n"]], "\n"
]];
ToFORMCodeString[test6]


ToFORMCodeString[
    FORMComment["this is a fold"],
    FORMFold["fold1", FORMCode[
        FORMProcedure["func1", FORMVariables[a, b, c], FORMCode[
            FORMComment[{"setting a to 1",
                         "settinh b to 0"}],
            ToFORMCodeString["id a = 1;\n"],
            ToFORMCodeString["id b = 0;\n"],
            FORMRepeat[FORMCode[
                FORMComment[{"hi there"}],
                FORMIfDef[FORMVariables[a], FORMCode[
                    FORMComment["no comment"],
                    FORMMessage["printing out..."]
                ]]
            ]]
        ]]
    ]]
]



ToFORMCodeString[
    FORMRepeat[
        FORMRepeat[
            FORMCode["id a = 0;\n"]
        ]
    ]
]

(* --- *)
Exit[];
