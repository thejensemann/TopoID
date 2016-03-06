

$TopoIDDebug = True;

<< ../topoid.m;

Exit[];


Defaults[test] = a -> 1;

(
 DefaultValue[test, a -> 2] // InputForm // Print;
DefaultValue[test, b -> 2] // InputForm // Print;
DefaultValue[test, {a -> 2}] // InputForm // Print;
DefaultValue[test, {b -> 2}] // InputForm // Print;
)

Defaults[test] = {a -> 1, b -> 2};
(
DefaultValue[test, a -> 2] // InputForm // Print;
DefaultValue[test, b -> 3] // InputForm // Print;
DefaultValue[test, {a -> 2}] // InputForm // Print;
DefaultValue[test, {a -> 2, c -> 3}] // InputForm // Print;
 );


<< inc/syst.m;

(Checks[test] = {arg1 -> _Integer,
                 arg2 -> _String,
                 arg3 -> {_Integer, _String}});

CheckMessages[test] = {arg3 -> "dqdq"};

(testargs = {arg1 -> 1,
             arg2 -> "a",
             arg3 -> {1 , ""}});

CheckList[test, testargs]

CheckList[test, arg1 -> ""]
