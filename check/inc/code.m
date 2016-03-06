<< "../../aux/init.m";
<< "../../topoid.m";
<< "../../inc/text.m";
<< "../../inc/form.m";
<< "../../inc/code.m";

(* --- sample data --- *)

( setup = {Externals -> {p1, p2, p3, p4},
           Internals -> {v1, v2},
           Masses -> {"qu" -> 0, "qd" -> 0, "hb" -> mh,
                      "gl" -> 0, "gh" -> 0, "si" -> 0,
                      "Qu" -> 0, "Qd" -> 0},
           Constants -> {s, mh^2},
           Constraints -> {p3 -> p1, p4 -> p2, p1^2 -> 0, p2^2 -> 0, p1*p2 -> -s/2},
           Rules -> {mh^2 -> s*x}}; );
setup = InitSetup[setup];

ListView[setup]

Get["btops.m"];

ListView[btops[[1]], Depth -> 2]

PrepareTopology[btops[[1]], setup] // ListView;

Length[facs /. #] & /@ btops

btops = PrepareTopology[#, setup] & /@  btops;

(

 (tmp = ToFORMCodeString[FORMTopology[#]];
  str = OpenWrite["/x1/jens/dev/test/lap/" <> (name /. #) <> ".def"];
  WriteString[str, tmp];
  Close[str]) & /@ btops[[1 ;; -4]];

 );

test = FORMTopology[btops[[1]]];

test = FORMTopologyComment[btops[[1]]]
test = FORMTopologyDefine[btops[[1]]];
test = FORMTopologyFactors[btops[[1]]];
test = FORMTopologyFunction[btops[[1]]];
test = FORMTopologyExpand[btops[[1]]];

ToFORMCodeString[test, PageWidth -> 72, LineIndent -> 2]
SplitLines[%,  PageWidth -> 50, LineIndent -> 2]

(* --- *)
Exit[];
