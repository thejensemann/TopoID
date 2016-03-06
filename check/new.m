Exit

<< ../aux/init.m;

<< ../topoid.m;

tst = GetTopologies["top/TTA.def"][[1]];

{m1, tst2} = CanonicalizeTopology[tst];

tst3 = ReduceTopology[tst2];

tst4 = InspectTopology[tst3];

tst5 = SymmetrizeTopology[tst4];

Print /@ (zero /. tst5);
Print /@ (symm /. tst5);

<< ../new.m;

trs = TopologyToRules[tst5]

ex = TTA[1,1,1,0,1,1,-1];

LookUp[ex]

TTA[1,0,0,0,1,0,1] /. trs


ReplaceList[ex, trs] // Union

FixedPoint[Fold[ReplaceAll, #, trs] &, ex]
LookUp[%]


LookUp[{TTA[1,1,1,1,1,1,1], TTA[20,12,34,2,2,1,1], TTA[1,1,1,0,0,1,1]}]

(LookUp[TTA[1,1,1,0,0,1,1]])

(LookUp[TTA[1,1,1,1,1,1,1]])



(*
test TAP
*)

TTA[1,1,1,1,1,1,1] /. TAB
TTA[1,1,1,0,0,1,1] /. TAB

(1/2*TTA[1,1,1,0,0,1,1]) /. TAP[]
(1/2*TTA[1,1,1,0,0,1,1]) /. TAP[TTA]
(1/2*TTA[1,1,1,0,0,1,1]) /. TAP[{TTB, TTA}]
(1/2*TTA[1,1,1,0,0,1,1]) /. TAP[TTC]
(1/2*TTA[1,1,1,0,0,1,1]) /. TAP["TT*"]
(1/2*TTA[1,1,1,0,0,1,1]) /. TAP[RegularExpression["TT.*"]]
(1/2*TTA[1,1,1,0,0,1,1]) /. TAP[TTB, TTA]

fff[1,13] /. TAP


(1/2) /. TAP


Pattern[a, _][[1]]

(* -- *)

<< ../aux/init.m
<< ../topoid.m

<< ../cg.m


Exit
;

<<TopoID`;

?TopoID

 <<TopoID`Core`
