
<< TopoID`Code`

ToFORMCodeString[FORMTopologyZeros[
  Topology[TOPS["pre-g", "NLOr"][[1]], zero -> None]
]]

ToFORMCodeString[FORMTopologyZeros[
  "facs", Topology[TOPS["pre-g", "NLOr"][[1]], zero -> None]
]]

ToFORMCodeString[FORMTopologyZeros[
  "func", Topology[TOPS["pre-g", "NLOr"][[1]], zero -> None]
]]

ToFORMCodeString[FORMTopologyZeros[
  "fact", Topology[TOPS["pre-g", "NLOr"][[1]]]
]]

ToFORMCodeString[FORMTopologyZeros[
  "func", Topology[TOPS["pre-g", "NLOr"][[1]], zero -> {}]
]]

ToFORMCodeString[FORMTopologyZeros[
  Topology[TOPS["pre-g", "NLOr"][[1]]], SETUP["NLOr"]
]]

ToFORMCodeString[FORMTopologyZeros[
  Topology[Topology[TOPS["gen-g", "NLOr"][[1]]]]
]]

ToFORMCodeString[FORMTopologyZeros[
  Topology[Topology[TOPS["gen-g", "NLOr"][[1]]], zero -> {}]
]]

ToFORMCodeString[FORMTopologyZeros[
  Topology[Topology[TOPS["gen-g", "NLOr"][[1]]], zero -> None]
]]




<< TopoID`Code`
<< TopoID`Topology`

ToFORMCodeString[FORMTopologySymmetries[
  Topology[TOPS["gen-b", "NNLOr"][[1]], symm -> None]
]]

ToFORMCodeString[FORMTopologySymmetries[
  "facs", Topology[TOPS["gen-b", "NNLOr"][[1]], symm -> None]
]]

ToFORMCodeString[FORMTopologySymmetries[
  "func", Topology[TOPS["gen-b", "NNLOr"][[1]], symm -> None]
]]

ToFORMCodeString[FORMTopologySymmetries[
  "fact", Topology[TOPS["gen-b", "NNLOr"][[1]]]
]]

ToFORMCodeString[FORMTopologySymmetries[
  "func", Topology[TOPS["gen-b", "NNLOr"][[1]]]
]]

ToFORMCodeString[FORMTopologySymmetries[
  "func", Topology[TOPS["gen-b", "NNLOr"][[1]], symm -> {}]
]]

ToFORMCodeString[FORMTopologySymmetries[
  Topology[TOPS["gen-b", "NNLOr"][[1]]], SETUP["NNLOr"]
]]

ToFORMCodeString[FORMTopologySymmetries[
  Topology[Topology[TOPS["gen-b", "NNLOr"][[2]]]], All
]]

ToFORMCodeString[FORMTopologySymmetries[
  Topology[Topology[TOPS["gen-b", "NNLOr"][[1]]], symm -> {}]
]]

ToFORMCodeString[FORMTopologySymmetries[
  Topology[Topology[TOPS["gen-b", "NNLOr"][[1]]], symm -> None], All
]]


ToFORMCodeString[
  FORMTopology[
    Topology[TOPS["gen-b", "NLOr"][[1]],
             scps -> {}, rels -> {}, pros -> None,
             facs ->
             {d3 -> 2*p2*v1 + v1^2 + s*x, d4 -> v1^2 + s*x, d1 -> v1^2 (* d2 -> -((2*p1 - v1)*v1) *)}]
    (*SETUP["NLOr"]*)
    ]
]
