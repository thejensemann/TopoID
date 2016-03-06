



ToMapping[ARGS, Naming -> Inherit["MIm"]]
ToMapping[ARGS, Naming -> Table["ss" <> ToString[i] <> "ss", {i, Length[ARGS]}]]

ToMapping[ARGS[[1]], Naming -> Iteratse[5, "sss."]]
ToMapping[ARGS[[1]], Naming -> (ToString[#] & )]

ToMapping[ex2, Naming -> Inherit]

ToMapping[ex2, Naming -> "Hash"]

ToMapping[4*NN[1,2]]

ToMapping[{}]
