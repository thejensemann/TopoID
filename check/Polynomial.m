Quit[];

<< ./init.m;
<< TopoID`Polynomial`;

Print["
(* --- AlphaRepresentation --------------------------------------- *)"];

?alpha;

?AlphaRepresentation;

(* our case:
   total single-Higgs production via forward scattering amplitude;
   on-shell box @ 1-loop, one mass, non-planar, euclidean metric

    p1     mh,k1    p2
    ->--o====>====o-->-
        |         |
  p1-k1 v         ^ p2-k1
        |         |
    ->--o---->----o-->-
    p2   p1+p2-k1   p1
*)

ar = AlphaRepresentation[
  {mh^2 + k1^2,       (* line 1 *)
   (p1 - k1)^2,       (* line 2 *)
   (p1 + p2 - k1)^2,  (* line 3 *)
   (p2 - k1)^2},      (* line 4 *)
  {k1},
  GeneratedParameters -> x,
  Constraints -> {p1^2 -> 0, p2^2 -> 0, p1*p2 -> -s/2}];

Print[ar];

(* cf. Vladimir Smirnov's book [Smirnov:2012gma] ex.3.4:
   massless propagator @ 2-loop, minkowskian metric

          ----o----
     q-k /    |    \ q-l
        /     |     \
   ->--o      | k-l  o-->-
    q   \     |     /   q
       k \    |    / l
          ----o----
*)

ar = AlphaRepresentation[
  {k^2,         (* line 1 *)
   (q - k)^2,   (* line 2 *)
   l^2,         (* line 3 *)
   (q - l)^2,   (* line 4 *)
   (k - l)^2},  (* line 5 *)
  {k, l}];

Print[ar];

Print[
  AlphaRepresentation[
    {}, {k, l}]
];

Print[
  AlphaRepresentation[
    {k^2, (q - k)^2, l^2, (q - l)^2, (k - l)^2}, {}]
];

(* messages *)

Print[
  AlphaRepresentation[
    {mh^2, p1^2, (p1 + p2)^2, p2^2}, {k1},
    GeneratedParameters -> x,
    Constraints -> p1*p2 -> -s/2]
];

Print[
  AlphaRepresentation[
    {mh^2 + k1^2, (p1 - k1)^2, (p1 + p2 - k1)^2, (p2 - k1)^2}, {k1},
    GeneratedParameters -> x,
    Constraints -> {p1^2 -> 0, p2^2 -> 0, p1*p2}]
];

Print[
  AlphaRepresentation[
    {mh^2 + k1^2, (p1 - k1)^2, (p1 + p2 - k1)^2, (p2 - k1)^2}, {k1},
    GeneratedParameters -> x[1],
    Constraints -> {p1^2 -> 0, p2^2 -> 0, p1*p2 -> -s/2}]
];

Print[
  AlphaRepresentation[
    {mh^2 + k1^2, (p1 - k1)^2, (p1 + p2 - k1)^2, (p2 - k1)^2}, {k1},
    GeneratedParameters -> {x[1], x[2], x[3]},
    Constraints -> {p1^2 -> 0, p2^2 -> 0, p1*p2 -> -s/2}]
];

Print["
(* --- PolynomialRank -------------------------------------------- *)"];

?PolynomialRank;

Print[
  PolynomialRank[x[1]^2 + 2*x[1]*x[2], {x[1], x[2]}]
];

(* variables implicit *)
Print[
  PolynomialRank[x[1]^2 + 2*x[1]*x[2]]
];
Print[
  PolynomialRank[x[1]^2 + 2*x[1]*x[2] + x[2]^2]
];
Print[
  PolynomialRank[x[1]^2 + 2*x[1]*x[2] + x[2]]
];
Print[
  PolynomialRank[x[1]^2 + x[1]*x[2] + x[2]]
];

Print["
(* --- ScalefulQ ------------------------------------------------- *)"];

?ScalefulQ;

Print[
  ScalefulQ[x[1]^2 + 2*x[1]*x[2], {x[1], x[2]}]
];

(* variables implicit *)
Print[
  ScalefulQ[x[1]^2 + 2*x[1]*x[2]]
];
Print[
  ScalefulQ[x[1]^2 + 2*x[1]*x[2] + x[2]^2]
];
Print[
  ScalefulQ[x[1]^2 + 2*x[1]*x[2] + x[2]]
];
Print[
  ScalefulQ[x[1]^2 + x[1]*x[2] + x[2]]
];

Print["
(* --- PolynomialOrderings --------------------------------------- *)"];

?PolynomialOrderings;

(* basic example *)
Print[
  PolynomialOrderings[
    x[1]^2 + x[1]*x[2] + x[2]^2,
    {x[1], x[2], x[3]}]
];

(* single ordering *)
Print[
  PolynomialOrderings[
    x[1]^2 + x[1]*x[2] + x[2]^2,
    {x[1], x[2], x[3]},
    1]
];

(* variables implicit *)
Print[
  PolynomialOrderings[
    x[1]^2 + 2*x[1]*x[2] + x[1]*x[3]^3]
];

(* more complex example *)
Print[
  PolynomialOrderings[
    x[1] + 2*x[2]^2 + x[3]^2 - 3*x[1]*x[3]^2 - 5*x[3]*x[4]^2,
    {x[1], x[2], x[3], x[4]}]
];

Print["
(* --- PermuteRules ---------------------------------------------- *)"];

?PermuteRules;

(* all variables appear *)
Print[
  PermuteRules[{1, 2, 3}, {x[1], x[2], x[3]}]
];
Print[
  PermuteRules[{3, 2, 1}, {x[1], x[2], x[3]}]
];
Print[
  PermuteRules[{3, 1, 2}, {x[1], x[2], x[3]}]
];

(* some do not appear *)
Print[
  PermuteRules[{3, 1, 2, 4}, {x[1], x[2], x[3], x[4]}]
];
Print[
  PermuteRules[{3, 1, 2}, {x[1], x[2], x[3], x[4]}]
];
Print[
  PermuteRules[{3, 1}, {x[1], x[2], x[3], x[4]}]
];

(* zero permutation *)
Print[
  PermuteRules[{}, {}]
];
Print[
  PermuteRules[{}, {x[1], x[2]}]
];

(* too few variables *)
Print[
  PermuteRules[{1}, {}]
];
Print[
  PermuteRules[{3, 1, 2}, {x[1], x[2]}]
];

(* no permutation *)
Print[
  PermuteRules[{0, 2, 1}, {x[1], x[2]}]
];

Print["
(* --- helpers --------------------------------------------------- *)"];

(* on-shell, 1-loop, non-planar box, 1 mass *)
ap = {x[1], x[2], x[3], x[4]};
ar = AlphaRepresentation[
  {mh^2 + k1^2,
   (p1 - k1)^2,
   (p1 + p2 - k1)^2,
   (p2 - k1)^2},
  {k1},
  GeneratedParameters -> ap,
  Constraints -> {p1^2 -> 0, p2^2 -> 0, p1*p2 -> -s/2}];

(* SortGroups *)

?SortGroups;

Print[
  SortGroups[{{{2, 1, 3}, {1, 2, 3}}}]
];

Print[
  SortGroups[{{2, 1, 3}, {1, 2, 3}}]
];

(* CanonGroups *)

?CanonGroups;

Print[
  CanonGroups[ar, ap, {1, 2, 3, 4}]
];

Print[
  CanonGroups[ar, ap, {1, 2, 3, 4}, 1]
];

(* GroupsCheck *)

?GroupsCheck;

(* single symmetry *)
Print[
  GroupsCheck[
    ar, ap,
    {{1, 2, 3, 4}, {1, 4, 3, 2}}
  ]
];

(* all symmetries *)
Print[
  GroupsCheck[
    ar, ap,
    {{{1, 2, 3, 4}, {1, 4, 3, 2}}, {{1, 2, 4}, {1, 4, 2}}}
  ]
];

(* false symmetry *)
Print[
  GroupsCheck[
    ar, ap,
    {{{1, 2, 3, 4}, {1, 4, 3, 2}}, {{1, 2, 3}, {1, 3, 2}}}
  ]
];

(* missed symmetries *)
Print[
  GroupsCheck[
    ar, ap,
    {{{1, 2, 3, 4}, {1, 4, 3, 2}}, {{1, 2, 3, 4}, {1, 4, 3, 2}},
     {{1, 2, 4}, {1, 4, 2}}}
  ]
];

(* all sub-topologies *)
Print[
  GroupsCheck[
    ar, ap,
    {{{1, 2, 4, 3}}, {{1, 2, 4}}, {{2, 4, 3}}, {{1, 2, 3}, {1, 4, 3}},
     {{1, 3}}, {{2, 4}}, {{1, 2}, {1, 4}}, {{1}}}
  ]
];

(* false sub-topologies *)
Print[
  GroupsCheck[
    ar, ap,
    {{{1, 2, 4, 3}}, {{1, 2, 4}, {2, 4, 3}}, {{1, 2, 3}, {1, 4, 3}},
     {{1, 3}, {2, 4}}, {{1, 2}, {1, 4}}, {{1}}}
  ]
];

(* SubsetIdenticalGroups *)

?SubsetIdenticalGroups;

Print[
  SubsetIdenticalGroups[{{{1, 2, 4}}, {{1, 2}, {2, 3}}}, {1, 2, 3}]
];

(* FilterVanishingGroup *)

?FilterVanishingGroup;

Print[
  FilterVanishingGroup[{{1, 2, 3}, {1, 2}}]
];

(* SubsetVanishingGroup *)

?SubsetVanishingGroup;

Print[
  SubsetVanishingGroup[{{1, 2, 3}, {1, 2, 4}}, {1, 2, 3}]
];

(* MaskSymmetricGroups *)

?MaskSymmetricGroups;

Print[
  MaskSymmetricGroups[
    {{{1, 2, 3}, {1, 3, 2}, {1, 2, 4}}},
    {True, True, True, False}]
];

(* CutsSymmetricGroups *)

?CutsSymmetricGroups;

Print[
  CutsSymmetricGroups[
    {{{1, 2, 3}, {1, 3, 2}, {1, 2, 4}}},
    {{1, 3}, {1, 2}}]
];

Print[
  CutsSymmetricGroups[
    {{{1, 2, 3}, {1, 3, 2}, {1, 2, 4}}},
    {{2, 3}}]
];

(* SortSymmetricGroups *)

?SortSymmetricGroups;

Print[
  SortSymmetricGroups[{{{2, 1, 3}, {2, 3, 1}}}]
];

(* FilterSymmetricGroups *)

?FilterSymmetricGroups;

Print[
  FilterSymmetricGroups[{{{2, 1, 3}, {2, 3, 1}}, {{1, 3}, {3, 1}}}]
];

(* SubsetCutsGroup *)

?SubsetCutsGroup;

Print[
  SubsetCutsGroup[{{1, 2}, {1, 3}}, {1, 2}]
];

Print["
(* --- IdenticalGroups ------------------------------------------- *)"];

(* on-shell, 1-loop, non-planar box, 1 mass *)
ap = {x[1], x[2], x[3], x[4]};
ar = AlphaRepresentation[
  {mh^2 + k1^2,
   (p1 - k1)^2,
   (p1 + p2 - k1)^2,
   (p2 - k1)^2},
  {k1},
  GeneratedParameters -> ap,
  Constraints -> {p1^2 -> 0, p2^2 -> 0, p1*p2 -> -s/2}];

?IdenticalGroups;

Print[
  Options[IdenticalGroups]
];

Print[
  IdenticalGroups[ar, {}]
];

Print[
  IdenticalGroups[ar, ap]
];

(* sample output:
{# groups of equivalent subsets
 {{{1, 2, 4, 3}},          # maximal subset, i.e. identity
  {{1, 2, 4}},             # each subset in canonical order
  {{2, 4, 3}},
  {{1, 2, 3}, {1, 4, 3}},  # all entries are equal
  {{1, 3}},
  {{2, 4}},
  {{1, 2}, {1, 4}},        # also equal subgroups may appear
  {{1}}},
 # vanishing subsets of lines
 {{2, 3}, {3, 4}}}
*)

(* -- level -- *)

Print[
  IdenticalGroups[ar, ap, Null]
];

Print[
  IdenticalGroups[ar, ap, All]
];

Print[
  IdenticalGroups[ar, ap, 3]
];

(*
# only 3-4 lines
{{{{1, 2, 4, 3}},
  {{1, 2, 4}},
  {{2, 4, 3}},
  {{1, 2, 3}, {1, 4, 3}}},
 {}}
*)

Print[
  IdenticalGroups[ar, ap, {3}]
];

(*
# only 3 lines
{{{{1, 2, 4}},
  {{2, 4, 3}},
  {{1, 2, 3}, {1, 4, 3}}},
 {}}
*)

Print[
  IdenticalGroups[ar, ap, {2, 3}]
];

(*
# only 2-3 lines
{{{{1, 2, 4}},
  {{2, 4, 3}},
  {{1, 2, 3}, {1, 4, 3}},
  {{1, 3}},
  {{2, 4}},
  {{1, 2}, {1, 4}}},
 {{2, 3}, {3, 4}}}
*)

(* -- "Cuts" -- *)

Print[
  IdenticalGroups[
    ar, ap, All,
    "Cuts" -> Null]
];

Print[
  IdenticalGroups[
    ar, ap, All,
    "Cuts" -> {{1, 3}}]
];

(*
# subsets containing 1, 3
{{{{1, 2, 4, 3}},
  {{1, 2, 3}, {1, 4, 3}},
  {{1, 3}}},
 {}}
*)

(* -- "Mask" -- *)

Print[
  IdenticalGroups[
    ar, ap, All,
    "Mask" -> Null]
];

Print[
  IdenticalGroups[
    ar, ap, All,
    "Mask" -> {True, False, True, True}]
];

(*
# subsets without 2
{{{{1, 4, 3}},
  {{1, 3}},
  {{1, 4}},
  {{1}}},
 {{3, 4}}}
*)

(* -- Verbosity -- *)

Print[
  IdenticalGroups[
    ar, ap, All,
    Verbosity -> True]
];

Print["
(* --- SymmetricGroups ------------------------------------------- *)"];

(* on-shell, 1-loop, non-planar box, 1 mass *)
ap = {x[1], x[2], x[3], x[4]};
ar = AlphaRepresentation[
  {mh^2 + k1^2,
   (p1 - k1)^2,
   (p1 + p2 - k1)^2,
   (p2 - k1)^2},
  {k1},
  GeneratedParameters -> ap,
  Constraints -> {p1^2 -> 0, p2^2 -> 0, p1*p2 -> -s/2}];

?SymmetricGroups;

Print[
  Options[SymmetricGroups]
];

Print[
  SymmetricGroups[ar, {}]
];

Print[
  SymmetricGroups[ar, ap]
];

(* sample output:
{# groups of symmetric subsets with sub-groups filtered out
 {{{1, 2, 3, 4}, {1, 4, 3, 2}}},
 # vanishing subsets of lines
 {{2, 3}, {3, 4}}}
*)

(* -- level -- *)

Print[
  SymmetricGroups[ar, ap, Null]
];

Print[
  SymmetricGroups[ar, ap, All]
];

Print[
  SymmetricGroups[ar, ap, 3]
];

(*
# only 3-4 lines
{{{{1, 2, 3, 4}, {1, 4, 3, 2}}},
 {}}
*)

Print[
  SymmetricGroups[ar, ap, {3}]
];

(*
# only 3 lines
{{{{1, 2, 3}, {1, 4, 3}},
  {{1, 2, 4}, {1, 4, 2}},
  {{2, 3, 4}, {4, 3, 2}}},
 {}}
*)

Print[
  SymmetricGroups[ar, ap, {2, 3}]
];

(*
# only 2-3 lines
{{{{1, 2, 3}, {1, 4, 3}},
  {{1, 2, 4}, {1, 4, 2}},
  {{2, 3, 4}, {4, 3, 2}}},
 {{2, 3}, {3, 4}}}
*)

(* -- "Cuts" -- *)

Print[
  SymmetricGroups[
    ar, ap, All,
    "Cuts" -> Null]
];

Print[
  SymmetricGroups[
    ar, ap, All,
    "Cuts" -> {{1, 3}}]
];

(*
# subsets containing 1, 3
{{{{1, 2, 3, 4}, {1, 4, 3, 2}}},
 {}}
*)

(* -- "Mask" -- *)

Print[
  SymmetricGroups[
    ar, ap, All,
    "Mask" -> Null]
];

Print[
  SymmetricGroups[
    ar, ap, All,
    "Mask" -> {True, False, True, True}]
];

(*
# subsets without 2
{{},
 {{3, 4}}}
*)

(* -- Method -- *)

Print[
  SymmetricGroups[
    ar, ap, All,
    Method -> Null]
];

Print[
  SymmetricGroups[
    ar, ap, All,
    Method -> {}]
];

(*
{# complete sub-groups are not filtered out
 {{{1, 2, 4, 3}, {1, 4, 2, 3}},
  {{2, 4, 3}, {4, 2, 3}},
  {{1, 4, 3}, {1, 2, 3}},
  {{1, 2, 4}, {1, 4, 2}},
  {{2, 4}, {4, 2}},
  {{1, 4}, {1, 2}}},
 # subsets are not filtered out
 {{3, 4}, {2, 3}, {4}, {3}, {2}}}
*)

Print[
  SymmetricGroups[
    ar, ap, All,
    Method -> {"Order"}]
];

(*
# in order to faciliate filtering, groups are sorted
{{{{1, 2, 3, 4}, {1, 4, 3, 2}},
  {{1, 2, 3}, {1, 4, 3}},
  {{1, 2, 4}, {1, 4, 2}},
  {{2, 3, 4}, {4, 3, 2}},
  {{1, 2}, {1, 4}},
  {{2, 4}, {4, 2}}},
 {{2}, {3}, {4}, {2, 3}, {3, 4}}}
*)

(* -- Verbosity -- *)

Print[
  SymmetricGroups[
    ar, ap, All,
    Verbosity -> True]
];

Print["
(* --- PartialFractioning ---------------------------------------- *)"];

?PartialFractioning;

Print[
  PartialFractioning[
    {d1 - d2 - d3 + d4 + s - mh^2},
    {d1, d2, d3, d4},
    {s, mh^2}]
];

(Print /@ PartialFractioning[
  {d1 - d2 - d3 + d4 + s - mh^2},
  {d1, d2, d3, d4},
  {s, mh^2},
  Verbosity -> True]);

(* messages *)

(Print /@ PartialFractioning[
  {d1 - d2 - d3 + d4 + s - mh^2}]);

(Print /@ PartialFractioning[
  {d1 - d2 - d3 + d4 + s - mh^2}, {d1, d2, d3, d4}]);

(Print /@ PartialFractioning[
  {d1 - d2 - d3 + d4 + s - mh^2}, {}, {s, mh^2}]);

(Print /@ PartialFractioning[
  {d1 - d2 - d3 + d4 + s - mh^2}, {d1, d2, d3}, {s, mh^2}]);

(Print /@ PartialFractioning[
  {d1 - d2 - d3 + d4 + s - mh^2}, {d1, d2, d3, d4}, {s}]);

(* --- *)

Quit[];
