(* -- "Polynomial.m": Related to Polynomials ------------------------ *)

(* --- provided functions:

AlphaRepresentation[<facs>, <ks>, [opts]] -> {<U>, <W>}
-- build U- and W-polynomials for an integral

PolynomialRank[<poly>, [<vars>]] -> <int>
-- defined via matrix rank of variable exponents

ScalefulQ[<poly>, [<vars>]] -> <bool>
-- check if an integral is scaleful

PolynomialOrderings[<poly>, [<vars>], [<n>]] -> <perms>
-- canonical reorderings of variables in polynomial

PermuteRules[<perm>, <vars>] -> <rules>
-- apply reordering to expression


helpers:

SortGroups[<grp(s)>] -> <grp(s)>
-- sort groups by descending length

CanonGroups[{<U>, <W>}, <vars>, <grp>, [<n>]] ->
  {<nv>, <U'>, <W'>} -> <grps'>
-- pair of canonic polynomials and groups

GroupsCheck[{<U>, <W>}, <vars>, <grp(s)>] ->
  <bool> | {{<pos>...}, {{<pos>..}...}}
-- check identical or symmetric groups explicitly

SubsetIdenticalGroups[<grps>, <s>] -> <groups>
-- subsets of identical groups to some set

FilterVanishingGroup[<grp>] -> <grp>
-- filter out subsets in vanishing group

SubsetVanishingGroup[<grps>, <s>] -> <grps>
-- subset of vanishing group to some set

MaskSymmetricGroups[<grps>, <mask>] -> <grps>
-- split symmetric groups by some mask

CutsSymmetricGroups[<grps>, <cuts>] -> <grps>
-- split symmetric groups by cuts

SortSymmetricGroups[<grps>] -> <grps>
-- canonical ordering for symmetric groups

FilterSymmetricGroups[<grps>] -> <grps>
-- filter out sub-groups in symmetric groups

SubsetCutsGroup[<cuts>, <s>] -> <cuts>
-- subset of cuts to some set


IdenticalGroups[{<U>, <W>}, [<vars>], [<lvl>], [opts]] ->
  {<subt>, <zero>}
-- inspect equivalent and vanishing sub-topologies

SymmetricGroups[{<U>, <W>}, [<vars>], [<lvl>], [opts]] ->
  {<symm>, <zero>}
-- inspect symmetric and vanishing sub-topologies


PartialFractioning[<rels>, [<vars>], [<cons>]] -> <rules>
-- generate partial fractioning relations

*)

(* --- package begin ------------------------------------------------ *)

Unprotect["TopoID`Polynomial`*"];

ClearAll["TopoID`Polynomial`*", "TopoID`Polynomial`Private`*"];

BeginPackage[
  "TopoID`Polynomial`",
  {"TopoID`Common`", "TopoID`System`"}];  (* TODO *)

{Polynomial,
 alpha, AlphaRepresentation,
 PolynomialRank, ScalefulQ,
 PolynomialOrderings, PermuteRules};

{SortGroups, CanonGroups, $GroupsCheck, GroupsCheck,
 SubsetIdenticalGroups, CutsSelectGroups,
 FilterVanishingGroup, SubsetVanishingGroup,
 CutsSymmetricGroups, MaskSymmetricGroups,
 SortSymmetricGroups, FilterSymmetricGroups,
 SubsetCutsGroup};

{IdenticalGroups,
 $SymmetricGroupsMethods, SymmetricGroups};

{PartialFractioning};

Begin["`Private`"];

(* --- Polynomial::variables ---------------------------------------- *)

Polynomial::variables = "\
Warning: `1` will be used as variable(s).";

(* --- AlphaRepresentation ------------------------------------------ *)

(* ---- description ------------------------------------------------- *)

(* AlphaRepresentation[<fs>, <ks>, [opts]]
arguments:
  <fs> -- list of expressions for the topology factors,
  <ks> -- list of symbols denoting loop momenta.
options:
  [Constraints] -- rule or list of rules giving kinematic constraints,
    for instance,
  [GeneratedParameters] -- list of symbols used as alpha-parameters in
    the result.
output:
  Error messages in various cases (cf. tags).
return: {<U>, <W>}
  <U> -- expression for the first,
  <W> -- expression encoding <U> and the second Symanzik polynomial.
  {0, 0} in case of integrals obiously vanishing.
  $Aborted in case of not passing a check.
symbols:
  alpha[1], ..., alpha[<n>], with <n> being the number of <fs> in case
  of the default setting for [GeneratedParameters].
tags:
  >constraints< (no list of rules),
  >parameter< (no non-empty list, single symbol or string).
  >number< (too few symbols given in list).
version:
  2012-09-25 (adapted Alexey's version),
  2013-08-28 (application of constraints),
  2014-01-16 (minor modifications).
  2014-09-14 (introduced atomic).
description:
  The graph theoretical formula is not used at all, instead the Gaussian
  integrals are done loop by loop and the coefficients contributing to
  the polynomials are determined by completing the square accordingly.
notes:
- Cf. for example Vladimir Smirnov's book [Smirnov:2006ry] ch.2 (p.16,
  pp.19-21) and ch.3 (p.31 ff.).
- Alexey adopted Alexander Smirnov's original UF to his version of this
  file (cf. [http://science.sander.su/Tools-UF.htm]).
- <fs> and <ks> should be non-empty since there has to be at least one
  propagator forming one loop.
- Actually, <U> and <W> appear directly in the Feynman parametrization,
  not the alpha-representation.
*)

(* ---- definition -------------------------------------------------- *)

alpha::usage = "\
AlphaRepresentation: Default option value for [GeneratedParameters].
This results in symbols alpha[1], alpha[2], ... being used as alpha-\
parameters.";

Options[AlphaRepresentation] =
{Constraints -> {},              (* {___Rule} *)
 GeneratedParameters -> alpha};  (* _List | _String | _Symbol *)

AlphaRepresentation::usage = "\
AlphaRepresentation[<dens>, <ks>, [opts]] returns for scalar \
denominators <dens> parametrized by loop-momenta <ks> the two Symanzik \
polynomials {U, W}.
Via the option [Constraints] rules can be provided for, e.g., imposing \
kinematics.  With [GeneratedParemeters] a list, symbol or string can \
be specified to act as resulting alpha- or Feynman parameter(s).";

AlphaRepresentation::constraints = "\
Setting given by `1` for option [Constraints] is neither a rule, nor a \
list of rules.";

AlphaRepresentation::parameter = "\
Invalid setting `1` for option [GeneratedParameters], expected is a \
list, symbol or string."

AlphaRepresentation::number = "\
Not enough symbols for alpha-parameters provided by `1` as option \
value of [GeneratedParameters].";

(* atomic *)
$AlphaRepresentation[
  fs_List, ks_List, gs_List, cs_List:{}] /; Length[fs] === Length[gs] :=
  Module[
    {U,W, k, c0,c1,c2},

    (* special case *)
    If[fs === {} || ks === {},
       Return[{0, 0}]];

    (* argument of the exponential *)
    {U, W} = {1, -Inner[Times, gs, fs]};

    (* loop-wise integration *)
    Do[
      {c0, c1, c2} = Coefficient[W, k, {0, 1, 2}];

      (* check: odd integrals vanish *)
      If[c2 === 0,
         {U, W} = {0, 0};
         Break[]];

      (* rescale loop momenta *)
      U *= c2;

      (* complete the squares *)
      W = Together[c0 - c1^2/(4*c2)],

      {k, ks}];

    (* result *)
    {U, W} = Expand[Together[{U, -U*W}]] //. cs];

(* main *)
AlphaRepresentation[
  fs_List, ks_List,
  opts:OptionsPattern[]] :=
  Module[
    {cs,gs},

    (* options *)
    {cs, gs} = OptionValue[{Constraints, GeneratedParameters}];

    (* check: [Constraints] *)
    cs = Flatten[{cs}];
    If[!MatchQ[cs, {___Rule}],
       Message[AlphaRepresentation::constraints, cs];
       Abort[]];

    (* check: [GeneratedParameters] *)
    gs = Switch[
      gs,
      _List, gs,
      _Symbol, MapIndexed[gs[First[#2]] & , fs],
      _String, MapIndexed[Symbol[gs <> ToString[First[#2]]] & , fs],
      _, Message[AlphaRepresentation::parameter, gs]; Abort[]];

    (* check: number *)
    If[Length[gs] < Length[fs],
       Message[AlphaRepresentation::number, gs];
       Abort[]];

    (* call atomic *)
    $AlphaRepresentation[fs, ks, gs, cs]];

(* trap *)
AlphaRepresentation[___] :=
  (Message[AlphaRepresentation::usage];
   Abort[]);

(* ---- samples ----------------------------------------------------- *)

(* sample call:
E.g.: on-shell box @ 1-loop, one mass, non-planar, euclidean metric

    p1     mh,k1    p2
    ->--o====>====o-->-
        |         |
  p1-k1 v         ^ p2-k1
        |         |
    ->--o---->----o-->-
    p2   p1+p2-k1   p1

AlphaRepresentation[
  {mh^2 + k1^2,       # line 1
   (p1 - k1)^2,       # line 2
   (p1 + p2 - k1)^2,  # line 3
   (p2 - k1)^2},      # line 4
  {k1},
  GeneratedParameters -> x,
  Constraints -> {p1^2 -> 0, p2^2 -> 0, p1*p2 -> -s/2}];
*)

(* sample output:
{-x[1] - x[2] - x[3] - x[4],
 -(mh^2*x[1]^2) - mh^2*x[1]*x[2] - mh^2*x[1]*x[3] - mh^2*x[1]*x[4]
 + s*x[1]*x[3] - s*x[2]*x[4]}
*)

(* --- PolynomialRank ----------------------------------------------- *)

(* ---- description ------------------------------------------------- *)

(* PolynomialRank[<pn>, [<vs>]]
arguments:
  <pn> -- an expression interpreted as polynomial.
(optional):
  [<vs>] -- a list of symbols denoting the variables (if not given, they
    are chosen automatically by Mathematica).
return:
  Dimension of the envelope for points in the space of exponents.
tags:
  Polynomial::variables.
version:
  2013-06-08 (1st adapted version),
  2014-01-16 (minor modifications).
description:
  The dimensionality of the convex hull (or envelope) is computed for
  points corresponding to the exponents of the variables <vs> in the
  monomials of <pn>.
notes:
- If no [<vs>] are given, Variables[] is called within
  CoefficientRules[].
- No dependence on the specific values of the coefficients, just on the
  exponents of the occurring monomials.
- For further information cf. [Pak:2010pt].
*)

(* ---- definition -------------------------------------------------- *)

PolynomialRank::usage = "\
PolynomialRank[<poly>, [<vars>]] returns an integer rank corresponding \
to polynomial <poly> in variables [<vars>] converted into a matrix of \
variable exponents for all terms.
In case [<vars>] is not specified, Variables[<poly>] is employed.";

(* main *)
PolynomialRank[
  pn_, vs_List:{}] :=
  Module[
    {crs, cmx},

    (* check: variables *)
    If[vs === {},
       Message[Polynomial::variables, Variables[pn]]];

    (* polynomial -> coefficient rules *)
    crs = CoefficientRules[pn, vs];

    (* rules -> matrix of exponents *)
    cmx = First /@ crs;

    (* special case *)
    If[Length[cmx] <= 1,
       Return[0]];

    (* result *)
    MatrixRank[# - First[cmx] & /@ Rest[cmx]]];

(* trap *)
PolynomialRank[___] :=
  (Message[PolynomialRank::usage];
   Abort[]);

(* ---- samples ----------------------------------------------------- *)

(* sample calls:
PolynomialRank[x[1]^2 + 2*x[1]*x[2], {x[1], x[2]}]

# variables implicit
PolynomialRank[x[1]^2 + 2*x[1]*x[2]]
PolynomialRank[x[1]^2 + 2*x[1]*x[2] + x[2]]
*)

(* sample output:
1

# variables implicit
1
2
*)

(* --- ScalefulQ ---------------------------------------------------- *)

(* ---- description ------------------------------------------------- *)

(* ScalefulQ[<pn>, [<vs>]]
arguments:
  <pn> -- an expression interpreted as polynomial.
(optional):
  [<vs>] -- a list of symbols denoting the variables (if not given, they
    are chosen automatically by Mathematica).
return:
  False if an integral vanishes, True otherwise (assuming <pn> = U*W).
dependencies:
  PolynomialRank[].
tags:
  Polynomial::variables.
version:
  2013-06-08 (1st adapted version),
  2014-01-16 (minor modifications).
description:
  Check if an integral has a scale, i.e. is non-zero.
notes:
- If no [<vs>] are given, Variables[] is called within
  CoefficientRules[] in PolynomialRank[].
- No dependence on the specific values of the coefficients, just on the
  exponents of the occurring monomials.
- In the alpha-representation one parameter is not independent:
    ... dx_1 ... dx_N delta(1 - x_1 - ... - x_N) ... .
- An integral is zero if there exists a rescaling of integration
  variables leaving it invariant.  This in turn is the case if the rank
  of the polynomial is less than the number of variables (N.B. the
  constraint by the delta-function).
- Be definition: PolynomialRank[pn, vs] <= Length[vs] - 1.
- For further information cf. [Pak:2010pt].
*)

(* ---- definition -------------------------------------------------- *)

ScalefulQ::usage = "\
ScalefulQ[<poly>, [<vars>]] returns True if there is no homogeneous \
rescaling of polynomial <poly> with respect to variables [<vars>] and \
False otherwise.
In case [<vars>] is not specified, Variables[<poly>] is employed.
This can be used in order to check whether Feynman integrals ore zero \
or non-zero via their alpha-polynomials U and W, e.g.:
  xs = {x[1], x[2], ...};
  {U, W} = AlphaRepresentation[{...}, {...}, GeneratedParameters -> xs];
  ScalefulQ[U*W, xs]";

(* main *)
ScalefulQ[
  pn_, vs_List:{}] :=
  Module[
    {vt},

    (* check: variables *)
    vt = vs;
    If[vt === {},
       vt = Variables[pn];
       Message[Polynomial::variables, vt]];

    (* result *)
    pn =!= 0 && PolynomialRank[pn, vt] === Length[vt] - 1];

(* trap *)
ScalefulQ[___] :=
  (Message[ScalefulQ::usage];
   Abort[]);

(* ---- samples ----------------------------------------------------- *)

(* sample calls:
ScalefulQ[x[1]^2 + 2*x[1]*x[2], {x[1], x[2]}]

# variables implicit
ScalefulQ[x[1]^2 + 2*x[1]*x[2]]
ScalefulQ[x[1]^2 + 2*x[1]*x[2] + x[2]]
*)

(* sample output:
True

# variables implicit
True
False
*)

(* --- PolynomialOrderings ------------------------------------------ *)

(* ---- description ------------------------------------------------- *)

(* PolynomialOrderings[<pn>, [<vs>], [<n>]]
arguments:
  <pn> -- an expression interpreted as polynomial.
(optional):
  [<vs>] -- a list of symbols denoting the variables (if not given, they
    are chosen automatically by Mathematica),
  [<n>] -- integer giving the maximum number of orderings returned (all
    available orderings are computed and returned by default).
return:
  All or at most [<n>] permutations of the variables [<vs>] that
  maximize a metric over polynomial <pn>.
tags:
  Polynomial::variables.
version:
  2013-06-18 (reworked Alexey's version),
  2013-01-16 (minor modifications),
  2014-09-07 (major bug-fix).
description:
  Canonical reorderings of the variables [<vs>] for the polynomial <pn>
  are computed.
notes:
- For description of the algorithm cf. [Pak:2011xt] or better my thesis.
- A permutation can be applied via PermuteRules[] to a polynomial, e.g.
    poly = ...;
    vars = {...};
    ord = PolynomialOrderings[poly, vars];
    poly = poly /. PermuteRules[First[ord], vars];
  brings <poly> into canonical form.
*)

(* ---- definition -------------------------------------------------- *)

Options[PolynomialOrderings] =
{Verbosity -> False};

PolynomialOrderings::usage = "\
PolynomialOrderings[<poly>, [<vars>], [<n>] returns for a polynomial \
<poly> a list of permutations of variables [<vars>] of maximum length \
[<n>].  These constitute the canonical orderings, permutations, etc. \
of variables that can be applied to <poly> in order to arrive at its \
unique form.
In case [<vars>] is missing, Variables[<poly>] is invoked.  When no \
value for [<n>] is given, all canonical permutations are computed and \
returned.
For example, a permutation can be applied via PermuteRules[] in the \
following way:
  poly = ...;
  vars = {...};
  ords = PolynomialOrderings[poly, vars];
  poly /. PermuteRules[First[ords], vars]";

(* main *)
PolynomialOrderings[
  pn_, vs_List:{}, n_Integer:-1, opts:OptionsPattern[]] :=
  Module[
    {vb, vt, crs, gcd, cmx, cns, cas, cps, cvs, ord, max},

    vb = OptionValue[Verbosity] /. VerbosityRules;
    Report[vb, 1, "PolynomialOrderings[]: ", n, " orderings."];

    (* check: variables *)
    vt = vs;
    If[vt === {},
       vt = Variables[pn];
       Message[Polynomial::variables, vt]];

    (* -- (1) -- *)

    (* polynomial -> coefficient rules *)
    crs = CoefficientRules[pn, vt];
    Report[vb, 3, "Coefficient rules:\n  ", TableForm[crs], "."];

    (* possible common factor *)
    gcd = PolynomialGCD @@ (Last /@ crs);
    Report[vb, 2, "Common factor: ", gcd, "."];

    (* rules -> matrix of exponents, coefficients *)
    cmx = Append[First[#], Simplify[Last[#]/gcd]] & /@ crs;
    Report[vb, 3, "Basic matrix:\n  ", MatrixForm[cmx], "."];

    (* operate on the transposed *)
    cmx = Transpose[Sort[cmx]];
    Report[vb, 3, "Transposed matrix:\n  ", MatrixForm[cmx], "."];

    (* -- (2) -- *)

    (* initialize list of column numbers, permutations *)
    cns = Range[Length[vt]];
    cas = {{}};

    (* iterate until all variables ordered *)
    While[
      Length[First[cas]] < Length[vt],

      (* -- (3) -- *)

      (* extended permutations *)
      cps = Join @@
        (Function[ca, Append[ca, #] & /@ Complement[cns, ca]] /@ cas);
      Report[vb, 4, "Extended permutations:\n  ", TableForm[cps], "."];

      (* -- (4) -- *)

      (* candidate vectors *)
      cvs = (cmx[[Prepend[#, -1]]]  (* coefficients, swap rows *)
             // Transpose           (* -> columns *)
             // Sort                (* sort rows *)
             // Transpose           (* -> columns *)
             // Last) & /@ cps;     (* extract vector *)
      Report[vb, 5, "Candidate vectors:\n  ", TableForm[cvs], "."];

      (* -- (5) -- *)

      (* lexicographical ordering *)
      ord = Ordering[cvs];
      Report[vb, 4, "Ordering:\n  ", ord, "."];

      (* maximum vector *)
      max = cvs[[Last[ord]]];
      Report[vb, 5, "Maximum vector:\n  ", max, "."];

      (* -- (6) -- *)

      (* select (maximum number of) candidate permutations *)
      cas = Part[cps, Select[ord, cvs[[#]] === max & ]];
      (*cas = If[n >= 0 && n < Length[cas], Take[cas, n], cas];*)            (* TODO: can one really during ordering neglect some? *)
      Report[vb, 4, "Candidate permutations:\n  ", TableForm[cas], "."]

    ];

    (* -- (7) -- *)

    (* result: canonical orderings *)
    cas];

(* trap *)
PolynomialOrderings[___] :=
  (Message[PolynomialOrderings::usage];
   Abort[]);

(* ---- samples ----------------------------------------------------- *)

(* sample calls:
# basic example
PolynomialOrderings[
  x[1]^2 + x[1]*x[2] + x[2]^2,
  {x[1], x[2], x[3]}]

# single ordering
PolynomialOrderings[
  x[1]^2 + x[1]*x[2] + x[2]^2,
  {x[1], x[2], x[3]},
  1]

# variables implicit
PolynomialOrderings[
  x[1]^2 + 2*x[1]*x[2] + x[1]*x[3]^3]
*)

(* sample output:
#basic example
{{1, 2, 3}, {2, 1, 3}}

# single ordering
{{1, 2, 3}}

# variables implicit
{{1, 3, 2}}
*)

(* --- PermuteRules ------------------------------------------------- *)

(* ---- description ------------------------------------------------- *)

(* PermuteRules[<pm>, <vs>]
arguments:
  <pm> -- extended permutation (list of non-negative integers),
  <vs> -- list of symbols defining the variables and their order.
output:
  Error message in case of no valid permutation or not enough specified
  variables.
return:
  Replacement rules for the variables <vs>.  $Aborted in case of one of
  the errors (cf. tags).
tags:
  >number< (not enough variables in <vs> to apply <pm>),
  >noperm< (<pm> is no valid extended permutation list).
version:
  2013-06-16 (rewrite),
  2014-01-16 (minor modifications).
description:
  Reorder the variables <vs> according to the extended permutation <pm>
  and set the remaining variables (those not appearing in <pm>) to zero.
notes:
- This function is used to apply a (sub-)topology selection to the
  alpha-representation and thus also to bring it into the canonical
  form.
*)

(* ---- definition -------------------------------------------------- *)

PermuteRules::usage = "\
PermuteRules[<perm>, <vars>] returns a list of replacement rules \
applying the permutation <perm> to the variables <vars>.
For example, a permutation obtained via PolynomialOrderings[] can be \
applied in the following way:
  poly = ...;
  vars = {...};
  ords = PolynomialOrderings[poly, vars];
  poly /. PermuteRules[First[ords], vars]";

PermuteRules::number = "\
Insufficient number of `1` variables provided, need at least `2`.";

PermuteRules::noperm = "\
`1` is no valid permutation list.";

(* main *)
PermuteRules[
  pm:{___Integer}, vs_List] /; And @@ Positive /@ pm :=
  Module[
    {mp,nv, ps,zs},

    {mp, nv} = {Max[pm], Length[vs]};

    (* check: number of variables *)
    If[mp > nv,
       Message[PermuteRules::number, nv, mp];
       Abort[]];

    (* permute *)
    ps = MapIndexed[vs[[#1]] -> vs[[First[#2]]] &, pm];

    (* set to zero *)
    zs = vs[[#]] -> 0 & /@ Complement[Range[nv], pm];

    (* result *)
    Join[ps, zs]];

(* trap: no permutation *)
PermuteRules[pm_List, vs_List] :=
  (Message[PermuteRules::noperm, pm];
   Abort[]);

(* trap *)
PermuteRules[___] :=
  (Message[PermuteRules::usage];
   Abort[]);

(* ---- samples ----------------------------------------------------- *)

(* sample calls:
# all variables appear
PermuteRules[{3, 1, 2}, {x[1], x[2], x[3]}]

# some do not appear
PermuteRules[{3, 1, 2}, {x[1], x[2], x[3], x[4]}]

# too few variables
PermuteRules[{3, 1, 2}, {x[1], x[2]}]

# no permutation
PermuteRules[{0, 2, 1}, {x[1], x[2]}]
*)

(* sample output:
# all variables appear
{x[3] -> x[1], x[1] -> x[2], x[2] -> x[3]}

# some do not appear
{x[3] -> x[1], x[1] -> x[2], x[2] -> x[3], x[4] -> 0}

# too few variables
PermuteRules::number: Insufficient number of variables 2 provided, need
at least 3.
$Aborted

# no permutation
PermuteRules::noperm: {0, 2, 1} is no valid permutation list.
$Aborted
*)

(* --- helpers: IdenticalGroups, SymmetricGroups, ... --------------- *)

(* ---- SortGroups -------------------------------------------------- *)

SortGroups::usage = "\
SortGroups[<grp(s)>] returns the group(s) <grp(s)> sorted properly (by \
descending length).
In case of groups the individual representations within each group are \
not touched, only their order is changed.  This is not the case when \
acting on a single group, elements in representations are sorted.";

(* main *)
SortGroups[
  sgs:{{__}...}] :=
  SortBy[Union[Sort /@ sgs], -Length[First[#]] & ];

(* trap *)
SortGroups[___] :=
  (Message[SortGroups::usage];
   Abort[]);

(* ---- CanonGroups ------------------------------------------------- *)

CanonGroups::usage = "\
CanonGroups[{<U>, <W>}, <vars>, <grp>, [<n>]] returns a rule {<nv>, \
<U'>, <W'>} -> <grps'> for the alpha-polynomials {<U>, <W>} with \
alpha-parameters <vars> under contraction by the group <grp> of <nv> \
parameters.  Thereby {<U'>, <W'>} is the contracted version of {<U>, \
<W>} in canonical form and <grps'> are the at most [<n>] corresponding \
canonically reordered groups.";

(* main *)
CanonGroups[
  ps:{_, _}, vs_List, sg:{__Integer}, n_Integer:-1] :=
  Module[
    {ns, vt, pt, os, pc, scs},
    (* number of elements *)
    ns = Length[sg];
    (* alpha-representation *)
    vt = Take[vs, ns];
    pt = ps /. PermuteRules[sg, vs];
    (* case: scaleless *)
    If[!ScalefulQ[$TopoIDScale @@ pt, vt],
       Return[0 -> {sg}]];
    (* canonical orderings of variables *)
    os = PolynomialOrderings[$TopoIDMetric @@ pt, vt, n];
    (* permute alpha-representation *)
    pc = pt /. PermuteRules[First[os], vt];
    (* permutations of subset *)
    scs = sg[[#]] & /@ os;
    (* result: search key -> canonical subset *)
    Prepend[pc, ns] -> scs];

(* trap *)
CanonGroups[___] :=
  (Message[CanonGroups::usage];
   Abort[]);

(* Needs:
- $TopoIDScale[], $TopoIDMetric[].
- ScalefulQ[], PolynomialOrderings[], PermuteRules[].
*)

(* ---- GroupsCheck ------------------------------------------------- *)

GroupsCheck::usage = "\
GroupsCheck[{<U>, <W>}, <vars>, <grp(s)>] returns for the \
alpha-polynomials {<U>, <W>} with alpha-parameters <vars> and assumed \
structure of identical or symmetric group(s) <grp(s)> True if this \
assumption indeed holds and False otherwise.";

GroupsCheck::missed = "\
Missed equivalent group(s) at position(s) `1`.";

GroupsCheck::wrong = "\
Found wrong group(s) at position(s) `1`.";

(* main: atomic *)
$GroupsCheck[
  ps:{_, _}, vs_List, sg:{{__Integer}..}] :=
  Module[
    {cc, cw},
    cc = (ps /. PermuteRules[#, vs]) & /@ sg;
    cw = # - First[cc] & /@ Rest[cc];
    cw = Expand[#] === {0, 0} & /@ cw;
    If[And @@ cw,
       Return[First[cc]]];
    False];

(* overload: singular *)
GroupsCheck[
  ps:{_, _}, vs_List, sg:{{__Integer}..}] :=
  $GroupsCheck[ps, vs, sg] =!= False;

(* overload: plural *)
GroupsCheck[
  ps:{_, _}, vs_List, sgs:{{{__Integer}..}...}] :=
  Module[
    {ccs, cws, cms},
    (* call for each group *)
    ccs = $GroupsCheck[ps, vs, #] & /@ sgs;
    (* check: wrong groups *)
    cws = # =!= False & /@ ccs;
    cws = Join @@ Position[cws, False];
    If[cws =!= {},
       Message[GroupsCheck::wrong, cws]];
    (* check: missed groups *)
    cms = Tally[ccs];
    cms = Select[cms, First[#] =!= False && Last[#] > 1 & ];
    cms = (Join @@ Position[ccs, First[#]]) & /@ cms;
    If[cms =!= {},
       Message[GroupsCheck::missed, cms]];
    (* result: boolean or positions *)
    If[cws =!= {} || cms =!= {},
       Return[{cws, cms}]];
    True];

(* trap *)
GroupsCheck[___] :=
  (Message[GroupsCheck::usage];
   Abort[]);

(* Needs:
- PermuteRules[].
*)

(* ---- SubsetIdenticalGroups --------------------------------------- *)

SubsetIdenticalGroups::usage = "\
SubsetIdenticalGroups[<grps>, <s>] returns the identical groups <grps> \
contained in set <s> and sorted properly.";

(* main *)
SubsetIdenticalGroups[
  sgs:{{{__Integer}..}...}, s:{__Integer}] :=
  Module[
    {tgs},
    (* select only subsets of <s> *)
    tgs = Function[sg, Select[sg, Complement[#, s] === {} & ]] /@ sgs;
    (* result: delete empty groups, sort *)
    SortGroups[DeleteCases[tgs, {}, {1, 2}]]];

(* trap *)
SubsetIdenticalGroups[___] :=
  (Message[SubsetIdenticalGroups::usage];
   Abort[]);

(* Needs:
- SortGroups[].
*)

(* ---- CutsSelectGroups -------------------------------------------- *)

CutsSelectGroups::usage = "\
CutsSelectGroups[<grps>, <cuts>] selects the identical groups from \
<grps> containing at least one of the cuts in the set of subsets \
<cuts>.";

(* main *)
CutsSelectGroups[
  sgs:{{__Integer}...}, cts:{{__Integer}...}] :=
  Select[sgs, Function[sg, Or @@ (Intersection[sg, #] === # & /@ cts)]];

(* overload: plural *)
CutsSelectGroups[
  sgs:{{{__Integer}..}...}, cts:{{__Integer}...}] :=
  DeleteCases[CutsSelectGroups[#, cts] & /@ sgs, {}];

(* trap *)
CutsSelectGroups[___] :=
  (Message[CutsSelectGroups::usage];
   Abort[]);

(* ---- FilterVanishingGroup ---------------------------------------- *)

FilterVanishingGroup::usage = "\
FilterVanishingGroup[<grp>] returns the vanishing group <grp> with \
subsets filtered out and sorted properly.";

(* main *)
FilterVanishingGroup[
  zg:{{__Integer}...}] :=
  Module[
    {inQ, zgs},
    (* helper: check if #1 is subset of #2 or vice versa *)
    inQ = MemberQ[{#1, #2}, Intersection[#1, #2]] & ;
    (* sort by descending length, gather subsets *)
    zgs = Gather[SortBy[zg, -Length[#] & ], inQ];
    (* result: sort by ascending length, take largest subsets *)
    SortGroups[Last /@ (SortBy[#, Length] & /@ zgs)]];

(* trap *)
FilterVanishingGroup[___] :=
  (Message[FilterVanishingGroup::usage];
   Abort[]);

(* Needs:
- SortGroups[].
*)

(* ---- SubsetVanishingGroup ---------------------------------------- *)

SubsetVanishingGroup::usage = "\
SubsetVanishingGroup[<grp>, <s>] returns the vanishing group <grp> \
intersected with set <s>, subsets filtered out and sorted properly.";

(* main *)
SubsetVanishingGroup[
  zg:{{__Integer}...}, s:{__Integer}] :=
  FilterVanishingGroup[DeleteCases[Intersection[#, s] & /@ zg, {}]];

(* trap *)
SubsetVanishingGroup[___] :=
  (Message[SubsetVanishingGroup::usage];
   Abort[]);

(* Needs:
- FilterVanishingGroup[].
*)

(* ---- CutsSymmetricGroups ----------------------------------------- *)

CutsSymmetricGroups::usage = "\
CutsSymmetricGroups[<grps>, <cuts>] returns the symmetric groups \
<grps> split into sub-groups according to their cut-structure <cuts>.
Single representations or whole groups are discarded if they do not \
contain a valid cut.  Groups are split in case they mediate forbidden \
interchanges.";

(* main *)
CutsSymmetricGroupsOLD[
  sgs:{{{__Integer}..}...}, cts:{{__Integer}...}] :=
  Module[
    {s,c, inQ,sig,sigs, split},
    (* helper: check if cut <c> is contained in representation <s> *)
    inQ = Function[{c, s}, Intersection[c, s] === c];
    (* helper: signature of representation <s> concerning cut <c> *)
    sig = Function[{s, c}, If[
      inQ[c, s], Sort[Flatten[Position[s, #] & /@ c]], {}]];
    (* helper: signatures of representation <s> concerning all cuts *)
    sigs = Function[s, DeleteCases[Union[sig[s, #] & /@ cts], {}]];
    (* helper: split symmetric group <sg> by cuts into subsets *)
    split[sg_] := Module[
      {cs, tgs},
      (* determine distinct classes *)
      cs = DeleteCases[Union[sigs /@ sg], {}];
      (* classify elements accordingly *)
      tgs = Function[c, Select[sg, sigs[#] === c & ]] /@ cs;
      (* only applicable groups (more than one representation) *)
      Select[tgs, Length[#] > 1 & ]];
    (* result: apply to all *)
    Union @@ (split /@ sgs)];

(* == NOTE: modified on 2016-01-20 == *)

(* main *)
CutsSymmetricGroups[
  sgs:{{{__Integer}..}...}, cts:{{__Integer}...}] :=
  Module[
    {s,c, inQ,sig,sigs, sel,split},
    (* helper: check if representation <s> contains cut <c> *)
    inQ = Function[{s, c}, Intersection[s, c] === c];
    (* helper: signature of representation <s> concerning cut <c> *)
    sig = Function[{s, c}, If[
      inQ[s, c], Union @@ (First[Position[s, #, {1}, 1]] & /@ c), {}]];
    (* helper: signatures of representation <s> concerning all cuts *)
    sigs = Function[s, DeleteCases[Union[sig[s, #] & /@ cts], {}]];
    (* helper: select only groups with more than one representation *)
    sel = Function[s, Length[s] > 1];
    (* helper: split symmetric group into subsets by cut classes *)
    split = Select[GatherBy[#, sigs], sel] & ;
    (* result: apply to all *)
    Union @@ (split /@ sgs)];

(* trap *)
CutsSymmetricGroups[___] :=
  (Message[CutsSymmetricGroups::usage];
   Abort[]);

(* ---- MaskSymmetricGroups ----------------------------------------- *)

MaskSymmetricGroups::usage = "\
MaskSymmetricGroups[<grps>, <mask>] returns the symmetric groups \
<grps> split by applying the mask <mask> to the representations in \
each group.";

MaskSymmetricGroups::mask = "\
Mask `1` has insufficient length for total number of elements `2` \
referred to by groups.";

(* main *)
MaskSymmetricGroups[
  sgs:{{{__Integer}..}...}, msk:{(True | False)..}] :=
  Module[
    {max, mask},
    (* check: length of mask *)
    max = Max[sgs];
    If[Length[msk] < max,
       Message[MaskSymmetricGroups::mask, msk, max];
       Abort[]];
    (* helper: split symmetric group <sg> by mask into subsets *)
    mask[sg_] := Module[
      {cs, tgs},
      (* determine distinct classes *)
      cs = Union[msk[[#]] & /@ sg];
      (* classify elements accordingly *)
      tgs = Function[c, Select[sg, msk[[#]] === c & ]] /@ cs;
      (* only applicable groups (more than one representation) *)
      Select[tgs, Length[#] > 1 & ]];
    (* result: apply to all *)
    Union @@ (mask /@ sgs)];

(* trap *)
MaskSymmetricGroups[___] :=
  (Message[MaskSymmetricGroups::usage];
   Abort[]);

(* ---- SortSymmetricGroups ----------------------------------------- *)

SortSymmetricGroups::usage = "\
SortSymmetricGroups[<grps>] returns the symmetric groups <grps> sorted \
in a canonical way, e.g. as preparation for filtering groups already \
contained in superset groups via FilterSymmetricGroups[<grps>].";

(* main *)
SortSymmetricGroups[
  sgs:{{{__Integer}..}...}] :=
  Module[
    {sort},
    (* helper: sort single group <sg> *)
    sort[sg_] := Module[
      {os, tg, ot, mo},
      (* all orderings *)
      os = Ordering /@ sg;
      (* apply to respective representations *)
      tg = MapThread[#1[[#2]] & , {sg, os}];
      (* their ordering *)
      ot = Ordering[tg];
      (* minimizing ordering *)
      mo = os[[First[ot]]];
      (* apply to group *)
      Union[#[[mo]] & /@ sg]];
    (* result: global sort *)
    SortBy[DeleteDuplicates[sort /@ sgs], -Length[First[#]] & ]];

(* trap *)
SortSymmetricGroups[___] :=
  (Message[SortSymmetricGroups::usage];
   Abort[]);

(* N.B.:
- For the purpose of sorting find the element with minimizing ordering,a
  apply this ordering to all elements and then finally sort the group as
  a whole.
*)

(* ---- FilterSymmetricGroups --------------------------------------- *)

FilterSymmetricGroups::usage = "\
FilterSymmetricGroups[<grps>] returns the symmetric groups <grps> \
without sub-groups that are already contained in superset groups with \
more elements.
Note that <grps> is assumed to be in the sorted form returned by \
SortSymmetricGroups[<grps>] .";

(* main *)
FilterSymmetricGroups[
  sgs:{{{__Integer}..}...}] :=
  Module[
    {inQ, tgs, sg, cgs},
    (* helper: <s> is equal to or contained in <t> *)
    inQ[s_, t_] := Module[
      {m},
      (* position mask relative to base set <t> *)
      m = Flatten[Position[First[t], #] & /@ First[s]];
      (* result: apply to base set <t>, compare *)
      Complement[s, #[[m]] & /@ t] === {}];
    (* initialize *)
    tgs = {};
    (* iterate over symmetric groups *)
    Do[
      (* superset candidates: more elements, more lines *)
      cgs = Select[tgs, Length[#] >= Length[sg] & ];
      cgs = Select[cgs, Length[First[#]] >= Length[First[sg]] & ];
      (* test for superset groups *)
      cgs = Select[cgs, inQ[sg, #] & ];
      (* case: old group found *)
      If[cgs =!= {},
         Continue[]];
      (* case: new group needed *)
      AppendTo[tgs, sg],
      {sg, sgs}];
    (* result *)
    tgs];

(* trap *)
FilterSymmetricGroups[___] :=
  (Message[FilterSymmetricGroups::usage];
   Abort[]);

(* ---- SubsetCutsGroup --------------------------------------------- *)

SubsetCutsGroup::usage = "\
SubsetCutsGroup[<cuts>, <s>] returns the elements from the cuts group \
<cuts> contained in set <s>.";

(* main *)
SubsetCutsGroup[
  cts:{{__Integer}...}, s:{__Integer}] :=
  Select[cts, Intersection[#, s] === # & ];

(* trap *)
SubsetCutsGroup[___] :=
  (Message[SubsetCutsGroup::usage];
   Abort[]);

(* --- IdenticalGroups ---------------------------------------------- *)

(* ---- description ------------------------------------------------- *)

(* IdenticalGroups[{<ps>, [<vs>], [<lv>], [opts]]
arguments:
  <ps> -- list of two expressions for the U- and W-polynomials.
(optional):
  [<vs>] -- list of symbols for alpha-parameters,
  [<lv>] -- standard level specification ("levelspec") for lengths of
    subsets to be considered (cf. >level<), by default All.
options:
  ["Cuts"] -- list of integer lists denoting cuts (cf. >cuts<),
  ["Mask"] -- boolean list as mask (cf. >mask<).
output:
  Warning in case of missing declaration of variables.  Error messages
  in case of erroneous level specification [<lv>] or option values for
  ["Cuts"] or ["Mask"].
return: {<subt>, <zero>}
  <subt> -- equivalence groups of unique non-zero subsets,
  <zero> -- maximal vanishing subsets of lines.
dependencies:
  CanonGroups[], FilterVanishingGroup[], SortGroups[].
tags:
  >usage<, >level<, >cuts<, >mask< and Polynomial::variables.
version:
  2013-06-20 (reworked Alexey's version),
  2014-01-16 (minor modifications),
  2014-08-?? (changed iteration scheme).
description:
  By means of its alpha-representations, the structure of line subsets
  for a given integral family is computed.
notes:
- To be used in conjunction with AlphaRepresentation[].
- The alpha-parameters <vs> must be given explicitly in form of a list
  for looking at the right subsets.
- Complete sub-groups to a given symmetric group may appear by
  construction and are not filtered out (cf. SymmetricGroups[]).
- Maybe some speed-up is possible by comparing additional simple fields
  before the complete polynomials (as done for <nv>), e.g. appearing
  masses or kinematic invariants (cf. CanonGroups)?
*)

(* ---- definition -------------------------------------------------- *)

Options[IdenticalGroups] =
{"Cuts" -> None,
 "Mask" -> None,
 Parallel -> False,
 Verbosity -> False};

IdenticalGroups::usage = "\
IdenticalGroups[{<U>, <W>}, [<vars>], [<lvl>], [opts]] returns for the \
alpha-polynomials <U> and <W> with alpha-parameters [<vars>] the pair \
{<subt>, <zero>}, with identical groups of subsets <subt> and \
vanishing subsets <zero>.  <zero> contains lists of integers referring \
to positions in [<vars>], each list meaning a scaleless subset.  \
<subt> contains a list of such lists, each meaning a group of subsets \
considered to be equivalent.
In case no variables [<vars>] are specified, Variables[{<U>, <W>}] is \
assumed.  Optionally a level specification can be given via [<lvl>].
Via the option \"Cuts\" a list of valid cuts can be supplied in order \
to neglect subsets without any cut in the first place.  The option \
\"Mask\" allows to restrict inspected subsets to certain parameters \
indicated by a boolean list, e.g. those not corresponding to \
irreducible numerators.";

IdenticalGroups::level = "\
Warning: No valid level specification for subsets given by `1`, the \
default All will be used.
Use one of the following (symbols denote positive integers, nv is the \
number of available variables):
- All/Infinity -- all levels (equivalent to {1, nv}),
- n -- down to n elements (equivalent to {n, nv}),
- {n} -- exactly n elements,
- {nmin, nmax/Infinity} -- between nmin and nmax elements,
- {nmin, nmax/Infinity, dn} -- between nmin and nmax elements with \
distance dn.";

IdenticalGroups::cuts = "\
Warning: Option value for \"Cuts\" `1` is not of the form \
{{__Integer}...} and will be ignored.";

IdenticalGroups::mask = "\
Warning: Option value for \"Mask\" `1` is not of the form {(True | \
False)..} with sufficient length and will be ignored.";

(* main *)
IdenticalGroups[
  ps:{_, _}, vs_List:{}, lv_:All,
    opts:OptionsPattern[]] :=
  Module[
    {cts,msk,pl,vb, vt, nv, lw, sbs, sgs,zgs},

    (* -- options -- *)

    {cts, msk, pl, vb} = OptionValue[
      {"Cuts", "Mask", Parallel, Verbosity}];
    pl = pl /. $FlagRules;
    vb = vb /. VerbosityRules;
    Report[vb, 1, "IdenticalGroups[]."];

    (* check: variables *)
    vt = vs;
    If[vt === {},
       vt = Variables[ps];
       Message[Polynomial::variables, vt]];

    (* check: level *)
    nv = Length[vt];
    lw = lv /. If[
      Head[lv] === List,
      {All | Infinity -> nv},
      {All | Infinity -> {1, nv}, n_Integer -> {n, nv}}];
    If[!MatchQ[lw, {__Integer}] || Length[lw] > 3 ||
         !And @@ Positive /@ lw,
       Message[IdenticalGroups::level, lv];
       lw = {1, nv}];
    Report[vb, 2, "Level specification: ", lw, "."];

    (* check: "Cuts" *)
    If[cts =!= None && !MatchQ[cts, {{__Integer}...}],
       Message[IdenticalGroups::cuts, cts];
       cts = None];
    If[cts =!= None,
       Report[vb, 2, "Cuts: ", cts, "."]];

    (* check: "Mask" *)
    If[msk =!= None &&
         (!MatchQ[msk, {(True | False)..}] || Length[msk] < nv),
       Message[IdenticalGroups::mask, msk];
       msk = None];
    If[msk =!= None,
       Report[vb, 2, "Mask: ", msk, "."]];

    (* -- classification -- *)

    (* subsets: descending length *)
    sbs = Subsets[Range[nv], lw];
    Report[vb, 2, "Inspecting ", Length[sbs], " subsets."];
    Report[vb, 3, "Subsets:\n  ", TableForm[sbs], "."];

    (* filter: "Cuts" *)
    If[cts =!= None,
       sbs = CutsSelectGroups[sbs, cts];
       Report[vb, 2, "Filtered via cuts to ", Length[sbs], " subsets."];
       Report[vb, 3, "Subsets:\n  ", TableForm[sbs], "."]];

    (* filter: "Mask" *)
    If[msk =!= None,
       sbs = Select[sbs, And @@ msk[[#]] & ];
       Report[vb, 2, "Filtered via mask to ", Length[sbs], " subsets."];
       Report[vb, 3, "Subsets:\n  ", TableForm[sbs], "."]];

    (* inspect all subsets *)
    sgs = If[pl, ParallelMap, Map][CanonGroups[ps, vt, #, 1] & , sbs];
    Report[vb, 2, "Inspected all subsets."];

    (* identical groups: gather by search keys, rearrange *)
    sgs = GatherBy[sgs, First];
    sgs = Part[#, 1, 1] -> Join @@ (Last /@ #) & /@ sgs;
    Report[vb, 2, "Found ",  Length[sgs], " distinct groups."];

    (* vanishing group: find supersets, sort *)
    zgs = FilterVanishingGroup[0 /. sgs /. 0 -> {}];
    Report[vb, 2, "Found ", Length[zgs], " vanishing subsets."];

    (* identical groups: clean scaleless and search keys, sort *)
    sgs = SortGroups[Last /@ DeleteCases[sgs, 0 -> _]];

    (* result: identical groups, vanishing group *)
    {sgs, zgs}];

(* trap *)
IdenticalGroups[___] :=
  (Message[IdenticalGroups::usage];
   Abort[]);

(* ---- samples ----------------------------------------------------- *)

(* sample input/call:
# on-shell, 1-loop, non-planar box, 1 mass
ap = {x[1], x[2], x[3], x[4]};
ar = AlphaRepresentation[
  {mh^2 + k1^2,
   (p1 - k1)^2,
   (p1 + p2 - k1)^2,
   (p2 - k1)^2},
  {k1},
  GeneratedParameters -> ap,
  Constraints -> {p1^2 -> 0, p2^2 -> 0, p1*p2 -> -s/2}];

IdenticalGroups[ar, ap]
*)

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

(* For further examples cf. "Check/Polynomial.m" *)

(* --- SymmetricGroups ---------------------------------------------- *)

(* ---- description ------------------------------------------------- *)

(* SymmetricGroups[{<ps>, [<vs>], [<lv>], [opts]]
arguments:
  <ps> -- list of two expressions for the U- and W-polynomials.
(optional):
  [<vs>] -- list of symbols for alpha-parameters,
  [<lv>] -- standard level specification ("levelspec") for lengths of
    subsets to be considered (cf. >level<), by default All.
options:
  "Cuts" -- list of integer lists denoting cuts (cf. >cuts<),
  "Mask" -- boolean list as mask (cf. >mask<),
  [Method] -- one or list of the strings "Order", "Filter" (cf.
    $SymmetricGroupsMethods).  Without "Order" or "Filter" inspected
    groups of subsets are returned directly.  "Order" causes the groups
    to be returned after imposing a canonic ordering on them and within
    their subsets.  The default "Filter" implies ordered groups for
    discarding groups of subsets already contained in groups of
    supersets (thus not contributing any new symmetry).
output:
  Warning in case of missing declaration of variables.  Error messages
  in case of erroneous level specification [<lv>] or option values for
  ["Cuts"], ["Mask"] or [Method].
return: {<symm>, <zero>}
  <symm> -- symmetric groups of non-zero subsets,
  <zero> -- vanishing subsets of lines.
dependencies:
  CanonGroups[],
  CutsSymmetricGroups[],
  MaskSymmetricGroups[], SubsetVanishingGroup[],
  SortSymmetricGroups[], SortGroups[],
  FilterSymmetricGroups[], FilterVanishingGroup[].
symbols:
  $SymmetricGroupsMethods.
tags:
  From IdenticalGroups: >usage<, >level<, >cuts<, >mask< and
  Method::invalid, Polynomial:::variables.
version:
  2013-1?-?? (fork from IdenticalGroups[]),
  2014-01-16 (minor modifications),
  2014-08-?? (changed iteration scheme).
description:
  By means of the alpha-representation and its different possible
  canonical reorderings, the symmetry structure of line subsets for a
  given integral family is computed.
notes:
- Cf. IdenticalGroups[].
*)

(* ---- definition -------------------------------------------------- *)

$SymmetricGroupsMethods =
{"Symm", "Subt", "Only", "Order", "Filter"};

(* TODO:
"Symm": all canonical orderings searched
"Subt": cross-subtopology mappings allowed
"Only": keep single representations
*)

Options[SymmetricGroups] =
{"Cuts" -> None,
 "Mask" -> None,
 Method -> {"Symm", "Subt", "Filter"},
 Parallel -> False,
 Verbosity -> False};
(* Cf. $SymmetricGroupsMethods *)

SymmetricGroups::usage = "\
SymmetricGroups[{<U>, <W>}, [<vars>], [<lvl>], [opts]] returns for the \
alpha-polynomials <U> and <W> with alpha-parameters [<vars>] the pair \
{<symm>, <zero>}, with symmetric groups of subsets <symm> and \
vanishing subsets <zero>.  <zero> contains lists of integers referring \
to positions in [<vars>], each list meaning a scaleless subset.  \
<symm> contains a list of such lists, each meaning a group of subsets \
considered to be symmetric.
In contrast to the unique subsets <subt> returned by \
IdenticalGroups[], <symm> includes also all possible reorderings of \
these subsets.
In case no variables [<vars>] are specified, Variables[{<U>, <W>}] is \
assumed.  Optionally a level specification can be given via [<lvl>].
Via the option \"Cuts\" a list of valid cuts can be supplied in order \
to neglect subsets without any cut and to split symmetric groups \
finally in such a way that transitions between different \
cut-structures are forbidden.  The option \"Mask\" again allows to \
split symmetric groups, but in such a way that transitions between \
certain parameters, indicated by a boolean list, are restricted, \
e.g. between actual denominators and irreducible numerators.
The option [Method] expects a keyword string (list):
- {} -- return inspected groups of subsets directly,
- \"Order\" -- impose a canonic ordering on groups and within their \
subsets,
- \"Filter\" -- discard groups of subsets already contained in groups \
of supersets (implying ordered groups) and not contributing new \
symmetries.
The default is {\"Filter\"}.";

SymmetricGroups::level =
  IdenticalGroups::level;

SymmetricGroups::cuts =
  IdenticalGroups::cuts;

SymmetricGroups::mask =
  IdenticalGroups::mask;

(* main *)
SymmetricGroups[
  ps:{_, _}, vs_List, lv_:All,
    opts:OptionsPattern[]] :=
  Module[
    {cts,msk,me,pl,vb, vt, nv, lw, cp, sbs, sgs,zgs, sep,sel},

    (* -- options -- *)

    {cts, msk, me, pl, vb} = OptionValue[
      {"Cuts", "Mask", Method, Parallel, Verbosity}];
    pl = pl /. $FlagRules;
    vb = vb /. VerbosityRules;
    Report[vb, 1, "SymmetricGroups[]."];

    (* check: variables *)
    vt = vs;
    If[vt === {},
       vt = Variables[ps];
       Message[Polynomial::variables, vt]];

    (* check: level *)
    nv = Length[vt];
    lw = lv /. If[
      Head[lv] === List,
      {All | Infinity -> nv},
      {All | Infinity -> {1, nv}, n_Integer -> {n, nv}}];
    If[!MatchQ[lw, {__Integer}] || Length[lw] > 3 ||
         !And @@ Positive /@ lw,
       Message[SymmetricGroups::level, lv];
       lw = {1, nv}];
    Report[vb, 2, "Level specification: ", lw, "."];

    (* check: "Cuts" *)
    If[cts =!= None && !MatchQ[cts, {{__Integer}...}],
       Message[SymmetricGroups::cuts, cts];
       cts = None];
    If[cts =!= None,
       Report[vb, 2, "Cuts: ", cts, "."]];

    (* check: "Mask" *)
    If[msk =!= None &&
         (!MatchQ[msk, {(True | False)..}] || Length[msk] < nv),
       Message[SymmetricGroups::mask, msk];
       msk = None];
    If[msk =!= None,
       Report[vb, 2, "Mask: ", msk, "."]];

    (* check: Method *)
    me = Flatten[{me}];
    cp = Complement[me, $SymmetricGroupsMethods];
    If[cp =!= {},
       Message[Method::invalid, cp, $SymmetricGroupsMethods];
       me = Complement[me, cp]];
    Report[vb, 2, "Method: ", me, "."];

    (* -- symmetry search -- *)

    (* subsets: descending length *)
    sbs = Subsets[Range[nv], lw];
    Report[vb, 2, "Inspecting ", Length[sbs], " subsets."];
    Report[vb, 3, "Subsets:\n  ", TableForm[sbs], "."];

    (* filter: "Cuts" *)
    If[cts =!= None,
       sbs = CutsSelectGroups[sbs, cts];
       Report[vb, 2, "Filtered to ", Length[sbs], " subsets."];
       Report[vb, 3, "Subsets:\n  ", TableForm[sbs], "."]];

    (* Method: {"Symm"} *)
    (* inspect all reorderings of subsets *)
    sgs = If[pl, ParallelMap, Map][
      CanonGroups[ps, vt, #, If[MemberQ[me, "Symm"], -1, 1]] & , sbs];
    Report[vb, 2, "Inspected all subsets."];

    (* Method: {"Subt"} *)
    (* symmetric groups: gather by search keys, rearrange *)
    sep = {First[#], Sort[Part[#, 2, 1]]} & ;
    If[MemberQ[me, "Subt"], sep = First];
    sgs = GatherBy[sgs, sep];
    sgs = Part[#, 1, 1] -> Join @@ (Last /@ #) & /@ sgs;
    Report[vb, 2, "Found ",  Length[sgs], " distinct groups."];
    Report[vb, 3, "Numbers of lines, elements:\n  ",
           TableForm[{Length[First[#]], Length[#]} & /@ sgs], "."];

    (* scaleless group *)
    zgs = Union @@ Cases[sgs, HoldPattern[0 -> z_] -> z];
    Report[vb, 2, "Found ", Length[zgs], " vanishing subsets."];
    Report[vb, 3, "Numbers of lines: ", Length /@ zgs, "."];

    (* identical groups: clean scaleless, single subsets, search keys *)
    sel = Length[#] > 1 & ;
    If[MemberQ[me, "Only"], sel = True & ];
    sgs = Select[Last /@ DeleteCases[sgs, 0 -> _], sel];
    Report[vb, 2, "Found ",  Length[sgs], " symmetric groups."];
    Report[vb, 3, "Numbers of lines, elements:\n  ",
           TableForm[{Length[First[#]], Length[#]} & /@ sgs], "."];

    (* enforce: "Cuts" *)
    If[cts =!= None,
       sgs = CutsSymmetricGroups[sgs, cts];
       Report[vb, 2, "Cuts give ",  Length[sgs], " symmetric groups."];
       Report[vb, 3, "Numbers of lines, elements:\n  ",
              TableForm[{Length[First[#]], Length[#]} & /@ sgs], "."]];

    (* enforce: "Mask" *)
    If[msk =!= None,

       sgs = MaskSymmetricGroups[sgs, msk];
       Report[vb, 2, "Mask gives ",  Length[sgs], " symmetric groups."];
       Report[vb, 3, "Numbers of lines, elements:\n  ",
              TableForm[{Length[First[#]], Length[#]} & /@ sgs], "."];

       zgs = SubsetVanishingGroup[zgs, Join @@ Position[msk, True]];
       Report[vb, 2, "Mask gives ",  Length[zgs], " vanishing subsets."]

     ];

    (* Method: {} *)
    If[Intersection[{"Order", "Filter"}, me] === {},
       Return[{sgs, zgs}]];

    Report[vb, 4, "Symmetric groups:\n  ", TableForm[sgs], "."];
    Report[vb, 4, "Vanishing subsets:\n  ", TableForm[zgs], "."];

    (* -- sort subset groups -- *)

    sgs = SortSymmetricGroups[sgs];
    Report[vb, 2, "Sorted ", Length[sgs], " symmetric groups."];

    zgs = SortGroups[zgs];
    Report[vb, 2, "Sorted ", Length[zgs], " vanishing subsets."];

    (* Method: {"Order"} *)
    If[Intersection[{"Filter"}, me] === {},
       Return[{sgs, zgs}]];

    Report[vb, 4, "Symmetric groups:\n  ", TableForm[sgs], "."];
    Report[vb, 4, "Vanishing subsets:\n  ", TableForm[zgs], "."];

    (* -- filter subset groups -- *)

    sgs = FilterSymmetricGroups[sgs];
    Report[vb, 2, "Filtered to ", Length[sgs], " symmetric groups."];
    Report[vb, 3, "Numbers of lines, elements:\n  ",
           TableForm[{Length[First[#]], Length[#]} & /@ sgs], "."];

    zgs = FilterVanishingGroup[zgs];
    Report[vb, 2, "Filtered to ", Length[zgs], " vanishing subsets."];
    Report[vb, 3, "Numbers of lines: ", Length /@ zgs, "."];

    (* result: symmetric groups, vanishing group *)
    {sgs, zgs}];

(* trap *)
SymmetricGroups[___] :=
  (Message[SymmetricGroups::usage];
   Abort[]);

(* ---- samples ----------------------------------------------------- *)

(* sample input/call:
# on-shell, 1-loop, non-planar box, 1 mass
ap = {x[1], x[2], x[3], x[4]};
ar = AlphaRepresentation[
  {mh^2 + k1^2,
   (p1 - k1)^2,
   (p1 + p2 - k1)^2,
   (p2 - k1)^2},
  {k1},
  GeneratedParameters -> ap,
  Constraints -> {p1^2 -> 0, p2^2 -> 0, p1*p2 -> -s/2}];

SymmetricGroups[ar, ap]
*)

(* sample output:
{# groups of symmetric subsets with sub-groups filtered out
 {{{1, 2, 3, 4}, {1, 4, 3, 2}}},
 # vanishing subsets of lines
 {{2, 3}, {3, 4}}}
*)

(* For further examples cf. "Check/Polynomial.m" *)

(* --- PartialFractioning ------------------------------------------- *)

(* ---- description ------------------------------------------------- *)

(* PartialFractioning[<ps>, [<vs>], [<cs>]]
arguments:
  <ps> -- list of expression equal to zero denoting relations,
(optional):
  [<vs>] -- list of occuring variables,
  [<cs>] -- list of remaining constants.
output:
  Error messages or warnings in case of missing variables or undeclared
  variables or constants.
return:
  List of substitution rules reducing any possible monomial involving
  [<vs>] to canonical form, i.e. permforming partial fraction
  decomposition.
  $Failed in case of too few variables given.
tags:
  >constants< (no constants given in [<cs>])
  >variables< (no variables given in [<vs>], Polynomial::variables),
  >declare< (too few variables or constants declared in [<vs>] or
    [<cs>]).
version:
  2013-09-27 (rewrite),
  2014-01-17 (minor modifications),
  2014-09-12 (mainly Report[]).
description:
  First, the relations <ps> are completed by relations for the inverse
  variables to <vs>, thus forming the ideal.  Then the Groebner basis is
  constructed and rewritten in form of a terminating set of substitution
  rules.  Therefore a weighting for monomials must be introduced.
notes:
- comp[...] compares two vectors weighted with a matrix
  lexicographically.
- This function can be applied in order to simplify expressions
  involving linearly dependent denominators in Feynman integrals.
- Cf. [Pak:2011xt] for detailed information.
*)

(* ---- definition -------------------------------------------------- *)

Options[PartialFractioning] =
{Verbosity -> False};

PartialFractioning::usage = "\
PartialFractioning[<rels>, [<vars>], [<cons>]] returns a list of \
terminating replacement rules performing partial fraction \
decomposition of monomials in variables [<vars>] among which hold the \
linear relations <rels> involving possibly additional constants \
[<cons>].
In case no variables [<vars>] are specified, Variables[<rels>] is \
assumed.";

PartialFractioning::constants = "\
Warning: `1` will be used as constant(s).";

PartialFractioning::declare = "\
Not all variables or constants have been declared properly in `1` or \
`2`.";

(* main *)
PartialFractioning[
  ps_List, vs_List:{}, cs_List:{}, opts:OptionsPattern[]] :=
  Module[
    {vb, vt,ds, nv,nc, v,vi, c,cw, va,fa, rs,ri, id, i,j, wm,wn, gb,
     comp, rule},

    vb = OptionValue[Verbosity] /. VerbosityRules;
    Report[vb, 1, "PartialFractioning[]."];

    {vt, ds} = {vs, cs};

    (* check: constants *)
    If[ds === {} && vt =!= {},
       ds = Complement[Variables[ps], vt];
       Message[PartialFractioning::constants, ds]];

    (* check: variables *)
    If[vt === {},
       vt = Complement[Variables[ps], cs];
       Message[Polynomial::variables, vt]];

    (* -- variables -- *)

    (* numbers: variables, constants *)
    {nv, nc} = Length /@ {vt, cs};

    (* wrap: inverse variables, constants *)
    vi = v[1/#] & /@ vt;
    cw = c /@ cs;
    If[vb > 0,
       c = Unique["Global`c"];
       v = Unique["Global`v"]];

    (* all: variables, factors *)
    va = Join[vt, vi];
    fa = Join[va, cw];
    Report[vb, 2, "All factors:\n  ", fa, "."];

    (* substitution rules, their inverse *)
    rs = MapThread[#1 -> #2 &, {Join[1/vt, cs], Join[vi, cw]}];
    ri = Reverse /@ rs;

    (* -- main steps -- *)

    (* ideal: add relations for inverse variables *)
    id = Join[ps /. rs, MapThread[#1*#2 - 1 & , {vt, vi}]];
    Report[vb, 3, "Ideal:\n  ", TableForm[id], "."];

    (* reduction order: monomial weight matrix *)
    wm = Table[
      If[j >= i && i <= 2*nv && j <= 2*nv || i == j, 1, 0],
      {i, 2*nv + nc}, {j, 2*nv + nc}];
    wn = Take[wm, {1, 2*nv}, {1, 2*nv}];
    Report[vb, 3, "Weight matrix:\n  ", MatrixForm[wm], "."];

    (* -- Groebner basis -- *)

    gb = Quiet[Check[
      GroebnerBasis[id, fa, MonomialOrder -> wm],
      Null]];

    (* check: constants undeclared *)
    If[gb === Null,
       Message[PartialFractioning::declare, vt, cs];
       Return[$Failed]];

    (* select non-trivial relations *)
    gb = Select[gb, (# /. ri) =!= 0 & ];
    Report[vb, 4, "Groebner basis:\n  ", TableForm[gb], "."];

    (* -- rewrite: polynomials -> rules -- *)

    (* helper: compare two weighted vectors *)
    comp[av_, bv_] := Module[
      {aw = wn . av, bw = wn . bv, c, r = 0},
      Do[Which[
        Greater @@ c, r = +1; Break[],
        Less @@ c, r = -1; Break[]],
         {c, Transpose[{aw, bw}]}];
      r];

    (* helper: rewrite as substitution *)
    rule[eq_] := Module[
      {crs, max, lhs, rhs},
      crs = CoefficientRules[eq, va];
      (* find maximum vector *)
      max = Fold[
        If[comp[First[#1], First[#2]] > 0, #1, #2] &,
        First[crs], Rest[crs]];
      (* rewrite as monomial without coefficient *)
      lhs = Inner[Power, va, First[max], Times];
      Report[vb, 3, "Maximum monomial: ", Last[max]*lhs, "."];
      (* express via remaining terms *)
      rhs = (lhs*Last[max] - eq)/Last[max];
      Report[vb, 3, "Remaining terms:\n  ", rhs, "."];
      lhs -> rhs];

    (* result: apply helper, inverse, rearrange *)
    Expand[Factor[rule /@ gb /. ri]]];

(* trap *)
PartialFractioning[___] :=
  (Message[PartialFractioning::usage];
   Abort[]);

(* ---- samples ----------------------------------------------------- *)

(* sample call:
PartialFractioning[
  {d1 - d2 - d3 + d4 + s - mh^2},
  {d1, d2, d3, d4},
{s, mh^2}]
*)

(* sample output:
{d4 -> -d1 + d2 + d3 + mh^2 - s,
 d3/d4 -> 1 + d1/d4 - d2/d4 - mh^2/d4 + s/d4,
 d2/(d3*d4) -> d3^(-1) - d4^(-1) + d1/(d3*d4) - mh^2/(d3*d4)
   + s/(d3*d4),
 d1/(d2*d3*d4) -> -(1/(d2*d3)) + 1/(d2*d4) + 1/(d3*d4) + mh^2/(d2*d3*d4)
   - s/(d2*d3*d4),
 1/(d1*d2*d3*d4) -> 1/(d1*d2*d3*(mh^2 - s)) - 1/(d1*d2*d4*(mh^2 - s))
   - 1/(d1*d3*d4*(mh^2 - s)) + 1/(d2*d3*d4*(mh^2 - s))}
*)

(* --- package end -------------------------------------------------- *)

Protect["TopoID`Polynomial`*"];

Scan[
  SetAttributes[#, {ReadProtected}] & , Select[
    Symbol /@ Names["TopoID`Polynomial`*"], Head[#] === Symbol & ]];

End[];

EndPackage[];

(* ------------------------------------------------------------------ *)



(* --- TODO:

-

*)
