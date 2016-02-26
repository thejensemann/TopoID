(* -- "Setup.m": Initialization for a Process ----------------------- *)

BeginPackage[
  "TopoID`Setup`",
  {"TopoID`Common`",  (* TODO: ??? *)
   "TopoID`System`"}];

(* needs: "syst.m" *)

(* provided functions:
ScalarProducts -- possible scalar products,
SetupPattern -- setup data pattern,
SetupQ -- query function,
Setup -- corresponding constructor,
*)

ClearAll[ScalarProducts];

ClearAll[s, m];

ClearAll[ps,ks,vs, ss,is, xs,cs,rs, ms,zs, np,nk,nv,ns, nl,ne];

ClearAll[SetupPattern];

ClearAll[SetupQ];

ClearAll[Setup];

Begin["`Private`"];

(* --- ScalarProducts ----------------------------------------------- *)

(* ---- description ------------------------------------------------- *)

(* ScalarProducts[[<ps>], <ks>, [<cs>]]
arguments:
  [<ps>], <ks> -- lists for the external and internal momenta.
(optional):
  [<ps>] -- can be missing for absent external momenta, i.e. problems
    involving only vacuum diagrams,
  [<cs>] -- list of rules imposing constraints on the scalar products.
return:
  A list of scalar products formed by external and internal momenta
  [<ps>] and <ks>, respecting the constraints [<cs>] if given, in
  lexicalic order.
version:
  2012-09-20 (no adaptions necessary),
  2013-08-05 (added [<cs>] and more),
  2014-01-08 (minor modifications).
description:
  Scalar products formed exclusively by external momenta are kinematic
  invariants, i.e. independent of the loop integration.  All other
  scalar products may potentially occur in the integrands during the
  evaluation of the problem at hand.  Hence, there must be at least one
  internal momentum on the input.
*)

(* ---- definition -------------------------------------------------- *)

ScalarProducts::usage =
  "GENERATED.";

(* main *)
ScalarProducts[ps:{___}, ks:{__}, cs:{___Rule}] :=
  Select[Union[Flatten[Outer[Times, Join[ps, ks], ks] /. cs]],
         Intersection[ks, Variables[#]] =!= {} & ];

(* overload: missing external momenta *)
ScalarProducts[ks:{__}, cs:{___Rule}] :=
  ScalarProducts[{}, ks, cs];

(* overload: missing constraints *)
ScalarProducts[ps:{___}, ks:{__}] :=
  ScalarProducts[ps, ks, {}];

(* overload: missing external momenta, constraints *)
ScalarProducts[ks:{__}] :=
  ScalarProducts[{}, ks, {}];

(* trap *)
ScalarProducts[___] :=
  (Message[ScalarProducts::usage];
   Abort[]);

(* ---- samples ----------------------------------------------------- *)

(* sample calls:
# without constraints
ScalarProducts[{k1, k2, k3}]
ScalarProducts[{p1, p2}, {k1, k2}]

# with constraints
ScalarProducts[{p1, p2, p3, p4}, {k1}, {p3 -> p1, p4 -> p2}]
*)

(* sample output:
# without constraints
{k1^2, k1*k2, k2^2, k1*k3, k2*k3, k3^2}
{k1^2, k1*k2, k2^2, k1*p1, k2*p1, k1*p2, k2*p2}

# with constraints
{k1^2, k1*p1, k1*p2}
*)

(* --- 1. Setup ----------------------------------------------------- *)

(* ---- wrappers ---------------------------------------------------- *)

s::usage = "\
Symbol introduced as wrapper s[<int>] in the constructor function \
Setup[] which represents a specific scalar product in the setup of \
external and internal momenta and under the given kinematic \
constraints.  Such scalar products must involve at least one loop \
momentum.  They appear in the >ss< and >is< entries of an object \
matching SetupPattern[].  These give the relations of symbols and \
explicit expressions for scalar products.
Examples:
  ss -> {s[1] -> k1^2, s[2] -> k1*p1, s[3] -> k1*p2},
  is -> {k1^2 -> s[1], k1*p1 -> s[2], k1*p2 -> s[3]}.";

m::usage = "\
Wrapper m[<id>] introduced in the constructor function Setup[] and \
present in the constraints entry >cs< in an object matching \
SetupPattern[].  The rules in >cs< are applied, e.g., after reading \
diagrams via GetDiagrams[] and wrapping the particle ID strings <id> \
in m[<id>].
Examples:
- set the up quark mass to zero
    m[\"qu\"] -> 0,
- assign the Higgs boson the mass mh
    m[\"hb\"] -> mh.";

(* ---- keys -------------------------------------------------------- *)

(* N.B.:
- Entries in setup data pattern:
   {      momenta (<= cs):
    ps,   -- external,
    ks,   -- internal,
    vs,   -- all vectors (<= ps, ks),
          scalar products:
    ss,   -- symbolic rules (<= vs),
    is,   -- reversed set (<= ss),
          kinematics and notation:
    xs,   -- appearing constants,
    cs,   -- all constraints,
    rs,   -- transformations,
         redundant (<= cs):
    ms,  -- particle masses,
    zs,  -- zero expressions,
         numbers of (momenta):
    np,  -- externals (<= ps),
    nk,  -- internals (<= ks),
    nv,  -- all vectors (<= vs),
    ns,  -- scalar products (<= ss),
         numbers of (graph):
    nl,  -- legs (<= ps, cs),
    ne}  -- edges (<= nl, nk).
*)

(* ---- SetupPattern ------------------------------------------------ *)

SetupPattern::usage = "\
Data type pattern describing the setup of a calculation.
It consists of entries storing information about:
- involved momenta (external and internal),
- possibly occuring scalar products,
- kinematics and notation.
The pattern imposes certain restrictions upon an object:
- External momenta in >ps< may be missing, but at least one internal \
momentum is required in >ks<.
- Hence, from all momenta in >vs<, there results at least one possible \
scalar product in >ss< and >is<.
- There should be one expression for a fundamental scale minimally in \
>xs<, else everything equals zero.  Unless, the problem has been \
rendered dimensionless beforehand, of course.
- >cs< contains all frequently applied rules, i.e. optional \
assignments of masses (via the wrapper m[]) and kinematic constraints.
- The entries in >rs< for transformation rules are purely optional too.
- As are those for the (expressions of linear) masses in >ms< and \
those for expressions equal to zero in >zs<.
An object of this type is usually generated automatically by calling \
the constructor function Setup[] which is invoked for example via:
  Setup[{Externals -> {p1, p2, p3, p4},
         Internals -> {k1, k2},
         Masses -> {\"hb\" -> mh},
         Constants -> {s, mh^2},
         Constraints -> {p3 -> p1, p4 -> p2,
                         p1^2 -> 0, p2^2 -> 0,
                         p1*p2 -> -s/2},
         Rules -> {mh^2 -> s*x}}].";

(* data type pattern *)
SetupPattern[] :=
{ps -> _List,
 ks -> {__},
 vs -> {__},
 (* -- *)
 ss -> {__Rule},
 is -> {__Rule},
 (* -- *)
 xs -> _List,
 cs -> {(_Rule | _RuleDelayed)...},
 rs -> {(_Rule | _RuleDelayed)...},
 (* -- *)
 ms -> _List,
 zs -> _List,
 (* -- *)
 np -> _Integer,
 nk -> _Integer,
 nv -> _Integer,
 ns -> _Integer,
 (* -- *)
 nl -> _Integer,
 ne -> _Integer};

(* trap *)
SetupPattern[___] :=
  (Message[SetupPattern::usage];
   Abort[]);

(* ---- SetupQ ------------------------------------------------------ *)

SetupQ::usage = "\
SetupQ[<set>] gives >True< if <set> matches SetupPattern[] and >False< \
otherwise.";

(* pass *)
SetupQ[set_] :=
  MatchQ[set, SetupPattern[]];

(* trap *)
SetupQ[___] :=
  (Message[SetupQ::usage];
   Abort[]);

(* ---- input ------------------------------------------------------- *)

Externals::usage = "\
Setup: List of symbols denoting external momenta which can also be \
empty.
Example:
  Externals -> {p1, p2, p3, p4}.";

Internals::usage = "\
Setup: List of symbols denoting internal momenta which must not be \
empty.
Example:
  Internals -> {k1}.";

Masses::usage = "\
Setup: List of (symbolic) rules assigning masses to fields where no \
rule implies masslessness.
Example:
  Masses -> {\"hb\" -> mh}.";

Constants::usage = "\
Setup: List of expressions of independent constants appearing in the \
calculation.
Example:
  Constants -> {s, mh^2}.";

Constraints::usage = "\
Setup: List of rules stating all the kinematic constraints of the \
process.  These rules may be applied repeatedly.
Example:
  Constraints -> {p3 -> p1, p4 -> p2,
                  p1^2 -> 0, p2^2 -> 0,
                  p1*p2 -> -s/2}.";

Rules::usage = "\
Setup: List of rules introducing variables at the stage of FORM \
code.  These rules may beapplied repeatedly.
Example:
  Rules -> {mh^2 -> s*x}.";

(* ---- Setup ------------------------------------------------------- *)

(* ----- description ------------------------------------------------ *)

(* Setup[<rls>]
arguments:
  <rls> -- list of rules defining the setup for a calculation (cf. notes
    and the sample input shown below).
output:
  Error messages are generated if the settings in <rls> are not defined
  properly.
return:
  An enhanced setup description list for internal use in the program is
  returned (cf. SetupPattern[]).
  $Aborted is returned in case of erroneous settings.
dependecies:
  DefaultList[], CheckList[], ScalarProducts[].
symbols:
  >Externals<, >Internals<, >Masses<, >Constants<, >Constraints<,
  >Rules<.
tags:
  >nscps< (inconsistency in number of scalar products).
version:
  2012-09-20 (adopted new data format),
  2013-08-06 (extended with graph information),
  2014-01-09 (minor modifications).
description:
  Based on a minimal input set, building up some generic data necessary
  for other routines of the TopoID package.
notes:
  >Externals<, >Internals< -- symbol lists for external and internal
    momenta,
  >Masses< -- symbolic rules assigning masses to fields/particles (no
    rule implies massless),
  >Constants< -- list of expressions for independent constants appearing
    in the calculation (N.B.: these are external to loop integrations),
  >Constraints< -- rules setting up the kinematics,
  >Rules< -- transformation rules to be applied repeatedly on various
    occasions during the execution of TopoID code (e.g. simplifications
    or introduction of some notation in terms of parameters/variables).
- For the total number of scalar products n_sps. (see e.g. Smirnov's
  book) combine each internal with each external momentum
    n_int.*n_ext.,
  and each internal with each internal momentum
    n_int.*(n_int. + 1)/2.
  This results in
    n_sps. = n_int.*(n_ext. + n_int./2 + 1/2).
- For the maximum number of edges in a graph n_max., assume vertices of
  degree three.  At one loop there are as many internal lines possible
  as the number of legs n_legs.  With each loop three additional
  propagators may emerge (add one and cut/split two).  This totals in
    n_max. = n_legs + 3*(n_int. - 1).
*)

(* ----- defintion -------------------------------------------------- *)

Setup::usage =
  "GENERATED.";

Defaults[Setup] =
{Externals -> {},    (* external momenta (=> ps, vs) *)
 Internals -> {k},   (* internal momenta (=> ks, vs) *)
 Masses -> {},       (* mass definitions (=> cs, ms) *)
 Constants -> {},    (* appearing constants (=> xs) *)
 Constraints -> {},  (* kinematic constraints (=> cs, zs) *)
 Rules -> {}};       (* transformation rules (=> rs) *)

Checks[Setup] =
{Externals -> _List,
 Internals -> {__},
 Masses -> {(_String -> _)...},
 Constants -> _List,
 Constraints -> {(_Rule | _RuleDelayed)...},
 Rules -> {(_Rule | _RuleDelayed)...}};

CheckMessages[Setup] =
{Externals ->
   "a list of symbols is needed, for example {p1, p2}",
 Internals ->
   "at least one loop momentum is required, e.g. {k1}",
 Masses ->
   "define masses in the manner of {\"hb\" -> mh}",
 Constants ->
   "appearing constants are expected, e.g. {s, mh^2}",
 Constraints ->
   "for instance {p1^2 -> 0, p2^2 -> 0, p1*p2 -> -s/2}",
 Rules ->
   "implement transformations as for example {mh^2 -> s*x}"};

Setup::nscps = "\
Warning: Inconsistency in total number of possible scalar products \
detected.  There should be `1`, but the following products were \
generated: `2`.";

Setup[rls___Rule] :=
  Module[
    {chk, cns, ext,int, next,nint,nvec, scpe, nscp, scps,scpr,scpi, set,
     mss,zrs, nleg,nedg},

    (* -- handle arguments -- *)

    chk = DefaultList[Setup, {rls}];

    Check[CheckList[Setup, chk], Abort[]];

    (* -- assemble canonical information -- *)

    (* rules from masses and kinematics *)
    cns = Join[
      MapAt[m, #, 1] & /@ (Masses /. chk), Constraints /. chk];

    (* treat momenta *)
    {ext, int} = Variables[# /. chk //. cns] &
      /@ {Externals, Internals};

    (* obtain their numbers *)
    {next, nint} = Length /@ {ext, int};
    nvec = next + nint;

    (* form possible scalar products expressions *)
    scpe = ScalarProducts[ext, int, cns];

    (* obtain and check their number *)
    nscp = nint*(next + nint/2 + 1/2);
    If[nscp =!= Length[scpe],
       Message[Setup::nscps, nscp, scpe];
       nscp = Length[scpe]];

    (* generate symbols and (inverse) rules *)
    scps = s /@ Range[nscp];
    scpr = MapThread[Rule, {scps, scpe}];
    scpi = Reverse /@ scpr;

    (* -- canonical information -- *)
    set =
    {ps -> ext, ks -> int,    (* externals, internals *)
     vs -> Join[ext, int],    (* all vectors together *)
     ss -> scpr, is -> scpi,  (* scalar product rules *)
     xs -> Constants /. chk,  (* constants of problem *)
     cs -> cns,               (* complete constraints *)
     rs -> Rules /. chk};     (* transformation rules *)

    (* -- obtain redundant information -- *)

    (* particle masses *)
    (*mss = DeleteCases[DeleteDuplicates[
      Last /@ (Masses /. chk)], 0 | Null];*)  (* TODO: remove this old version *)
    mss = DeleteCases[Variables[Last /@ (Masses /. chk)], Null];

    (* zero expressions *)
    zrs = First /@ Select[Constraints /. chk, Last[#] === 0 & ];

    (* graph numbers of legs and edges at most *)
    nleg = Length[Externals /. chk];
    nedg = nleg + 3*(nint - 1);

    (* -- append redundancies -- *)
    Join[
      set,
      {ms -> mss, zs -> zrs,     (* masses, zeros *)
       (* numbers of: *)
       np -> next, nk -> nint,   (* exts., ints. *)
       nv -> nvec, ns -> nscp,   (* vecs., pros. *)
       nl -> nleg, ne -> nedg}]  (* legs, edges *)
  ];

(* overload: lists *)
Setup[rls1___, rls_List, rls2___] :=
  Setup[rls1, Sequence @@ rls, rls2];

(* trap *)
Setup[___] :=
  (Message[Setup::usage];
   Abort[]);

(* ----- samples ---------------------------------------------------- *)

(* sample call:
Setup[
  Externals -> {p1, p2, p3, p4},
  Internals -> {k1},
  Masses -> {"gh" -> 0, "gl" -> 0, "hb" -> mh,
             "qd" -> 0, "qu" -> 0, "si" -> 0},
  Constants -> {s, mh^2},
  Constraints -> {p3 -> p1, p4 -> p2,
                  p1^2 -> 0, p2^2 -> 0,
                  p1*p2 -> -s/2},
  Rules -> {mh^2 -> s*x}]
*)

(* sample output:
{ps -> {p1, p2},
 ks -> {k1},
 vs -> {p1, p2, k1},
 ss -> {s[1] -> k1^2, s[2] -> k1*p1, s[3] -> k1*p2},
 is -> {k1^2 -> s[1], k1*p1 -> s[2], k1*p2 -> s[3]},
 xs -> {s, mh^2},
 cs -> {m["gh"] -> 0, m["gl"] -> 0, m["hb"] -> mh,
        m["qd"] -> 0, m["qu"] -> 0, m["si"] -> 0,
        p3 -> p1, p4 -> p2,
        p1^2 -> 0, p2^2 -> 0,
        p1*p2 -> -s/2},
 rs -> {mh^2 -> s*x},
 ms -> {mh},
 zs -> {p1^2, p2^2},
 np -> 2,
 nk -> 1,
 nv -> 3,
 ns -> 3,
 nl -> 4,
 ne -> 4}
*)






(* ------------------------------------------------------------------ *)

End[];

EndPackage[];

(* ------------------------------------------------------------------ *)
