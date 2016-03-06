<< ./init.m;
<< TopoID`Calculate`;



(Run["cat <<EOF | ~/dev/bin/lap-dev/fd.pl '-5,-5,-5,-5,-5,-5,-5' '5,5,5,5,5,5,5' TOP
=
+ TOP(1,-3,-2,4,2,0,3)*acc(1)
;
EOF\n"])

min = {-5, -5, -5, -5, -5, -5, -5};
max = {+5, +5, +5, +5, +5, +5, +5};

min = {-5, -5, -5};
max = {+5, +5, +5};

$LaportaEncode[0, {1,-3,-2,4,2,0,3}, min, max]
$LaportaDecode[%, min, max]

$LaportaEncode[0, {1,1,1,0,-1,1,1}, min, max]
$LaportaEncode[0, {1,1,1,0,-1,1,1}, min, max]
$LaportaEncode[0, {1,1,1,0,-1,1,1}, min, max]
