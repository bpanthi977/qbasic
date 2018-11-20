DECLARE FUNCTION area! (p1 AS ANY, p2 AS ANY, p3 AS ANY)
DECLARE FUNCTION cpointQuad! (l1 AS ANY, med AS ANY, tmp() AS ANY)
DECLARE FUNCTION pointSlope! (p AS ANY, m!, ans AS ANY)
DECLARE FUNCTION correctCollision! (s AS ANY, ang!, tmp() AS ANY, cpoint AS ANY)
DECLARE FUNCTION cplane! (cpoint AS ANY, med AS ANY, segm AS ANY)
DECLARE FUNCTION drawNormal! (plane AS ANY, p1 AS ANY, nplane AS ANY)
DECLARE FUNCTION twopoint! (p AS ANY, l1 AS ANY)
DECLARE FUNCTION incidentAng! (s AS ANY, cpoint AS ANY, nplane AS ANY)
DECLARE FUNCTION calculateAng! (ang!, s1!, s2!)
DECLARE FUNCTION asin! (v!)
DECLARE FUNCTION hpoint! (p AS ANY)
DECLARE FUNCTION drawQuad! (q AS ANY, clr!)
DECLARE FUNCTION findMedia! (p AS ANY, med AS ANY)
DECLARE FUNCTION quadArea! (q AS ANY)
DECLARE FUNCTION nextPoint! (p AS ANY, ang!, np AS ANY, n!)
DECLARE FUNCTION drawLine! (p1 AS ANY, p2 AS ANY, n!)
DECLARE FUNCTION drawLineEq! (eqn AS ANY)
DECLARE FUNCTION eql! (p1 AS ANY, p2 AS ANY)
DECLARE FUNCTION an! (cpoint AS ANY, s AS ANY)
DECLARE FUNCTION mid! (p1 AS ANY, p2 AS ANY, ans AS ANY)

'ON ERROR GOTO ErrorHandler

SCREEN 12

LOCATE 5, 40: PRINT "LIGHT"
LOCATE 6, 50: PRINT "v. 1.3"
LOCATE 28, 50: PRINT "Developer Bibek Panthi"
a$ = INPUT$(1)

CLS
INPUT "Enter .Obj File To Process ", file$
IF file$ = "" THEN file$ = "2prism.obj"
file$ = "light"+ CHR$(92) + file$
CLS
messagex = 25
messagey = 5
drawNormals = 1
velocityDecrease = 0
'$INCLUDE: 'light/physics.'
     
FOR i = 1 TO media
a = drawQuad(medias(i).quad, medias(i).clr)
medias(i).area = quadArea(medias(i).quad)
'PRINT medias(i).area
NEXT


DIM s AS coord, sp AS coord, sb AS coord
DIM SHARED inci AS INTEGER

INPUT #1, null
                       FOR j = 1 TO raysCount          'number of rays
s = sp: ang = angp
raycheck:
   
IF EOF(1) = 0 THEN
INPUT #1, calc$, sb.x, sb.y, angB
angB = angB * pi / 180
ELSEIF j = 1 THEN
GOTO ErrorHandler               'no value is for the first ray
END IF

IF calc$ = "a" THEN
s = sb
ang = angB
ELSEIF calc$ = "r" THEN
IF j = 1 THEN GOTO ErrorHandler
s.x = s.x + sb.x
s.y = s.y + sb.y
ang = ang + angB
ELSEIF calc$ = "m" THEN
INPUT #1, message$
LOCATE messagex, messagey: PRINT message$
SLEEP
LOCATE 1, 1
GOTO raycheck
END IF

IF ang = 0 THEN ang = .000000001#
IF ang * 180 / pi = 180 THEN ang = 180.0001# * pi / 180
sp = s: angp = ang
count = 0
endCount = 40                           'number of collisions per rays
                                        'to prevent infinite loops

        DO UNTIL INKEY$ = "e" OR count >= endCount
count = count + 1

        'scientific calculation

a = findMedia(s, med)
a = pointSlope(s, TAN(ang), sray)
'PRINT sray.a, sray.b, sray.c
a = cpointQuad(sray, med, tmp())
a = correctCollision(s, ang, tmp(), cpoint)
a = cplane(cpoint, med, plane)
'a = hpoint(cpoint)
a = drawNormal(plane, cpoint, nplane)
inAng = (incidentAng(s, cpoint, nplane))
ang = an(cpoint, s)
a = nextPoint(cpoint, ang, npoint, 1)
a = findMedia(npoint, medR)
reAng = calculateAng(inAng, med.speed, medR.speed)

        'PRINT ang * 180 / pi; inAng * 180 / pi; reAng * 180 / pi;

ang2 = reAng
IF inci < 0 THEN ang2 = -reAng
npoint = nplane.p2
IF ABS(inci) = 2 THEN npoint = nplane.p1
ang2 = ang2 + an(npoint, cpoint)

'PRINT ang2 * 180 / pi; inci
a = nextPoint(cpoint, ang2, npoint, 1)

ang = an(npoint, cpoint)

        FOR i = 1 TO media
        medias(i).speed = medias(i).speed - medias(i).vDec
        NEXT


        ' mathematics finished
        'now graphics
a = drawLine(s, cpoint, 1)              'incidentRay
IF drawNormals = 1 THEN
a = drawLine(nplane.p1, nplane.p2, 0)   'normal
END IF

s = npoint
'ang = reAng

LOOP
       
ending:

        IF (velocityDecrease) <> 0 THEN
        medias(2).speed = medias(2).speed - velocityDecrease
        medias(4).speed = medias(2).speed
        END IF

        NEXT j

PRINT
PRINT "ending pogram"
END

CONST BADFILENAME = 53
ErrorHandler:
'CLS
PRINT
PRINT
PRINT

   IF ERR = BADFILENAME THEN
      ' abort the program
      PRINT "File "; UCASE$(file$); " not found."
   ELSE
      ' some other error, so print message and abort
      PRINT "Unrecoverable error--"; ERR
   END IF
   PRINT "Press any key to exit"
   a$ = INPUT$(1)
   END



FUNCTION an (cpoint AS coord, s AS coord)
a = (cpoint.y - s.y)
b = (cpoint.x - s.x)
bang = ATN(a / b) * 180 / pi
'PRINT bang, a, b
bang = ABS(bang)
IF b < 0 AND a >= 0 THEN
bang = 180 - bang
ELSEIF b < 0 AND a < 0 THEN
bang = 180 + bang
ELSEIF b >= 0 AND a < 0 THEN
bang = 360 - bang
END IF
bang = bang * pi / 180
an = bang
END FUNCTION

FUNCTION area (p1 AS coord, p2 AS coord, p3 AS coord)
DIM a AS DOUBLE
'LINE (o.x + p1.x, o.y - p1.y)-(o.x + p2.x, o.y - p2.y)
'LINE (o.x + p1.x, o.y - p1.y)-(o.x + p3.x, o.y - p3.y)
'LINE (o.x + p3.x, o.y - p3.y)-(o.x + p2.x, o.y - p2.y)

a = (1 / 2) * ((p1.x * p2.y - p1.y * p2.x) + (p2.x * p3.y - p2.y * p3.x) + (p3.x * p1.y - p3.y * p1.x))

area = ABS(a)
END FUNCTION

FUNCTION asin (v)
v = ABS(v)
IF v > 1 THEN v = 1
IF v < 0 THEN v = 0
asin = ATN(v / (1 - v ^ 2) ^ (1 / 2))
END FUNCTION

FUNCTION calculateAng (ang, s1, s2)

's = SGN(ang)
ang = ABS(ang)
ri = s1 / s2

IF ang = 0 THEN calculateAng = 0: EXIT FUNCTION
IF (ri <= 1) THEN
critical = asin(ri)
        IF (ang > critical) THEN
        calculateAng = pi - ang: EXIT FUNCTION
        END IF
END IF

ag = asin(SIN(ang) / ri)
'ang = s * ang
'IF s = -1 THEN ag = 2 * pi - ag
calculateAng = ag

END FUNCTION

FUNCTION correctCollision (s AS coord, ang, tmp() AS coord, cpoint AS coord)

DIM g AS coord
DIM q AS INTEGER
g = tmp(1)
gdis = 1D+308


ang = ang * 180 / pi

IF ang >= 0 AND ang <= 90 THEN
        q = 1
ELSEIF ang > 90 AND ang <= 180 THEN
        q = 2
ELSEIF ang > 180 AND ang <= 270 THEN
        q = 3
ELSE
        q = 4
END IF
        FOR i = 1 TO 4

IF (tmp(i).x = s.x AND tmp(i).y = s.y) THEN GOTO nxt:

x = tmp(i).x - s.x
y = tmp(i).y - s.y

IF ABS(x) = x AND ABS(y) = y AND q = 1 THEN
        f = 1
        
ELSEIF x = -ABS(x) AND y = ABS(y) AND q = 2 THEN
        f = 1
ELSEIF x = -ABS(x) AND y = -ABS(y) AND q = 3 THEN
        f = 1
ELSEIF x = ABS(x) AND y = -ABS(y) AND q = 4 THEN
        f = 1
END IF

IF f = 1 THEN
'IF count = 3 THEN a = hpoint(tmp(i))
IF ((x ^ 2 + y ^ 2) < gdis) THEN
g.x = tmp(i).x
g.y = tmp(i).y
gdis = ((g.x - s.x) ^ 2 + (g.y - s.y) ^ 2)
END IF

END IF

nxt:
f = 0

NEXT i
     
ang = ang * pi / 180
cpoint = g
'a = (cpoint)
'IF count = 3 THEN PRINT cpoint.x, cpoint.y
END FUNCTION

FUNCTION cplane (cpoint AS coord, med AS medium, segm AS segment)
i = 0
'a = hpoint(cpoint)
DIM p1 AS coord, p2 AS coord
GOTO proces:

chek:

m1 = (cpoint.y - p1.y) / (cpoint.x - p1.x)
m2 = (p2.y - cpoint.y) / (p2.x - cpoint.x)
       'PRINT m1; m2, p1.x; p1.y, p2.x; p2.y
IF m1 = m2 THEN f = 1
IF eql(p1, p2) = 1 THEN f = 0

proces:
i = i + 1
IF (f = 1) THEN
'PRINT count, I - 1
'a = hpoint(p1)
'a = hpoint(p2)

segm.p1 = p1
segm.p2 = p2
EXIT FUNCTION
END IF

IF (i = 1) THEN
        p1 = med.quad.p1: p2 = med.quad.p2
ELSEIF (i = 2) THEN
        p1 = med.quad.p2: p2 = med.quad.p3
ELSEIF (i = 3) THEN
        p1 = med.quad.p3: p2 = med.quad.p4
ELSEIF (i = 4) THEN
        p1 = med.quad.p4: p2 = med.quad.p1
ELSE
EXIT FUNCTION
END IF

f = 0
GOTO chek:


END FUNCTION

FUNCTION cpointQuad (l1 AS eqn, med AS medium, tmp() AS coord)
'PRINT med.nam
DIM l2 AS eqn
DIM p1 AS coord, p2 AS coord
DIM sg AS segment

i = 0
GOTO Proc:
chk:
a = twopoint(sg, l2)

'PRINT l2.a, l2.b, l2.c
'IF eql(sg.p1, sg.p2) = 1 THEN PRINT "eql fnd"
'PRINT med.nam

tmp(i).y = (l2.a * l1.c - l1.a * l2.c) / (l1.a * l2.b - l2.a * l1.b)
tmp(i).x = (l1.b * tmp(i).y + l1.c) / -l1.a
                'PRINT tmp(i).y, tmp(i).x
IF (l1.a * l2.b - l2.a * l1.b) = 0 THEN tmp(i) = inf
'PRINT l2.a, l2.b, l2.c
Proc:
i = i + 1

IF (i = 1) THEN
        sg.p1 = med.quad.p1: sg.p2 = med.quad.p2
ELSEIF (i = 2) THEN
        sg.p1 = med.quad.p2: sg.p2 = med.quad.p3
ELSEIF (i = 3) THEN
        sg.p1 = med.quad.p3: sg.p2 = med.quad.p4
ELSEIF (i = 4) THEN
        sg.p1 = med.quad.p4: sg.p2 = med.quad.p1
ELSE
EXIT FUNCTION
END IF

GOTO chk:
END FUNCTION

FUNCTION drawLine (p1 AS coord, p2 AS coord, n)
IF p1.x = inf.x OR p2.x = inf.x THEN EXIT FUNCTION
IF n = 1 THEN
'PRINT p1.x, p1.y, p2.x, p2.y
LINE (o.x + p1.x, o.y - p1.y)-(o.x + p2.x, o.y - p2.y)
ELSE
LINE (o.x + p1.x, o.y - p1.y)-(o.x + p2.x, o.y - p2.y), , , 6553
END IF
END FUNCTION

FUNCTION drawLineEq (eqn AS eqn)

IF eqn.a < eqn.b THEN

FOR i = (-swidth / 2) TO swidth / 2
py = (eqn.a * i + eqn.c) / (-eqn.b)
PSET (o.x + i, o.y - py)

NEXT i

ELSE

FOR i = sheight / 2 TO -sheight / 2 STEP -1
px = (eqn.b * i + eqn.c) / -eqn.a
PSET (o.x + px, o.y - i)
NEXT i

END IF




END FUNCTION

FUNCTION drawNormal (plane AS segment, p1 AS coord, nplane AS segment)
n = 30

IF (plane.p1.x - plane.p2.x) = 0 THEN
a1 = pi / 2
ELSE
a1 = ATN((plane.p1.y - plane.p2.y) / (plane.p1.x - plane.p2.x))
END IF

ang = a1 + (pi / 2)
a = n * COS(ang)
b = n * SIN(ang)
nplane.p1.x = p1.x - a
nplane.p1.y = p1.y - b
nplane.p2.x = p1.x + a
nplane.p2.y = p1.y + b


'PRINT a1 * 180 / pi
'a = hpoint(nplane.p1)
'a = hpoint(nplane.p2)
 

END FUNCTION

FUNCTION drawQuad (q AS quad, clr)

LINE (o.x + q.p1.x, o.y - q.p1.y)-(o.x + q.p2.x, o.y - q.p2.y), clr
LINE (o.x + q.p2.x, o.y - q.p2.y)-(o.x + q.p3.x, o.y - q.p3.y), clr
LINE (o.x + q.p3.x, o.y - q.p3.y)-(o.x + q.p4.x, o.y - q.p4.y), clr
LINE (o.x + q.p4.x, o.y - q.p4.y)-(o.x + q.p1.x, o.y - q.p1.y), clr
'          a = hpoint(q.p1)
'          a = hpoint(q.p2)
'          a = hpoint(q.p3)
'          a = hpoint(q.p4)
         
DIM p AS coord, g AS coord, c AS coord
'dim  s AS coord, r AS coord
'DIM e AS eqn, m AS segment

a = mid(q.p1, q.p4, p)
a = mid(q.p2, q.p3, g)
'a = mid(q.p1, q.p2, r)
'a = mid(q.p4, q.p3, s)
'm.p1 = p: m.p2 = g
'a = twopoint(m, e)
'PRINT e.a, e.b, e.c
'c1 = (e.a * r.x - e.b * r.y + e.c)
'c2 = (e.a * s.x - e.b * s.y + e.c)
'k = -c1 / c2
'c.x = (k * s.x + r.x) / (k + 1)
'c.y = (k * s.y + r.y) / (k + 1)

a = mid(p, g, c)

'LINE (o.x + q.p1.x, o.y - q.p1.y)-(o.x + c.x, o.y - c.y), clr
'PRINT c.x, c.y
cm$ = "BM " + LTRIM$(STR$(INT(o.x + c.x))) + "," + LTRIM$(STR$(INT(o.y - c.y)))
'PRINT cm$
'DRAW cm$

cm$ = MID$(STR$(clr), 2)
cm$ = "P" + cm$ + "," + cm$
DRAW cm$

END FUNCTION

FUNCTION eql (p1 AS coord, p2 AS coord)
IF (p1.x = p2.x) AND (p1.y = p2.y) THEN
eql = 1
ELSE
eql = -1
END IF
END FUNCTION

FUNCTION findMedia (p AS coord, med AS medium)

FOR i = 1 TO media
med = medias(i)
a1 = area(p, med.quad.p1, med.quad.p2)
a2 = area(p, med.quad.p2, med.quad.p3)
a3 = area(p, med.quad.p3, med.quad.p4)
a4 = area(p, med.quad.p4, med.quad.p1)
a = (a1 + a2 + a3 + a4)

IF VAL(STR$(a)) = VAL(STR$(med.area)) THEN
EXIT FUNCTION
END IF


IF CSNG(a) = CSNG(med.area) THEN
EXIT FUNCTION
END IF

NEXT i
PRINT "out";

endCount = count
med = medias(0)

END FUNCTION

FUNCTION hpoint (p AS coord)
CIRCLE (o.x + p.x, o.y - p.y), 2, 2
END FUNCTION

FUNCTION incidentAng (s AS coord, cpoint AS coord, nplane AS segment)
a1 = an(nplane.p1, cpoint)
a2 = an(s, cpoint)
vangle = a2 - a1
'PRINT "|"; vangle * 180 / pi; "|";
inci = 1
IF vangle < 0 THEN vangle = 2 * pi + vangle
vangle = ABS(vangle)
IF (vangle > 3 * pi / 2) THEN vangle = 2 * pi - vangle: inci = -1
IF (vangle > pi) THEN vangle = vangle - pi: inci = inci * 2
IF (vangle > pi / 2) THEN vangle = pi - vangle: inci = -2
incidentAng = vangle
'PRINT inci
END FUNCTION

FUNCTION mid (p1 AS coord, p2 AS coord, ans AS coord)
ans.x = (p1.x + p2.x) / 2
ans.y = (p1.y + p2.y) / 2
END FUNCTION

FUNCTION nextPoint (p AS coord, ang, np AS coord, n)
'a = hpoint(p)
'PRINT ang * 180 / pi
a = ang
'PRINT COS(an), SIN(an)
'IF (ang <= 0) THEN a = pi - ABS(a)
np.x = p.x + COS(a) * n
np.y = p.y + SIN(a) * n
END FUNCTION

FUNCTION pointSlope (p AS coord, m, ans AS eqn)
ans.a = m
ans.b = -1
ans.c = -m * p.x + p.y
END FUNCTION

FUNCTION quadArea (q AS quad)
qa = (1 / 2) * ((q.p1.x * q.p2.y - q.p2.x * q.p1.y) + (q.p2.x * q.p3.y - q.p2.y * q.p3.x) + (q.p3.x * q.p4.y - q.p3.y * q.p4.x) + (q.p4.x * q.p1.y - q.p4.y * q.p1.x))
quadArea = ABS(qa)
END FUNCTION

FUNCTION twopoint (p AS segment, l1 AS eqn)
n = p.p2.x - p.p1.x
m = p.p2.y - p.p1.y
l1.a = -m
l1.b = n
l1.c = (-p.p1.y * n) + (m * p.p1.x)

END FUNCTION

