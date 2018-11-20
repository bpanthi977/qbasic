DECLARE FUNCTION amicable! (a!, b!)
DECLARE FUNCTION factors! (n!, a!())
CLS

INPUT a, b

o = amicable(a, b)
p = amicable(b, a)
IF (o = 1 AND p = 0) OR (o = 0 AND p = 1) THEN
 PRINT "One sided amicable"
ELSEIF (o = 1 AND p = 1) THEN
 PRINT "Two sided amicable"
ELSE
 PRINT "No amicable"
END IF
END




 

FUNCTION amicable (a, b)
DIM c(20)
m = factors(a, c())

FOR i = 1 TO m
 FOR j = i TO m
  s = s + c(j)
  IF s = b THEN
  amicable = 1
  PRINT a; "--> ";
  FOR k = i TO j - 1: PRINT c(k); "+"; : NEXT
  PRINT c(j); "="; b
  EXIT FUNCTION
  END IF
 NEXT
 s = 0
NEXT

END FUNCTION

FUNCTION factors (n, a())
FOR i = 1 TO n
 IF n MOD i = 0 THEN c = c + 1: a(c) = i
NEXT
factors = c
END FUNCTION

