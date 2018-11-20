CLS

INPUT N

N$ = LTRIM$(STR$(N))
l = LEN(N$)
DIM v(l), d$(l)

FOR i = l TO 1 STEP -1
 c$ = MID$(N$, i, 1)
 v(i) = VAL(c$)
 c = c + 1
 

 
 SELECT CASE c
  CASE 1: RESTORE ones
  CASE 2: RESTORE tens
  CASE ELSE: RESTORE hundred
 END SELECT

 SELECT CASE v(i)

 CASE 1, 2, 3:
  READ d1$
  d$(i) = STRING$(v(i), d1$)

 CASE 4:
 READ d1$, d2$
 d$(i) = d1$ + d2$

 CASE 5:
 READ t$, t$
 d$(i) = t$

 CASE 6, 7, 8:
 READ d1$, d2$
 d$(i) = d2$ + STRING$(v(i) - 5, d2$)

 CASE 9:
 READ d1$, d2$, d3$
 d$(i) = d1$ + d3$

END SELECT

NEXT


ones:
DATA I,V,X
tens:
DATA X,L,C
hundred:
DATA C,D,M

FOR i = 1 TO l
 PRINT d$(i);
NEXT
END


