INPUT "Enter roman number :", a$
a$ = UCASE$(a$)

l = LEN(a$)
DIM d$(l), v(l)

FOR i = 1 TO l
 c$ = MID$(a$, i, 1)
 d$(i) = c$
 RESTORE roman

 FOR j = 1 TO 7
  READ b$, n
  IF b$ = c$ THEN v(i) = n
 NEXT

NEXT

roman:
DATA I,1,V,5,X,10,L,50,C,100,D,500,M,1000

FOR i = 1 TO l - 1
 IF v(i) < v(i + 1) THEN v(i) = v(i) * -1
 s = s + v(i)
NEXT
s = s + v(i)


PRINT "Roman to decimal value: "; s
END



                   

