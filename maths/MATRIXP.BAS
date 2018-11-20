CLS
INPUT "Dimension of matrix A(m,n) :-", m, n
INPUT "Dimension of matrix B(n,o) :-", n, o

DIM a(m, n), b(n, o)
CLS
PRINT "Enter Data For Matrix A"
w = 5

FOR i = 1 TO m
 FOR j = 1 TO n
 LOCATE i + 1, j * w: INPUT "", a(i, j)
NEXT j, i

CLS
PRINT "Enter data for matrix B"
FOR i = 1 TO n
 FOR j = 1 TO o
  LOCATE i + 1, j * w: INPUT "", b(i, j)
NEXT j, i

CLS
' Now Multiplication Procedure
DIM c(m, o)

FOR i = 1 TO m
 FOR j = 1 TO o
  t = 0
  FOR k = 1 TO n
   t = t + a(i, k) * b(k, j)
  NEXT
  c(i, j) = t
NEXT j, i

' Now display Data
LOCATE (m + 1) \ 2, 1: PRINT "A X B = C = "
margin = 10

FOR i = 1 TO m
 FOR j = 1 TO o
  LOCATE i, j * w + margin: PRINT c(i, j)
NEXT j, i
END








