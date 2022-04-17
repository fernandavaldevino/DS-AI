# Questão 04

a = c(1, 4, 0, -6, -2, 6, 4, 3, -3, 4, 0, 1, 2, -1, 6, -5)
b = c(5, -4, -1, 5, 1, 3, 0, 1, 0, -3, 5, -2, 8, -4, 3, 2)
A = matrix(a, 4, byrow = TRUE)
B = matrix(b, 4, byrow = TRUE)

# a) C = (AB)ł >> Multiplicação Matricial, seguida de transposição
C = t(A%*%B)
print(C)

# b) d = det(A + 2B)
d = det(A + (2 * B))
print(d)

# c) e = (A² - 5B)34
xx = (A*A) - (5*B)
e = xx[3,4]
print(e)
