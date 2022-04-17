# Questão 01
x1 = runif(15, 7, 18)
x2 = rnorm(15, 8, 0.5)

# a) Média
mean(x1)
mean(x2)

# b) Variância
var(x1)
var(x2)

# c) Desvio Padrão
sd(x1)
sd(x2)

# d) Mediana
median(x1)
median(x2)

# e) Quartis
quantile(x1)
quantile(x2)

# f) Coeficiente de Dispersão
cd1 = (sd(x1)/mean(x1))*100
cd2 = (sd(x2)/mean(x2))*100



#Questão 02
a = c(1, 2, 7, -7, -1, 7, 8, -3, 9, 6, 5, 0, 1, 0, 4, 5)
b = c(0, 5, 1, 2, 7, -5, 4, -3, 9, -2, -2, 7, 0, 4, 5, 8)

A = matrix(a, 4, 4, byrow = TRUE)
B = matrix(b, 4, 4, byrow = TRUE)

# a) A+B
A+B
A-B
A%*%B
det(A)
det(B)
---> faltou letra f)




