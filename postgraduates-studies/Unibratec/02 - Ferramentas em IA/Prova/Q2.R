# Questão 2
# Crie duas bases de dados aleatórias. A primeira composta dos elementos de uma Distribuição Uniforme 
# composta por 70 elementos entre os valores 5 e 10. A segunda composta dos elementos de uma 
# Distribuição Normal de média 7,00 e desvio padrão 1,50 composta por 70 elementos. Em seguida, 
# calcule o que se pede, para cada sequência.

b1 = runif(70,5,10)
b2 = rnorm(70,7,1.5)

# a) Média x
x1 = mean(b1)
x2 = mean(b2)
print(x1)
print(x2)

# b) Variância
print(var(b1, na.rm = TRUE))
print(var(b2, na.rm = TRUE))

# c) Desvio Padrão
print(sd(b1))
print(sd(b2))

# d) Mediana
print(median(b1, na.rm = TRUE))
print(median(b2, na.rm = TRUE))

# e) Quartis (25%, 50%, 75% e 100%)
x = quantile(b1, c(0.25, 0.5, 0.75, 1), type=1)
print(x)

# f) Coeficiente de Dispersão
cd1 = sd(b1)*100/mean(b1)
print(cd1)
cd2 = sd(b2)*100/mean(b2)
print(cd2)

# g) Coeficiente de Correlação entre as duas bases (interprete, com um comentário simples, o resultado encontrado)
print(cor(b1, b2)) #são inversamente proporcionais (r < 0), mas com uma iteração bem fraca entre as variáveis (-0.05 está distante de -1)














