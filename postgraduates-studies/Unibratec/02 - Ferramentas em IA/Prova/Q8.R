# Questão 08

# a) Plote um gráfico entre as variáveis distância (eixo X) e velocidade (eixo Y).
plot(cars$dist, cars$speed, type = "p", xlab = "Distância", ylab = "Velocidade", col = "purple")

# b) Faça uma regressão linear considerando Y=f(X)
reg = lm (cars$speed ~ cars$dist, data = cars)
coef1 = coef(reg)
abline(coef1)

# c) Com base na regressão preveja o valor da velocidade para as distâncias 25, 75 e 200.
coef1[1] + coef1[2] * 25 # = 12.4231
coef1[1] + coef1[2] * 70 # = 19.87364 
coef1[1] + coef1[2] * 200 # = 41.39742

# d) Qual dos valores da letra c não é considerado uma boa referência? Justifique em poucas
# palavras.
coef1[1] + coef1[2] * 200 # = 41.39742 não é uma boa referência porque atinge um intervalo não confiável.


