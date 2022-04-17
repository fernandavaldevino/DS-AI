# # 3/4 - (8^(1/4) + 6^3) * ((1/8)^2) + 14
# # 
# # x=34
# # 
# # y = 1:10
# # 
# # seq(3,15,2)
# # 
# # rep(3:6, 2)
# # 
# # #
# # x = runif(5, 1, 10)
# # 
# # #distribuição normal
# # x = rnorm(6, 4, 1)
# # 
# # #função concatenação
# # z = c("Papel", "Fernanda", "unibratec")
# # y = c(10, 20, 30, 40)
# 
# # normal = rnorm(10, 6.5 , 0.5)
# # x1 = min(normal)
# # x2 = max(normal)
# # x3 = range(normal)
# # x4 = sort(normal)
# # x11 = median(normal)
# # x5 = rank(normal)
# # x6 = length(normal)
# # x7 = nchar(normal)
# # x8 = sum(normal)
# # x9 = normal[6]
# # x10 = mean(normal)
# # x12 = var(normal)
# # x13 = quantile(normal)
# # x14 = class(normal)
# 
# 
# 
# # servicos = c("lavagem a seco", "lavagem normal", "lavagem ecológica", "aspiração")
# # precos = c(39.90, 29.90, 34.90, 9.90)
# # lava_jato = data.frame(Serviços = servicos, Preços = precos)
# # lava_jato1 = list(servicos, precos, 10)
# # 
# # w = matrix(1:9, 3, byrow=TRUE)
# # 
# # a = array(1:15, c(2, 2, 4))
# 
# 
# a = array(1:4, c(2,2,1))
# b = array(-1:2, c(2,2,1))
# 
# A = matrix(1:4, 2, byrow = TRUE)
# B = matrix(-1:2, 2, byrow = TRUE)
# A
# B
# A+B
# A-B
# A*B
# A/B
# A%%B
# t(A)
# t(B)
# cor(A,B)
# rbind(A)
# cbind(A)
# det(A)
# A[1,2]
# B[2,1]
# A[1,]
# B[,2]
# 
# 
a = c(1, 4, -2, 3, 0, 1, 4, 5, -3)
b = c(-1, 6, 3, 0, 1, 7, -3, 2, 5)
A = matrix(a, 3, byrow = TRUE)
B = matrix(b, 3, byrow = TRUE)

print(A)
print(B)
# 
# C = A%*%B   #multiplicação matricial: operador -> %*%
# C
# det(A)
# C[3,2]




# #lendo e exportando em xlsx
# var = read.xlsx("LavaJato.xlsx", 1)
# write.xlsx(var, "plan.xlsx")
# 
# #importando em xlsx e exportando em csv
# write.csv(var, "filexlsx.csv")
# 
# #lendo e exportando em csv
# varcsv = read.csv( "LavaJato.csv" )
# write.csv(varcsv, "file.csv")
# 
# #lendo e escrevendo docs de texto



eixox = iris[,2]
eixoy = iris[,4]
plot(eixox, eixoy, type="p", main = "PLANTA IRIS", xlab = "Sepal Width", ylab = "Petal Width", col = "blue")

especie = iris [, "Species"]
text(eixox, eixoy, labels = especie, cex = 0.8, adj = c(0, -1))


plot(eixox, eixoy, type = "n", main = "PLANTA IRIS", xlab = "Sepal Width", ylab = "Petal Width")
points(eixox[1:50], eixoy[1:50], col = "2", cex = 2, pch = 19)
points(eixox[51:100], eixoy[51:100], col = "3", cex = 2, pch = 1)
points(eixox[101:150], eixoy[101:150], col = "4", cex = 2, pch = 19)
legend(locator(1), c("Setosa", "Versicolor", "Virgínica"),pch = c(19, 1, 19), col = c(2, 3, 4), cex = 1.2)

plot(eixoy, type = "n", xlab = "Índice", ylab = "Sepal.Width")
lines(iris["Sepal.Width"])

qtde = tapply(rep(1,150), iris["Species"], sum)
barplot(qtde, xlab = "Espécies", ylab = "Frequência")

pie(qtde)

boxplot(iris[1:4])
summary(iris)

#histograma
#só lê vetor "linha". No caso da tabela Iris, tem que transformá-la em linha pra poder ler
colors = c("blue", "green", "red", "purple", "yellow")
hist(t(iris["Petal.Width"]), 5, main = "Histograma do Petal.Width separado em 5 faixas",
     xlab = "Petal.Width", ylab = "Frequência", col = colors)


vetor1 = c(-3, -1, 0, 1, 3, 6)
vetor2 = c(0, 1, 3, 3, 1, 4, -5)
vetor3 = c(1, 2, 6, 4, 5, 6, 6, 7)

mean(vetor1)
mean(vetor2)
mean(vetor3)

median(vetor1)
median(vetor2)
median(vetor3)

x1 = table(vetor1)
x1[x1 == max(x1)]

x2 = table(vetor2)
x2[x2 == max(x2)]

x3 = table(vetor3)
x3[x3 == max(x3)]


#correlação
x1 = iris[1:50, "Sepal.Width"]
y1 = iris[1:50, "Sepal.Length"]
cor(x1,y1)

x2 = iris[1:50, "Petal.Width"]
y2 = iris[1:50, "Petal.Length"]
cor(x2, y2)


x3 = iris[1:50, "Sepal.Width"]
y3 = iris[101:150, "Petal.Length"]
cor(x3, y3)

