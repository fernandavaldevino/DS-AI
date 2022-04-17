#Atividade 01

#Questão o1

dados_dist_unif <- runif(15,7,18)

md1 <- mean(dados_dist_unif)
var1 <- var(dados_dist_unif)
desv_padr1 <- sd(dados_dist_unif)
mediana1 <- median(dados_dist_unif)
quartis1 <- quantile(dados_dist_unif)
coef_disp1 <- (100*desv_padr1)/(md1)


dados_dist_unif

md1
var1 
desv_padr1
mediana1
quartis1
coef_disp1

dados_dist_norm <- rnorm(15,8,0.5)

md2 <- mean(dados_dist_norm)
var2 <- var(dados_dist_norm)
desv_padr2 <- sd(dados_dist_norm)
mediana2 <- median(dados_dist_norm)
quartis2 <- quantile(dados_dist_norm)
coef_disp2 <- (100*desv_padr2)/(md2)


dados_dist_norm

md2
var2 
desv_padr2
mediana2
quartis2
coef_disp2


#Questão o2

d_A=c(1,2,7,-7,-1,7,8,-3,9,6,5,0,1,0,4,5) #dados da matriz A
A=matrix(d_A,nrow=4,byrow = TRUE) #organiza os dados por linha

d_B=c(0,5,1,2,7,-5,4,-3,9,-2,-2,7,0,4,5,8) #dados da matriz B
B=matrix(d_B,nrow=4,byrow = TRUE) #organiza os dados por linha

C=A+B  #soma
D=A-B #subtração
E=A%*%B #Produto matricial
f=det(A)
g=det(B)

h=E[2,3]


#Questão o3
wine_red <- read.csv("winequality-red.csv", header = TRUE, sep = ';')

#a)
dados_sum <- summary(wine_red)
dados_sum

#b)
ph <- wine_red[,9]
qualidade <- wine_red[,12]

plot(qualidade,ph,type="p",main="Vinho Tinto", xlab = "Qualidade", ylab = "pH", col="blue")


#c)
quant_pH <- tapply(rep(1,1599), ph,sum)
barplot(quant_pH, xlab="pH", ylab = "Frequência")

quant_qual <- tapply(rep(1,1599), qualidade,sum)
barplot(quant_qual, xlab="Qualidade", ylab = "Frequência")

alcool <- wine_red[,11]

quant_alcool <- tapply(rep(1,1599), alcool,sum)
barplot(quant_alcool, xlab="Alcool", ylab = "Frequência")

#d)
acucar_resid <- wine_red[,4]
hist(t(acucar_resid), 10, main = "",xlab="Açucar Residual",ylab="Frequência")

volat_acida <- wine_red[,2]
hist(t(volat_acida), 10, main = "",xlab="Volatilidade Ácida",ylab="Frequência")

#e)
acidade_citrica <- wine_red[,3]
boxplot(acidade_citrica,main = "",xlab="Acidade Cítrica",ylab="Frequência")

densidade <- wine_red[,8]
boxplot(densidade,main = "",xlab="Densidade",ylab="Frequência")

#f)
corr_1 <- cor(qualidade,ph)
corr_1

corr_2 <- cor(densidade,alcool)
corr_2

corr_3 <- cor(acucar_resid,qualidade)
corr_3

#interpretação dos Coeficientes de Correlação consulte o site abaixo
# http://leg.ufpr.br/~silvia/CE003/node74.html


#Questão 03

moda <- function(dados) {
  md <- table(dados)
  md[md == max(md)]
}

seq_1 <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15) #amodal
moda(seq_1)

seq_2 <- c(-4,-3,-3,-2,-1,0,3,5,7,1,2,6,9,11,12)#modal
moda(seq_2)

seq_3 <- c(-4,-3,-2,-1,-2,0,3,5,7,1,2,0,9,11,12)#bimodal
moda(seq_3)

