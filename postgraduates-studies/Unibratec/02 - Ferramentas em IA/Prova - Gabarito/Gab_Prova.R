#Gabarito Avalia��o

#Quest�o 01

exp = ((5/9)-6*((64)^(1/3)+((5^3)-100))+(((32)^(1/4)/5)-2*((3/5)^(-2))))
exp

#Quest�o 02

base1 <- runif(70, min=5, max=10) #base aleat�ria 1
base2 <- rnorm(70,mean = 7,sd=1.5)#base aleat�ria 2

#Base 01 - Distribui��o Uniforme
media1 <- mean(base1)              #m�dia
variancia1 <- var(base1)           #vari�ncia
desvio_padr1 <- sd(base1)          #desvio padr�o
mediana1 <- median(base1)          #mediana
quartis1_1 <- quantile(base1, probs = 0.25) #Q1-25%
quartis2_1 <- quantile(base1, probs = 0.50) #Q2-50%
quartis3_1 <- quantile(base1, probs = 0.75) #Q3-75%
quartis4_1 <- quantile(base1, probs = 1.00) #Q4-100%
coef_disp_1 <- (desvio_padr1*100)/media1      #Coeficiente de Dispers�o

#Base 02 - Distribui��o Normal
media2 <- mean(base2)              #m�dia
variancia2 <- var(base2)           #vari�ncia
desvio_padr2 <- sd(base2)          #desvio padr�o
mediana2 <- median(base2)          #mediana
quartis1_2 <- quantile(base2, probs = 0.25) #Q1-25%
quartis2_2 <- quantile(base2, probs = 0.50) #Q2-50%
quartis3_2 <- quantile(base2, probs = 0.75) #Q3-75%
quartis4_2 <- quantile(base2, probs = 1.00) #Q4-100%
coef_disp_2 <- (desvio_padr2*100)/media2      #Coeficiente de Dispers�o

#Correla��o entre as bases
correlacao <- cor(base1,base2) #coeficiente de correla��o

#por serem bases aleat�rias divergentes � de se esperar uma baixa correla��o entre
#as mesmas. cabe uma an�lise do aluno para cada caso.

#Quest�o 03

abacate <- read.csv("avocado.csv")

#letra a)
Resumo_abacate <- summary(abacate)

#letra b)
indice = abacate[,1]
datas = abacate[,2]
preco = abacate[,3]
vol_total = abacate[,4]
x4046_peq = abacate[,5]
x4225_med = abacate[,6]
x4770_grd = abacate[,7]
sacas_total = abacate[,8]
small <- abacate[,9]
large <- abacate[,10]
xlarge <- abacate[,11]
tipo = abacate[,12]
ano = abacate[,13]
regiao = abacate[,14]

#letra c)
arquivo <- data.frame(tipo,ano,regiao)
write.xlsx(arquivo, "abacate.xlsx")

#letra d)
plot(sacas_total,ano)

#letra e)
n1 = length(preco)
quantidade<-tapply(rep(1,n1),preco,sum)
barplot(quantidade,xlab="Pre�o",ylab="Frequ�ncia")

#letra f)
n2 = length(ano)
quantidade<-tapply(rep(1,n2),ano,sum)
pie(quantidade,main = "Frequ�ncia dos Anos")

#letra g)
hist(t(preco),main="Distribui��o dos Pre�os",xlab="Pre�o", ylab = "Frequ�ncia")
hist(t(ano),main="Distribui��o em Anos",xlab="Ano", ylab = "Frequ�ncia")

#letra h)
#Correla��o entre Pre�o e Ano
correl_1 <- cor(preco,ano) #baixa correla��o

#Correla��o entre Volume Total e Sacas Totais
correl_2 <- cor(vol_total,sacas_total) #alta correla��o

#Correla��o entre Sacas Totais e Ano
correl_3 <- cor(sacas_total,ano) #baixa correla��o

#Nos Slides 65, 66 e 67 da Aula 01 encontra-se um resumo sobre a interpreta��o
# dos valores dos Coeficientes de Correla��o

#Quest�o 04

A1 = c(1,4,0,-6,-2,6,4,3,-3,4,0,1,2,-1,6,-5)
A = matrix(A1,4,4,byrow = TRUE)

B1 = c(5,-4,-1,5,1,3,0,1,0,-3,5,-2,8,-4,3,2)
B = matrix(B1,4,4,byrow = TRUE)

C= t(A%*%B)
d=det(A+2*B)

E = A%*%A -5*B
e=E[3,4]

#Quest�o 05

exp <- function(x){
  
  a=1
  for(i in 1:99){
    
    a=a+(x^i)/factorial(i)
    
  }
  return(a)
}

e1 = exp(-4)
e2 = exp(6)


#Quest�o 06

X_NA1 <- read.xlsx("Tabela1.xlsx",1,colClasses = c("numeric","numeric"))

X_NA <- as.numeric(X_NA1$Quantidade) #l� os dados na forma de n�meros
                                          #necess�ria para as fun��es estat�sticas
Media_NA <- mean(X_NA, na.rm = TRUE)
Mediana_NA <- median(X_NA, na.rm = TRUE)

Moda <- table(X_NA)
Moda[Moda == max(Moda)]

Var_NA <- var(X_NA, na.rm = TRUE) 
Desvio_NA <- sd(X_NA, na.rm = TRUE)

itens = as.numeric(X_NA1[,1])
quantidade_dado = as.numeric(X_NA1[,2])

cor(itens,quantidade_dado,use = "na.or.complete")
#correla��o bem fraca

#Quest�o 07

#a)a)	Obter tr�s caras, sem importar a ordem, nos 6 lan�amentos de uma moeda justa.

S1 <-tosscoin(6, makespace = TRUE)        #espa�o amostral
E1 <- subset(S1,isin(S1,c("H","H","H")))  #evento
P1 <- Prob(E1)                            #probabilidade
P1


#b)	Obter quatro coroas, sem importar a ordem, no lan�amento de 7 vezes de uma moeda com p_cara = 0.75 e p_coroa= 0.25. 
#Use a fun��o iidspace para criar as probabilidades

S2 <- iidspace(c("H","T"), ntrials = 7, probs = c(0.75,0.25)) #espa�o amostral
E2 <- subset(S2, isin(S2, c("T","T","T","T"))) #evento
P2 <- Prob(E2)                                 #probabilidade
P2

#c)Obter a soma do resultado do lan�amento de 4 vezes de um dado com 8 faces maior que 23.

S3 <- rolldie(4, nsides = 8, makespace = TRUE)  #espa�o amostral
E3 <- subset(S3, X1 + X2 + X3 + X4 > 23)        #evento
P3 <- Prob(E3)                                  #probabilidade
P3

#d)	Obter uma carta (qualquer naipe) de valor entre 5 e 9 na retirada de um baralho com os coringas presentes.

S4 <- cards(jokers = TRUE, makespace = TRUE)
E4 <- subset(S4, rank %in% 5:9)
Prob(E4) 

#e)Considere tr�s lan�amentos de um dado justo de seis faces. Evento A (valores iguais) 
#e B (soma dos valores menor ou igual a 12). Calcule P(A|B) e P(B|A).

S5 <- rolldie(time = 3,nsides = 6, makespace = TRUE)
E5 <- subset(S5, (X1==X2 & X2==X3))
E6 <- subset(S5, X1+X2+X3<=12)
#P(A|B)
Prob(E5, given = E6)
#P(B|A)
Prob(E6, given = E5)

#Quest�o 08

Dado_Carros <-cars

#letra a)
Distancia <- Dado_Carros[,2] #coluna dist�ncia de frenagem
velocidade <- Dado_Carros[,1] #coluna velocidade
plot(Distancia,velocidade, main= "Dados Cars - Regress�o") #grafico de Dist�ncia x Velocidade


#letra b)
Beaver.lm <- lm(velocidade ~ Distancia, data = Dado_Carros) #regress�o linear
coeficientes<- coef(Beaver.lm) #coeficientes da regress�o
abline(coeficientes) #Gr�fico da regress�o

#letra c)
#Lei de Forma��o da Fun��o --> y(x)= coeficientes[1]+coeficientes[2]*x
est_1 <- coeficientes[1]+coeficientes[2]*25 #estimativa para distancia 25m
est_2 <- coeficientes[1]+coeficientes[2]*75 #estimativa para distancia 75m
est_3 <- coeficientes[1]+coeficientes[2]*200 #estimativa para distancia 200m

#letra d)

#o valor 200 est� fora da faixa de valores da dist�ncia fornecido, 
#logo ele transmiste uma informa��o n confi�vel

#FIM DO GABARITO


#Parte Final do Intervalo de Confian�a que n�o vimos para consulta

estatistica <- summary(Beaver.lm)

conf1 <- confint(Beaver.lm) #intervalo de confian�a de 95%
ci.plot(Beaver.lm, conf.level = 0.95) #gr�fico

conf2 <- confint(Beaver.lm, level = 0.70) #intervalo de confiança de 70%
ci.plot(Beaver.lm, conf.level = 0.70) #grfic
