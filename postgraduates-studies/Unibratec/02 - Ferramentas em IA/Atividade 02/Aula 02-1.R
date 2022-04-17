# FUNÇÕES

# 01. Criar uma função que calcule a área de um triângulo equilátero.

triangulo = function(lado) {
  formula = ((lado^2)*sqrt(3))/4
  return (formula)
}

triangulo(5)


# 02. Criar uma função para calcular as raízes reais de uma equação do 2º grau.
# ∆ > 0: x1 =! x2
# ∆ = 0: x1 == x2
# ∆ < 0: raízes não reais

equacao = function(a, b, c){
  if (a != 0){
    delta = ((b^2) - 4*a*c)
    if (delta < 0){
      print ("∆ < 0 >> Não tem raízes reais!")
      result = c ("∆ = ", delta)
    } else{
      x1 = ((-1*b) + sqrt(delta))/(2*a)
      x2 = ((-1*b) - sqrt(delta))/(2*a)
      raiz1 = c("x1 = ", x1)
      raiz2 = c("x2 = ", x2)
#     return (sprintf("x1 = %.1f | x2 = %.1f", x1, x2))  >> imprime com 1 casa decimal
      return (sprintf("x1 = %s | x2 = %s", x1, x2))   # imprime como string
    }
  }else{
    print("O valor de a deve ser diferente de 0.")
  }
}

# ∆ > 0
equacao(1, -3, -10)
# ∆ = 0
equacao(9, -12, 4)
# ∆ < 0
equacao (5, 3, 5)





# DADOS AUSENTES

X = c(10, 15, 18, 23, NA, 8)
Y = c(20, 30, NA, 46, NA, 16)

mean(X, na.rm = TRUE)
var(X, na.rm = TRUE)
sd(X, na.rm = TRUE)
mean(Y, na.rm = TRUE)
var(Y, na.rm = TRUE)
sd(Y, na.rm = TRUE)
cor(X, Y, use = "na.or.complete")


qtde = read.xlsx("dataset_aula02-1.xlsx", 1)
mean(qtde$Quantidade, na.rm = TRUE)
median(qtde$Quantidade, na.rm = TRUE)
var(qtde$Quantidade, na.rm = TRUE)
sd(qtde$Quantidade, na.rm = TRUE)
cor(qtde$Item , qtde$Quantidade, use = "na.or.complete")



# PROBABILIDADE

# Faces da moeda
# S = {"Cara = Head", "Coroa = Tail"}
S1 = tosscoin(1)
class(S1)
S1
S2 = tosscoin(3)
class(S2)
S2

# Dado com 6 faces
# S = {1, 2, 3, 4, 5, 6}
S3 = rolldie (1)
S3
S4 = rolldie(3, nsides = 4)
S4


# Cartas de um baralho - Com ou Sem a carta coringa
S5 = cards(jokers = TRUE) #inclui os coringas
S5
S6 = cards() #exclui os coringas
S6


# Cria espaço de remoção numa urna de bolas
S7 = urnsamples(1:3, size = 2, replace = TRUE, ordered = TRUE)
S8 = urnsamples(1:3, size = 2, replace = FALSE, ordered = TRUE)
S9 = urnsamples(1:3, size = 2, replace = FALSE, ordered = FALSE)
S10 = urnsamples(1:3, size = 2, replace = TRUE, ordered = FALSE)

Y = c("azul", "vermelho", "preto")
S11 = urnsamples(Y, size = 2, replace = TRUE, ordered = TRUE)

S7
S8
S9
S10
S11



# Roletas
# tem a roleta americana e a roleta européia
S12 = tosscoin(2, makespace = TRUE)
S12
S12[1:3,]
S12
S12[c(2,4),]
12

S13 = cards()
E1 = subset(S13, suit == "Heart")
E2 = subset(S13, rank %in% 7:9)

S14 = rolldie(3)
E3 = subset(S14, X1+X2+X3 > 16)

S13
S14
E1
E2
E3


# Funções Auxiliares
# 01. %in% >> "está contido" >> verifica se os elementos de y estão contidos no vetor x
x1 = 1:10
y1 = 8:12
z1 = y1 %in% x1
z1

# 02. isin(x, y) >> verifica se todo o y está em x

x2 = 1:10
y2 = 8:12
z2 = isin(x2, y2)
z2

S15 = rolldie(4) #lançamento de um dado de 6 faces 4x
E4 = subset(15, isin(S15, c(2,2,6)), ordered = TRUE) 
#S15
E4


# OPERAÇÕES COM CONJUNTOS
# União >> union(A,B)
# Intersecção >> intersect(A,B)
# Diferença >> setdiff(A,B)

S13 = cards()
E1 = subset(S13, suit == "Heart")
E2 = subset(S13, rank %in% 7:9)
E3 = union(E1, E2)
E4 = intersect(E1, E2)
E5 = setdiff(E1, E2)
E6 = setdiff(E2, E1)
S13
E1
E2
E3
E4
E5
E6


A = c(0, 1)
B = c(0, 1, 2)
C = c(2, 3)

result1 = intersect(union(A,B), union(B,C)) 
result1

U = c(0:6)
A = c(1:2)
B = c(2:4)
C = c(4:5)

result2 = intersect(setdiff(U,A), union(B,C))
result2


# PROBABILIDADE
# - moeda
tosscoin(2, makespace = TRUE)
probspace(tosscoin(1), probs = c(0.7, 0.3)) # descompassa a probabilidade

# - dado
rolldie(1, makespace = TRUE)
probspace(rolldie(1), probs = c(0.06, 0.36, 0.06, 0.06, 0.06, 0.4))

# - cartas
S16 = cards(makespace = TRUE)
E9 = subset(S16, suit == "Diamond") #todas as cartas no naipe de ouros
E10 = subset(S16, rank %in% 4:7)
prob(E9)
prob(E10)


# 01. Obter três caras, sem ordem específica, no lançamento de6 6 vezes de uma moeda justa:
S1 = tosscoin(6, makespace = TRUE)
E1 = subset(S1, isin(S1, c("H", "H", "H")))
Prob(E1)

# 02. Obter quatro coroas, sem ordem específica, no lançamento de 7x de uma moeda com 
# p_cara = 0.65 e p_coroa = 0.35 (ou seja, uma moeda injusta)
# iidspace >> cria espaços amostrais de eventos independentes (um evento não depende de outro
# pra acontecer)
S2 = iidspace(t(tosscoin(1)), ntrials = 7, probs = c(0.65, 0.35))
E2 = subset(S2, isin(S2, c("T", "T", "T", "T")))
Prob(E2)

# 03. Obter a soma do resultado do lançamento de 4x de um dado com 8 faces maior que 22.
S3 = rolldie(4, nsides = 8, makespace = TRUE)
E3 = subset(S3, X1+X2+X3+X4 > 22)
Prob(E3)



# MÉTODOS DE CONTAGEM

# Oito bolas na urna enumeradas de 1 a 8, removendo quatro bolas, qual a quantidade de cada 
# evento a seguir:
n1 = nsamp(n=8, k=4, replace = TRUE, ordered = TRUE)
n2 = nsamp(n=8, k=4, replace = FALSE, ordered = TRUE)
n3 = nsamp(n=8, k=4, replace = FALSE, ordered = FALSE)
n4 = nsamp(n=8, k=4, replace = TRUE, ordered = FALSE)
n1
n2
n3
n4


# Exemplo dos 11 pintores e 7 obras
# 11 pintores --> 3 pintores
# 7 obras --> 4 obras
# 31 formas de iluminação

pintores = c(11, 7, 31)
obras = c(3, 4, 3)
devolve = c(FALSE, FALSE, TRUE) #indica a devolução de pintores, obras e iluminação, respect.

todas_as_prob = nsamp(pintores, obras, replace = devolve, ordered = TRUE)
prod(todas_as_prob)


# Slide - Dois lançamentos de um dado justo
S17 = rolldie(2, makespace = TRUE) # dado justo e igual prob. para cada face
E17 = subset(S17, X1 == X2)
E18 = subset(S17, X1 + X2 >= 8)
A = E17
B = E18
Prob(A, given = B)
Prob(B, given = A)


# Exemplo - Urna com 10 bolas, sendo 7 vermelhas e 7 verdes. Probabilidade:

balls = rep(c("vermelha", "verde"), times = c(7,3))
S18 = urnsamples(balls, size=3, replace = FALSE, ordered = TRUE)
P1 = probspace(S18)

# a) As 3 bolas são vermelhas
n8 = Prob(P1, isrep(P1, "vermelha", 3))

# b) 2 bolas vermelhas
n9 = Prob(P1, isrep(P1, "verde", 2))

# c) 1 bola vermelha, seguida de 1 verde, seguida de outra vermelha
n10 = Prob(P1, isin(P1, c("vermelha", "verde", "vermelha"), ordered = TRUE))

n8
n9
n10


# Probabilidade Complementar
S19 = tosscoin(10, makespace = TRUE)
E19 = subset(S19, isrep(S19, vals = "T", nrep = 10))
result = 1 - Prob(E19)
result

# Eventos Independentes
S20 = iidspace(c("H", "T"), ntrials = 3, probs = c(0.7, 0.3))
S20


#Variáveis Randômicas
S21 = rolldie(3, nsides = 4, makespace = TRUE)
S21 = addrv(S21, U=X1-X2+X3)
head(S21) #visualiza os 6 primeiros elementos de S@! OPCIONAL
n21 = Prob(S21, U>6)
n21


#Regressão Linear Simples
# quando a taxa estiver fora do range de valores, pode-se fazer o cálculo, mas não será um 
# valor "fiel". Ex: taxa de natalidade de 15% e 45%. 15% não é um valor fiel, pois está fora
# do range entre 22,5 e 59
reg = c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")
tx_nat = c(35.6, 59, 25.2, 22.5, 25.41)
tx_esc = c(12.7, 29.4, 8.6, 8.3, 12.4)
d1 = data.frame("Regiões"=reg, "Natalidade"=tx_nat, "Escolaridade"=tx_esc)
x1 = d1[,2]
y1 = d1[,3]

d1_reg = lm(y1 ~ x1, data=d1)
coef1 = coef(d1_reg)
plot(x1, y1, type="p", xlab = "Taxa de Escolaridade", ylab = "Taxa de Natalidade", col="blue")
abline(coef1)
b0 = coef1[1]
b1 = coef1[2]
r1 = b0 + b1*15
r2 = b0 + b1*45

