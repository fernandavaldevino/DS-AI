# Questão 06

arqxlsx = read.xlsx("dataset.xlsx", 1)

# Média
mean(arqxlsx$Quantidade, na.rm = TRUE)

# Mediana
median(arqxlsx$Quantidade, na.rm = TRUE)

# Moda
arqxlsx[arqxlsx == max(arqxlsx)]

# Variância
var(arqxlsx$Quantidade, na.rm = TRUE)

# Desvio Padrão
sd(arqxlsx$Quantidade, na.rm = TRUE)

# Correlação entre Item e Quantidade
cor(arqxlsx$Item , arqxlsx$Quantidade, use = "na.or.complete")
# [1] 0.1600207 --> Há uma correlação bem fraca entre as variáveis Item e Quantidade.
