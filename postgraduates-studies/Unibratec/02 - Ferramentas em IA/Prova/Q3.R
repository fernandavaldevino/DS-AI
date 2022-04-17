# Questão 03

varcsv = read.csv("avocado.csv", header = TRUE, sep = ',')

# a) Obtenha os parâmetros estatísticos da base com a função summary.
dadossum = summary(varcsv)
dadossum

# b) Salve cada coluna da base de dados em uma variável distinta.
date = varcsv[,2]
avrprice = varcsv[,3]
totalvol = varcsv[,4]
totalbags = varcsv[,8]
smallbags = varcsv[,9]
largebags = varcsv[,10]
xlargebags = varcsv[,11]
typedata = varcsv[,12]
year = varcsv[,13]
region = varcsv[,14]
planilha = list(avrprice)

# c) Concatene e exporte os parâmetros type, year e region para um arquivo nomeado
# “abacate.xlsx” (a ser criado pelo usuário)
lista = data.frame(Tipo = typedata, Ano = year, Regiao = region)
dados = list(typedata, year, region)
write.xlsx(dados, "abacate.xlsx")

# d) Plote um gráfico de pontos dos parâmetros de total.bags (eixo X) por year (eixo Y).
plot(totalbags, year, type="p" , main="Abacate", xlab = "Bolsas Totais", ylab = "Ano", col="blue")

# e) Plote um gráfico de barras para o parâmetro Averageprice.
barplot(tapply(rep(1,18249), varcsv["AveragePrice"], sum), main = "Average Prices", col = "green")

# f) Plote um gráfico de pizza para o parâmetro year.
pie(tapply(rep(1, 18249), varcsv["year"], sum), main = "Year")

# g) Plote histogramas distintos para os parâmetros Averageprice e year.
hist(t(varcsv["AveragePrice"]), 9, main = "Histograma de Average Prices", xlab = "Average Prices", ylab = "Frequência", col = c(1:10))
hist(t(varcsv["year"]), 4, main = "Histograma de Year", xlab = "Year", ylab = "Frequência", col = c(1:3))

# h) Calcule a correlação entre os parâmetros 
# Averageprice e year;
cor(avrprice, year)

# Total.volume e Total.bags;
cor(totalvol, totalbags)

# e Total.bags e year. Interprete cada resultado com um breve comentário.
cor(totalbags, year)



