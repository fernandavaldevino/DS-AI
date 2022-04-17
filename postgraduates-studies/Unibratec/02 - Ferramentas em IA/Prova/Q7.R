# Questão 07

# a) Obter três caras, sem importar a ordem, nos 6 lançamentos de uma moeda justa.
D1 = tosscoin(6, makespace = TRUE)
E1 = subset(D1, isin(D1, c("H", "H", "H")))
Prob(E1)

# b) Obter quatro coroas, sem importar a ordem, no lançamento de 7 vezes de uma moeda
# com p_cara = 0.75 e p_coroa= 0.25. Use a função iidspace para criar as probabilidades.
D2 = iidspace(t(tosscoin(1)), ntrials = 7, probs = c(0.75, 0.25))
E2 = subset(D2, isin(D2, c("T", "T", "T", "T")))
Prob(E2)


# c) Obter a soma do resultado do lançamento de 4 vezes de um dado com 8 faces maior que
# 23.
D3 = rolldie(4, nsides = 8, makespace = TRUE)
E3 = subset(D3, X1+X2+X3+X4 > 23)
Prob(E3)


# d) Obter uma carta (qualquer naipe) de valor entre 5 e 9 na retirada de um baralho com os
# coringas presentes.
D4 = cards(jokers = TRUE)
Ea = subset(D4, suit == "Heart")
Eb = subset(D4, suit == "Diamond")
Ec = subset(D4, suit == "Club")
Ed = subset(D4, suit == "Spade")
E41 = union(E1, E2, E3, E4) 
E42 = subset(D4, rank %in% 5:9)
Prob(E41)

# e) Considere três lançamentos de um dado justo de seis faces. Evento A (valores iguais) e
# B (soma dos valores menor ou igual a 12). Calcule P(A|B) e P(B|A).
D5 = rolldie(2, makespace = TRUE) # dado justo e igual prob. para cada face
E5 = subset(D5, X1 == X2)
E6 = subset(D5, X1 + X2 <= 12)
A = E5
B = E6
Prob(A, given = B)
Prob(B, given = A)


