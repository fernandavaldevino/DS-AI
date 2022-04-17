# Questão 05

soma100 = function(x){
  soma = 0
  for (i in 0:99){
    soma = soma + ((x^i)/fatorial(i))
  }
  return(soma)
}

fatorial = function(n){
  if (n == 0 || n == 1)
    return (1)
  else
    return (n*fatorial(n-1))
}

soma100(-4)
soma100(6)
