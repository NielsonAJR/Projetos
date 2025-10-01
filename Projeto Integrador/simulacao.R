# pacotes
  # pacotes
  library(tibble)
  library(tidyverse)

matrix_pot <- function(M, n){
  
  # Verificacao de pressupostos.
  # Matriz quadrada.
  if(dim(M)[1] != dim(M)[2]){
    print("Erro! A matriz nao quadrada.") # Avisa sobre o erro.
    return(M * 0) # Retorna uma matriz nula para chamar atenção.
  }
  # Com entradas nao negativas.
  if(any(M < 0)){
    return("Erro! A matriz possui alguma entrada negativa.") # Avisa sobre o erro.
    print(M * 0) # Retorna uma matriz nula para chamar atenção.
  }
  # A soma nas linhas deve ser igual a 1.
  if(any(abs(apply(M, 1, sum) - 1) > 1e-09)){
    print("Erro! Alguma linha na matriz nao soma 1.") # Avisa sobre o error.
    return(M * 0) # Retorna uma matriz nula para chamar atenção.
  }
  x <- diag(dim(M)[1])
  while(n){
    x <- x %*% M
    n <- n - 1
  }
  return(x)
}

simula_cadeia <- function(S, P, pi0, n){
  # Simula uma cadeia de Markov.
  #
  # Parametros de entrada:
  #
  ## S: espaco de estados finito de onde obteremos os estados indexados nas linhas
  ##    e colunas da matriz de transicao.
  ## P: matriz de transicao.
  ## pi0: vetor de probabilidades dos estados S
  ## n: numero de etapas a serem realizadas.
  
  # Gerar X0 com distribuicao pi0 e armazenar no vetor x.
  x <- numeric(n + 1)
  x[1] <- sample(S,size = 1, prob = pi0)
  
  # Gerar os estados das proximas etapas.
  for(i in 1:n){
    j <- match(x[i], S) # pega o indice do estado atual.
    p <- P[j,] # pega a funcao de transicao para o estado da etapa presente.
    x[i+1] <- sample(S, size = 1, prob = p) # obter estado da proxima etapa.
  }
  return(x)
}

## Aplicação

p <- 0.5
q <- 0.5
N <- 1000

P <- matrix(
  c(
    (1-q)**2, 2*q*(1-q), q**2,
    p*(1-q), p*q + (1-p)*(1-q), (1-p)*q,
    p**2, 2*p*(1-p), (1-p)**2
  ), byrow = TRUE, nrow = 3
)
P

set.seed(123)
S <- c(0,1,2)
pi0 <- c(0,0,1)
cadeia <- simula_cadeia(S, P, pi0, N)[-1]

cadeia_tbl <- tibble(etapa = 1:length(cadeia), estado = cadeia) |>
  arrange(etapa)

cadeia_tbl |>
  group_by(estado) |>
  count() |>
  mutate(freq = n/N)
