
# SIMULACAO DE MONTE CARLO
# Definir parâmetros do modelo



set.seed(123)  # fixando semente
n_simulacoes <- 1000  # Número de simulações de Monte Carlo
tamanhos_amostra <- c( 10, 50, 100)  # Tamanhos de amostra para a simulação

## Fixando os valores verdadeiro dos parametros
beta_0 <- 5  
beta_1 <- 10  

# armazenar os resultados das simulações
resultados <- list()



# Loop sobre os diferentes tamanhos de amostra
for (n_observacoes in tamanhos_amostra) {
  
  # Vetores para armazenar as estimativas dos coeficientes
  beta_0_est <- numeric(n_simulacoes)
  beta_1_est <- numeric(n_simulacoes)
  
  # Loop de simulação de Monte Carlo
  for (i in 1:n_simulacoes) {
    # Gerar dados de exemplo
    x <- rnorm(n_observacoes)  # Variável independente (X)
    epsilon <- rnorm(n_observacoes, mean = 0, sd = 1)  # Erro (epsilon)
    
    # Gerar a variável dependente (Y) com o modelo Y = beta_0 + beta_1 * X + erro
    
    y <- beta_0 + beta_1 * x + epsilon

    # EStimacao dos parametros por MQO
    beta_1_est[i] <- sum( ( y - mean(y) )*( x - mean(x) ) )/sum( ( x - mean(x) )^2 )
    beta_0_est[i] <- mean(y) - beta_1_est[i]*mean(x)
  }
  
  # Armazenar os resultados para o tamanho de amostra atual
  resultados[[paste("n =", n_observacoes)]] <- list(
    mean_beta_0 = mean(beta_0_est),
    mean_beta_1 = mean(beta_1_est),
    eqm_beta_0 = mean(  (beta_0 - beta_0_est)^2),
    eqm_beta_1 = mean(  (beta_1 - beta_1_est)^2)
  )
}
resultados


# Exibir os resultados
for (n_observacoes in tamanhos_amostra) {
  cat("\nTamanho da amostra:", n_observacoes, "\n")
  cat("Estimativa média de beta_0:", resultados[[paste("n =", n_observacoes)]]$mean_beta_0, "\n")
  cat("Estimativa média de beta_1:", resultados[[paste("n =", n_observacoes)]]$mean_beta_1, "\n")
  cat("EQM de beta_0:", resultados[[paste("n =", n_observacoes)]]$eqm_beta_0, "\n")
  cat("EQM de beta_1:", resultados[[paste("n =", n_observacoes)]]$eqm_beta_1, "\n")
}
