# Bibliotecas R 
library(ggplot2)
library(readr)
library(gtsummary) # VIsualizar os resultados dos modelos
library(betareg)
library(dplyr)

df = read.csv( "C:\\Users\\pedro\\Documents\\github\\econometria\\PROJETO_GINI\\dados_tratado\\dados_final_projeto.csv" )
names(df)

df = df %>% filter( UF == 'PE' )
View(df)

reg = GINI ~ NAO_POSSUI_AGUA_ENCANADA + NAO_POSSUI_LIGACAO_ESGOTO + NAO_TINHAM_BANHEIRO_NEM_SANITARIO + PERC_COLETA_LIXO + PERC_ACESSO_INTERNET + PERC_ALFA_15MAIS + 
  QTD_FORM_SUP_EDUC + QTD_FORM_SUP + TX_CRESC_GEO + DENSIDADE_DEMO + TX_PESSOAS_PRETA_OU_PARDA + PIB_POR_HABITANTE + TX_HOMICIDIO + TX_SUICIDIO + ARRECADACAO_TOTAL_IMPOSTOS_PERCAPITA +
  VALOR_TOTAL_BF_PERCAPITA + DESP_LEGISLATIVO_PERCAPITA + COTA_PARTE_FPM_PERCAPITA
mod <- betareg( reg, data = df)
summary(mod)

StepBeta::StepBeta(mod)

mod_final = betareg(formula =  GINI ~ QTD_FORM_SUP_EDUC + PERC_COLETA_LIXO + NAO_POSSUI_AGUA_ENCANADA + COTA_PARTE_FPM_PERCAPITA + NAO_POSSUI_LIGACAO_ESGOTO 
                    + TX_PESSOAS_PRETA_OU_PARDA + NAO_TINHAM_BANHEIRO_NEM_SANITARIO, data = df )
summary(mod_final)

tbl_regression(mod_final, intercept = F)

## Analise de residuo


# Resíduos quantílicos
residuos_q <- residuals(mod_final, type = "quantile")
hist(residuos_q)

# Visualização: QQ-plot dos resíduos quantílicos
qqnorm(residuos_q)
qqline(residuos_q, col = "red")


#  Resíduos quantílicos (outliers)
outliers <- which(abs(residuos_q) > 2)

#  Medidas de influência
leverage <- hatvalues(mod_final)
cooksD <- cooks.distance(mod_final)
influentes <- which(cooksD > 4 / nrow(df) )

# Combinação dos índices (union: remove se for outlier OU influente)
remover <- union(outliers, influentes)

# Remoção dos dados problemáticos
dados_limpos <- df[-remover, ]

# 7. Reajuste do modelo
modelo_limpo <- betareg(formula =  GINI ~ QTD_FORM_SUP_EDUC + PERC_COLETA_LIXO + NAO_POSSUI_AGUA_ENCANADA + COTA_PARTE_FPM_PERCAPITA + NAO_POSSUI_LIGACAO_ESGOTO 
                        + TX_PESSOAS_PRETA_OU_PARDA + NAO_TINHAM_BANHEIRO_NEM_SANITARIO, data = dados_limpos )
summary(modelo_limpo)

## removendo variavel nao significativa
modelo_limpo <- betareg(formula =  GINI ~ QTD_FORM_SUP_EDUC + PERC_COLETA_LIXO + NAO_POSSUI_AGUA_ENCANADA + COTA_PARTE_FPM_PERCAPITA + NAO_POSSUI_LIGACAO_ESGOTO 
                        + TX_PESSOAS_PRETA_OU_PARDA, data = dados_limpos )
summary(modelo_limpo)


### ANALISANDO OS RESIDUOS NOVAMENTE

# Resíduos quantílicos
residuos_q <- residuals(modelo_limpo, type = "quantile")
hist(residuos_q)

# Visualização: QQ-plot dos resíduos quantílicos
qqnorm(residuos_q)
qqline(residuos_q, col = "red")


