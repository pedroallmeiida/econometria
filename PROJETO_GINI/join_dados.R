library(dplyr)
library(ggplot2)
library(stringr)
library(readr)
library(readxl)


## Leitura dos dados
df_ibge = read.csv(  "C:\\Users\\pedro\\Documents\\github\\econometria\\PROJETO_GINI\\dados_tratado\\dados_tratado_ibge.csv"  )
df_ipea = read.csv(  "C:\\Users\\pedro\\Documents\\github\\econometria\\PROJETO_GINI\\dados_tratado\\dados_tratado_ipea.csv"  )
df_ideb = read.csv(  "C:\\Users\\pedro\\Documents\\github\\econometria\\PROJETO_GINI\\dados_tratado\\dados_tratado_ideb.csv"  )
names(df_ibge)

df_join1 <- left_join(df_ibge, df_ipea, by = c("COD_MUNICIPIO" = "COD"))
df_final <- left_join(df_join1, df_ideb, by = c("COD_MUNICIPIO" = "COD"))
View(df_final)
names(df_final)

df_final = df_final %>% rename( AGUA_ENCANADA_PRINCIPAL = POSSUI_LIGACAO_A_REDE_GERAL_E_A_UTILIZA_COMO_FORMA_PRINCIPAL,
                                NAO_POSSUI_AGUA_ENCANADA = NAO_POSSUI_LIGACAO_COM_A_REDE_GERAL,
                                NAO_POSSUI_LIGACAO_ESGOTO = REDE_GERAL_REDE_PLUVIAL_OU_FOSSA_LIGADA_A_REDE )

df_final %>% write.csv(  "C:\\Users\\pedro\\Documents\\github\\econometria\\PROJETO_GINI\\dados_tratado\\dados_final_projeto.csv", row.names = FALSE  )
