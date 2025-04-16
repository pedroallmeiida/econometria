# Pacotes necessários
library(readxl)
library(dplyr)
library(stringr)
library(zip)

setwd("C:\\Users\\pedro\\Documents\\github\\Econometria\\PROJETO_GINI\\")


# Ler a planilha com os dados (pode haver cabeçalhos adicionais — ajustar skip se necessário)
ideb_fund <- read_excel("dados_ideb\\ideb_fundamental\\divulgacao_anos_iniciais_municipios_2023.xlsx")

# Filtrar para o ano de 2021
ideb_fund = ideb_fund %>%
  filter(REDE == 'Pública') %>% 
  select( COD, INDICADOR_REND_P_FUND = INDICADOR_REND_P, SAEB_MEDIA_FUND = SAEB_MEDIA, IDEB_2021_FUND = IDEB_2021 ) 




## IDEB ENSINO MEDIO

# Ler a planilha com os dados (pode haver cabeçalhos adicionais — ajustar skip se necessário)
ideb_ensmedio <- read_excel("dados_ideb\\ideb_ensinomedio\\divulgacao_ensino_medio_municipios_2023.xlsx")

# Filtrar para o ano de 2021
ideb_ensmedio = ideb_ensmedio %>%
  filter(REDE == 'Pública') %>% 
  select( COD, INDICADOR_REND_P_ENS_MEDIO = INDICADOR_REND_P, SAEB_MEDIA_ENS_MEDIO = SAEB_MEDIA, IDEB_2021_ENS_MEDIO = IDEB_2021 ) 


df_ideb <- left_join(ideb_fund, ideb_ensmedio, by = c("COD" = "COD"))

df_ideb %>% write.csv(  "dados_tratado\\dados_tratado_ideb.csv", row.names = FALSE  )



