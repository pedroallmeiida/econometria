library(dplyr)
library(ggplot2)
library(stringr)
library(readr)
library(readxl)

# Função de limpeza
limpar_string <- function(x) {
  x <- base::tolower(x)                             # tudo minúsculo
  x <- stringr::str_trim(x)                            # remove espaços do início e fim
  x <- stringi::stri_trans_general(x, "Latin-ASCII")  # remove apenas os acentos, mas mantem as letras
  x <- stringr::str_replace_all(x, "[^a-z0-9 ]", "")   # remove caracteres especiais, mantém letras, números e espaço
  return(x)
}


pasta = "C:\\Users\\pedro\\Documents\\github\\econometria\\PROJETO_GINI\\dados_bruto\\"


arquivos_csv <- list.files(path = pasta, pattern = "\\.csv$", full.names = TRUE)
arquivos_csv

# Nomes limpos para os dataframes
nomes_arquivos <- tools::file_path_sans_ext(basename(arquivos_csv))

# Lê os arquivos e armazena em uma lista nomeada
lista_dfs <- setNames(lapply(arquivos_csv, read.csv), nomes_arquivos)


df_taxa_homicidio = lista_dfs$taxa_homicidio %>% select( COD, TX_HOMICIDIO  ) 
df_taxa_suicidio = lista_dfs$taxa_suicidio %>% select( COD, TX_SUICIDIO  ) 
df_admissoes = lista_dfs$admissoes %>% select( COD, ADMISSOES  ) 
df_demissoes = lista_dfs$demissoes %>% select( COD, DEMISSOES  ) 
df_exportacoes = lista_dfs$exportacoes %>% select( COD, EXPORTACAO  ) 
df_poupanca = lista_dfs$poupanca %>% select( COD, POUPANCA  ) 
df_arrecadacao_total_IPTU_ISS_ITBI = lista_dfs$arrecadacao_total_IPTU_ISS_ITBI %>% select( COD, ARRECADACAO_TOTAL_IMPOSTOS  ) 
df_despesa_ciencia_tecnologia = lista_dfs$despesa_ciencia_tecnologia %>% select( COD, DESP_CIENCIA_TEC  ) 
df_indice_gini_2010 = lista_dfs$indice_gini_2010 %>% select( COD, GINI  ) 
df_bolsa_familia = lista_dfs$bolsa_familia %>% select( COD, VALOR_TOTAL_BF  ) 
df_despesa_legislativa = lista_dfs$despesa_legislativa %>% select( COD, DESP_LEGISLATIVO  ) 
df_iptu = lista_dfs$iptu %>% select( COD, RECEITA_IPTU  ) 
df_cota_parte_FPM = lista_dfs$cota_parte_FPM %>% select( COD, COTA_PARTE_FPM  ) 
df_despesa_saude_saneamento = lista_dfs$despesa_saude_saneamento %>% select( COD, DESP_SAUDE_SANEAMENTO  ) 
df_operacoes_credito = lista_dfs$operacoes_credito %>% select( COD, OPERACOES_CREDITO  ) 
df_pop_residente = lista_dfs$pop_residente %>% select( COD, POP  ) 



nomes_df =list(df_taxa_homicidio, df_taxa_suicidio, df_admissoes, df_demissoes, df_exportacoes, df_poupanca, df_arrecadacao_total_IPTU_ISS_ITBI, 
               df_despesa_ciencia_tecnologia, df_indice_gini_2010, df_bolsa_familia, df_despesa_legislativa, df_iptu, df_cota_parte_FPM, 
               df_despesa_saude_saneamento, df_operacoes_credito, df_pop_residente)

  
  
df_ipea <- purrr::reduce(nomes_df, left_join, by = "COD")
names(df_ipea)

df_ipea = df_ipea %>% 
  mutate(
  SALDO_EMPREGO_PERCAPITA = (ADMISSOES - DEMISSOES)/POP,
  EXPORTACAO_PERCAPITA = EXPORTACAO/POP,
  POUPANCA_PERCAPITA = POUPANCA/POP,
  ARRECADACAO_TOTAL_IMPOSTOS_PERCAPITA = ARRECADACAO_TOTAL_IMPOSTOS/POP,
  DESP_CIENCIA_TEC_PERCAPITA = DESP_CIENCIA_TEC/POP,
  VALOR_TOTAL_BF_PERCAPITA = VALOR_TOTAL_BF/POP,
  DESP_LEGISLATIVO_PERCAPITA = DESP_LEGISLATIVO/POP,
  RECEITA_IPTU_PERCAPITA = RECEITA_IPTU/POP,
  COTA_PARTE_FPM_PERCAPITA = COTA_PARTE_FPM/POP,
  DESP_SAUDE_SANEAMENTO_PERCAPITA = DESP_SAUDE_SANEAMENTO/POP,
  OPERACOES_CREDITO_PERCAPITA = OPERACOES_CREDITO/POP
  ) %>% 
  select(COD, GINI,  TX_HOMICIDIO, TX_SUICIDIO, EXPORTACAO_PERCAPITA, POUPANCA_PERCAPITA, ARRECADACAO_TOTAL_IMPOSTOS_PERCAPITA, DESP_CIENCIA_TEC_PERCAPITA, 
         VALOR_TOTAL_BF_PERCAPITA, DESP_LEGISLATIVO_PERCAPITA, RECEITA_IPTU_PERCAPITA, COTA_PARTE_FPM_PERCAPITA, DESP_SAUDE_SANEAMENTO_PERCAPITA,
         OPERACOES_CREDITO_PERCAPITA
         )


df_ipea %>%
  write.csv(  "dados_tratado\\dados_tratado_ipea.csv", row.names = FALSE  )





