library(sidrar)
library(dplyr)
library(ggplot2)

# Função de limpeza
limpar_string <- function(x) {
  x <- base::tolower(x)                             # tudo minúsculo
  x <- stringr::str_trim(x)                            # remove espaços do início e fim
  x <- stringi::stri_trans_general(x, "Latin-ASCII")  # remove apenas os acentos, mas mantem as letras
  x <- stringr::str_replace_all(x, "[^a-z0-9 ]", "")   # remove caracteres especiais, mantém letras, números e espaço
  return(x)
}



## Baixa os dados sobre coleta de agua 
df_agua <- get_sidra( api = "/t/6803/n6/all/v/1000381/p/all/c1821/72129,72144,72153/d/v1000381%202")

df_agua = df_agua %>% 
  filter( `Existência de ligação à rede geral de distribuição de água e principal forma de abastecimento de água (Código)`  != '72129' ) %>% 
  select(
    COD_MUNICIPIO = `Município (Código)`,
    NM_MUNICIPIO = Município,
    PERC = Valor,
    VARIAVEL = `Existência de ligação à rede geral de distribuição de água e principal forma de abastecimento de água`
  ) %>% 
  mutate(
    UF = stringr::str_split_fixed(NM_MUNICIPIO, " - ", 2)[,2],
    NM_MUNICIPIO = stringr::str_split_fixed(NM_MUNICIPIO, " - ", 2)[,1]
  ) %>% 
  mutate(
    NM_MUNICIPIO = limpar_string(NM_MUNICIPIO)
  ) %>% 
  tidyr::pivot_wider(
    names_from = VARIAVEL,
    values_from = PERC
  ) %>%  
  janitor::clean_names(case = "all_caps")



## Baixa os dados sobre coleta de esgoto 
df_esgoto <- get_sidra( api = "/t/6805/n6/all/v/1000381/p/all/c11558/46290,46292,92861/d/v1000381%202")

df_esgoto = df_esgoto %>% 
  filter( `Tipo de esgotamento sanitário (Código)`  != '46292' ) %>% 
  select(
    COD_MUNICIPIO = `Município (Código)`,
    NM_MUNICIPIO = Município,
    PERC = Valor,
    VARIAVEL = `Tipo de esgotamento sanitário`
  ) %>% 
  mutate(
    UF = stringr::str_split_fixed(NM_MUNICIPIO, " - ", 2)[,2],
    NM_MUNICIPIO = stringr::str_split_fixed(NM_MUNICIPIO, " - ", 2)[,1]
  ) %>% 
  mutate(
    NM_MUNICIPIO = limpar_string(NM_MUNICIPIO)
  ) %>% 
  tidyr::pivot_wider(
    names_from = VARIAVEL,
    values_from = PERC
  ) %>%  
  janitor::clean_names(case = "all_caps")


## Baixa os dados sobre coleta de lixo 
df_lixo <- get_sidra( api = "/t/6892/n6/all/v/1000381/p/all/c67/2520/d/v1000381%202")

df_lixo = df_lixo %>% 
  select(
    COD_MUNICIPIO = `Município (Código)`,
    NM_MUNICIPIO = Município,
    PERC_COLETA_LIXO = Valor
  ) %>% 
  mutate(
    UF = stringr::str_split_fixed(NM_MUNICIPIO, " - ", 2)[,2],
    NM_MUNICIPIO = stringr::str_split_fixed(NM_MUNICIPIO, " - ", 2)[,1]
  ) %>% 
  mutate(
    NM_MUNICIPIO = limpar_string(NM_MUNICIPIO)
  )

## Baixa os dados sobre internet 
df_internet <- get_sidra( api = "/t/9936/n6/all/v/1000381/p/all/c2072/77585/c63/95826/c125/2932/d/v1000381%202")

df_internet = df_internet %>% 
  select(
  COD_MUNICIPIO = `Município (Código)`,
  NM_MUNICIPIO = Município,
  PERC_ACESSO_INTERNET = Valor
) %>% 
  mutate(
    UF = stringr::str_split_fixed(NM_MUNICIPIO, " - ", 2)[,2],
    NM_MUNICIPIO = stringr::str_split_fixed(NM_MUNICIPIO, " - ", 2)[,1]
  ) %>% 
  mutate(
    NM_MUNICIPIO = limpar_string(NM_MUNICIPIO)
  )



## Baixa os dados sobre populacao
df_populacao <- get_sidra( api = "/t/9605/n6/all/v/allxp/p/last%201/c86/2777,2779,95251")


df_populacao = df_populacao %>% 
  select(
    COD_MUNICIPIO = `Município (Código)`,
    NM_MUNICIPIO = Município,
    PERC = Valor,
    VARIAVEL = `Cor ou raça`
  ) %>% 
  mutate(
    UF = stringr::str_split_fixed(NM_MUNICIPIO, " - ", 2)[,2],
    NM_MUNICIPIO = stringr::str_split_fixed(NM_MUNICIPIO, " - ", 2)[,1]
  ) %>% 
  mutate(
    NM_MUNICIPIO = limpar_string(NM_MUNICIPIO)
  ) %>% 
  tidyr::pivot_wider(
    names_from = VARIAVEL,
    values_from = PERC
  ) %>%  
  janitor::clean_names(case = "all_caps") %>%
  rename( POP = TOTAL, POP_PRETA = PRETA, POP_PARDA = PARDA ) 


## Baixa os dados sobre envelhecimento
df_envelhecimento <- get_sidra( api = "/t/9756/n6/all/v/9175/p/last%201/c86/95251/d/v9175%202")

df_envelhecimento = df_envelhecimento %>% 
  select(
    COD_MUNICIPIO = `Município (Código)`,
    NM_MUNICIPIO = Município,
    INDICE_ENVELHECIMENTO = Valor
  ) %>% 
  mutate(
    UF = stringr::str_split_fixed(NM_MUNICIPIO, " - ", 2)[,2],
    NM_MUNICIPIO = stringr::str_split_fixed(NM_MUNICIPIO, " - ", 2)[,1]
  ) %>% 
  mutate(
    NM_MUNICIPIO = limpar_string(NM_MUNICIPIO)
  ) 

## Baixa os dados sobre taxa de alfabetizacao 15 anos ou mais
df_alfa_15mais <- get_sidra( api = "/t/9543/n6/all/v/all/p/all/c2/6794/c86/95251/c287/100362/d/v2513%202")

df_alfa_15mais = df_alfa_15mais %>% 
  select(
    COD_MUNICIPIO = `Município (Código)`,
    NM_MUNICIPIO = Município,
    PERC_ALFA_15MAIS  = Valor
  ) %>% 
  mutate(
    UF = stringr::str_split_fixed(NM_MUNICIPIO, " - ", 2)[,2],
    NM_MUNICIPIO = stringr::str_split_fixed(NM_MUNICIPIO, " - ", 2)[,1]
  ) %>% 
  mutate(
    NM_MUNICIPIO = limpar_string(NM_MUNICIPIO)
  ) 



## Baixa os dados sobre curso_superior
df_superior <- get_sidra( api = "/t/10065/n6/all/v/all/p/all/c2082/77843,78032/c2/6794/c86/95251")

df_superior = df_superior %>% 
  select(
    COD_MUNICIPIO = `Município (Código)`,
    NM_MUNICIPIO = Município,
    QTD = Valor,
    VARIAVEL = `Áreas gerais, específicas e detalhadas de formação do curso superior de graduação que concluiu`
  ) %>% 
  mutate(
    UF = stringr::str_split_fixed(NM_MUNICIPIO, " - ", 2)[,2],
    NM_MUNICIPIO = stringr::str_split_fixed(NM_MUNICIPIO, " - ", 2)[,1]
  ) %>% 
  mutate(
    NM_MUNICIPIO = limpar_string(NM_MUNICIPIO)
  ) %>% 
  tidyr::pivot_wider(
    names_from = VARIAVEL,
    values_from = QTD
  ) %>%  
  janitor::clean_names(case = "all_caps") %>%
  rename( QTD_FORM_SUP = TOTAL, QTD_FORM_SUP_EDUC = `X01_EDUCACAO` ) 


## Baixa os dados sobre densidade demografica, pessoas por km2
df_densidade_demo <- get_sidra( api = "/t/4714/n6/all/v/614/p/all/d/v614%202")


df_densidade_demo = df_densidade_demo %>% 
  select(
    COD_MUNICIPIO = `Município (Código)`,
    NM_MUNICIPIO = Município,
    DENSIDADE_DEMO = Valor
  ) %>% 
  mutate(
    UF = stringr::str_split_fixed(NM_MUNICIPIO, " - ", 2)[,2],
    NM_MUNICIPIO = stringr::str_split_fixed(NM_MUNICIPIO, " - ", 2)[,1]
  ) %>% 
  mutate(
    NM_MUNICIPIO = limpar_string(NM_MUNICIPIO)
  ) 


## Baixa os dados sobre crescimento geometrico
df_crescimento_geo <- get_sidra( api = "/t/4709/n6/all/v/10605/p/all/d/v10605%202")


df_crescimento_geo = df_crescimento_geo %>% 
  select(
    COD_MUNICIPIO = `Município (Código)`,
    NM_MUNICIPIO = Município,
    TX_CRESC_GEO = Valor
  ) %>% 
  mutate(
    UF = stringr::str_split_fixed(NM_MUNICIPIO, " - ", 2)[,2],
    NM_MUNICIPIO = stringr::str_split_fixed(NM_MUNICIPIO, " - ", 2)[,1]
  ) %>% 
  mutate(
    NM_MUNICIPIO = limpar_string(NM_MUNICIPIO)
  ) 


df_pib <- get_sidra(api = "/t/5938/n6/all/v/37,497,552/p/last%201/d/v37%200,v497%202,v552%202")
View(df_pib)

df_pib = df_pib %>% 
  select(
    COD_MUNICIPIO = `Município (Código)`,
    NM_MUNICIPIO = Município,
    VALOR = Valor,
    VARIAVEL = `Variável`
  ) %>% 
  mutate(
    UF = stringr::str_split_fixed(NM_MUNICIPIO, " - ", 2)[,2],
    NM_MUNICIPIO = stringr::str_split_fixed(NM_MUNICIPIO, " - ", 2)[,1]
  ) %>% 
  mutate(
    NM_MUNICIPIO = limpar_string(NM_MUNICIPIO)
  ) %>% 
  tidyr::pivot_wider(
    names_from = VARIAVEL,
    values_from = VALOR
  ) %>%  
  janitor::clean_names(case = "all_caps") %>%
  rename( PIB_BRUTO = PRODUTO_INTERNO_BRUTO_A_PRECOS_CORRENTES, 
          PARTICIPACAO_PIB_UF = PARTICIPACAO_DO_PRODUTO_INTERNO_BRUTO_A_PRECOS_CORRENTES_NO_PRODUTO_INTERNO_BRUTO_A_PRECOS_CORRENTES_DA_UNIDADE_DA_FEDERACAO,
          PARTICIPACAO_PIB_MESOREGIAO = PARTICIPACAO_DO_PRODUTO_INTERNO_BRUTO_A_PRECOS_CORRENTES_NO_PRODUTO_INTERNO_BRUTO_A_PRECOS_CORRENTES_DA_MESORREGIAO_GEOGRAFICA) 


library(dplyr)
library(purrr)

lista_dfs <- list(df_agua, df_esgoto, df_lixo, df_internet, df_alfa_15mais, df_superior, df_crescimento_geo, df_densidade_demo, df_envelhecimento, df_populacao,  df_pib)

df_ibge<- reduce(lista_dfs, left_join, by = c("COD_MUNICIPIO", "NM_MUNICIPIO", "UF"))
names(df_ibge)

df_ibge = df_ibge %>% 
  mutate( TX_PESSOAS_PRETA_OU_PARDA = ( POP_PRETA + POP_PARDA )/POP,
          PIB_POR_HABITANTE = PIB_BRUTO/POP)
View(df_ibge)


df_ibge %>%
  write.csv(  "dados_tratado\\dados_tratado_ibge.csv" , row.names = FALSE )

