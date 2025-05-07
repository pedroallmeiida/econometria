## Bibliotecas de séries temporais
library(TSA)
library(forecast)
library(fpp3)
library(tsibble)
library(tsibbledata)


### Carregando banco de dados em formato csv
df_auto <- readr::read_csv("C:\\Users\\pedro\\Documents\\github\\econometria\\Aulas\\Etapa2\\Visualizacao_series_temporais\\vendas_automoveis.csv")
head(df_auto)


## Transformando a variavel Sales no formato TS
y = ts( df_auto$vendas, start = 1990,end = 2025,  frequency = 12  )
plot.ts( y  )


plot(decompose(y))

df_auto$Data_formatada <- as.Date(paste0("01/", df_auto$Data), format = "%d/%m/%Y")
View(df_auto)

### transformando para o formato tstible
serie_mensal <- df_auto %>%
  mutate(mes = yearmonth(Data_formatada)) %>%
  as_tsibble(index = mes) %>%
  select( mes, vendas )
serie_mensal

### Plot das series
serie_mensal %>%
  pivot_longer(-mes) %>%
  ggplot(aes(x = mes, y = value)) +
  geom_line() +
  theme_minimal()

## serie trimestral
serie_trimestral <- df_auto %>%
  mutate(trimestre = yearquarter(Data_formatada)) %>% 
  group_by(trimestre) %>%
  summarise(vendas = sum(vendas), .groups = "drop") %>%
  as_tsibble(index = trimestre)


### Plot das series
serie_trimestral %>%
  pivot_longer(-trimestre) %>%
  ggplot(aes(x = trimestre, y = value)) +
  geom_line() +
  theme_minimal()

### transformando para anual
serie_anual <- df_auto %>%
  mutate(ano = year(Data_formatada)) %>%
  group_by(ano) %>%
  summarise(vendas = sum(vendas), .groups = "drop") %>%
  as_tsibble(index = ano)
serie_anual



### Plot das series
serie_anual %>%
  pivot_longer(-ano) %>%
  ggplot(aes(x = ano, y = value)) +
  geom_line() +
  theme_minimal()


serie_mensal %>%
  model(
    classical_decomposition(vendas, type = "additive")
  ) %>%
  components() %>%
  autoplot() +
  labs(title = "Decomposição classica aditiva do total de vendas")


### Estudando a sazonalidade

serie_trimestral %>%
  gg_season(vendas, labels = "both") +
  labs(y = "Quantidade de vendas",
       title = "Sazonalidade: Sales")


### subgraficos 

serie_trimestral %>%
  gg_subseries(vendas) +
  labs(
    y = "Quantidade de vendas",
    title = "Sazonalidade: vendas"
  )
