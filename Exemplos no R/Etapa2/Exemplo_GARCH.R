## Baixar as bibliotecas 
library(quantmod)
library(tidyquant)
library(rugarch)

## Extraindo dados da bolsa de valor 
dados <- tq_get("BOVA11.SA", from = "2016-01-01", to = "2025-06-30" )
head(dados)
tail(dados)


### transformando para o formato tstible
serie <- dados %>%
  mutate(retorno = log(adjusted / lag(adjusted))) %>%
  as_tsibble(index = date) %>% 
  drop_na(retorno) %>%
  select( date, price = adjusted, retorno );serie
  
#### Comportamento da serie
plot_serie = serie %>%
  autoplot(price) +
  labs(title="Preço Ajustado do indice Bovespa",
       y="")
plot_serie

#### Comportamento da serie
plot_retorno = serie %>%
  autoplot(retorno) +
  labs(title="Retorno do indice Bovespa",
       y="")
plot_retorno

## plotar os graficos em grid
gridExtra::grid.arrange(plot_serie, plot_retorno )



# Transformar retorno em vetor numérico
retorno <- serie$retorno

## Testar se existe efeito ARCH e GARCH
FinTS::ArchTest(retorno, lags = 12)

# Especificar modelo GARCH(1,1)
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
  distribution.model = "norm"
)

# Ajustar modelo
fit <- ugarchfit(spec = spec, data = retorno)
fit

## Previsao

prev <- ugarchforecast(fit, n.ahead = 12)
sigma(prev) ## previsao volatilidade



### criptomoedas 

btc <- tq_get("BTC-USD", from = "2020-01-01", to = Sys.Date())
shiba <- tq_get("SHIB-USD", from = "2020-01-01", to = Sys.Date())
eth <- tq_get("ETH-USD", from = "2020-01-01", to = Sys.Date())
render <- tq_get("RENDER-USD", from = "2020-01-01", to = Sys.Date())
sol <- tq_get("SOL-USD", from = "2020-01-01", to = Sys.Date())




