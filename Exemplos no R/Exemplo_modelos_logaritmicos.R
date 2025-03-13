## Instalando a bliblioteca 
#install.packages("wooldridge")

# carrega a biblioteca
library(wooldridge)

# leitura do dataset
?data("wage1")


View(wage1)

## Modelo 1: Modelo de regressão linear simples
wageModel <- lm(wage ~ educ + exper + tenure, data = wage1)
summary(wageModel)


## Modelo 2: Modelo de regressão log-log
wageModel <- lm( log(wage) ~ log(exper), data = wage1)
summary(wageModel)


## Modelo 3: Modelo de regressão log-lin
wageModel <- lm( lwage ~ educ + exper + tenure, data = wage1)
summary(wageModel)


## Modelo 4: Modelo de regressão lin-log
wageModel <- lm( wage ~ log(exper), data = wage1)
summary(wageModel)





## IDH 
url <- "https://raw.githubusercontent.com/pedroallmeiida/econometria/refs/heads/main/Dados/idh.csv"
dados_idh <- readr::read_csv(url)
head(dados_idh)
names(dados_idh)




## Modelo 1: Modelo de regressão linear simples
idhModel <- lm( idh ~ log(media_de_anos_escola), data = dados_idh)
summary(idhModel)
