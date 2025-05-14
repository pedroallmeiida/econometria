### Pacotes

library(GLMsData)
library(ISLR)
library(MASS)

### Dados
data("Boston")
View(Boston)


### variavel resposta
hist(Boston$medv)


#### MODELO GAMA

mod = glm( formula = medv ~ crim + zn  + chas 
           + nox + rm  + dis + rad 
           + tax+ ptratio + black + lstat 
           , data = Boston , family = Gamma(link = 'log')  )
summary(mod)
names(Boston)


## Real versus predito
plot( Boston$medv, mod$fitted.values )



# Pseudo- R2

modEvA::RsqGLM(mod)


### Analise de residuo

## residuo quantilico
res = statmod::qresiduals(mod)

hist( res )
qqnorm(res)
qqline( res, col = 'red' )


cooksD = cooks.distance(mod)
n <- nrow(Boston)
plot(cooksD, main = "Cooks Distance for Influential Obs")
abline(h = 4/n, lty = 2, col = "steelblue") # add cutoff line

## removendo dados influentes 
influential_obs <- as.numeric(names(cooksD)[(cooksD > (4/n))])
BOSTON_new <- Boston[-influential_obs, ]

### NOVO AJUSTE COM DADOS SEM PONTOS INFLUENTES


mod = glm( formula = medv~crim + zn  + chas 
           + nox + rm  + dis + rad 
           + tax+ ptratio + black + lstat 
           , data = BOSTON_new , family = Gamma(link = 'log')  )
summary(mod)


#R2

modEvA::RsqGLM(mod)

### Analise de residuo

## residuo quantilico
res = statmod::qresiduals(mod)

hist( res )
qqnorm(res)
qqline( res, col = 'red' )


cooksD = cooks.distance(mod)
n <- nrow(Boston)
plot(cooksD, main = "Cooks Distance for Influential Obs")
abline(h = 4/n, lty = 2, col = "steelblue") # add cutoff line


## removendo dados influentes 
#identify influential points
influential_obs <- as.numeric(names(cooksD)[(cooksD > (4/n))])

#define new data frame with influential points removed
BOSTON_new <- BOSTON_new[-influential_obs, ]


mod = glm( formula = medv~crim + zn  + chas 
           + nox + rm  + dis + rad 
           + tax+ ptratio + black + lstat 
           , data = BOSTON_new , family = Gamma(link = 'log')  )
summary(mod)
names(Boston)


#R2

modEvA::RsqGLM(mod)

### Analise de residuo
res = statmod::qresiduals(mod)
hist( res )
qqnorm(res)
qqline( res, col = 'red' )

## Real versus predito
plot( BOSTON_new$medv, mod$fitted.values )

## Envelope Simulado
hnp::hnp(resid(mod, type = 'deviance'), sim = 1000,how.many.out=T , resid.type = 'deviance',
         conf = 0.95,scale = T)




