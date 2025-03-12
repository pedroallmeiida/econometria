
library(wooldridge)
library(AER)
library(Ecdat)

data(package = "wooldridge")
data(package = "AER")
data(package = "Ecdat")


### dados pacote wooldridge
data("phillips")
?phillips
cig_lm <- lm(inf ~ inf_1, data = phillips)
summary(cig_lm)

plot(phillips$inf, phillips$inf_1)

### dados pacote AER
data("CigarettesB")
CigarettesB
cig_lm <- lm(packs ~ price + income, data = CigarettesB)
summary(cig_lm)


## Pacote Ecdat
cig_lm <- lm(hours ~ ., data = Workinghours)
summary(cig_lm)

names(cig_lm)

## residuos 
res = cig_lm$residuals
hist(res, probability = TRUE, main = "Histograma com curva normal")
curve(dnorm(x, mean = mean(res), sd = sd(res)), add = TRUE, col = "red", lwd = 2)


qqnorm(res)
qqline(res, col = "red", lwd = 2)


## testes de hipoteses para os residuos 
library(nortest)
ad.test(res)
shapiro.test(res)
lillie.test(res)
ks.test(res, "pnorm", mean = mean(res), sd = sd(res))


## Autocorrelação e padrao homocedastico
acf( res  ) 
plot(res)



