install.packages("Rcmdr", dependencies = TRUE)
library(Rcmdr)
library(dplyr)
library(readr)
library(dgof)
library(skedastic)
library(car)

setwd("C:/Users/lvfli/Desktop/TrabalhoRegressao")
regressão <- readXL("EsgotamentoXDoença2.xlsx")
head(regressão)

summary(regressão)
colnames(regressão)

#lm roda a regressão linear, sinal de soma é para inserir mais variáveis independentes
#o que fica a esquerda de ~ é a variável dependente, que quero explicar.

modelo1 <- lm(formula = Sociedade.de.economia.mista ~ 
                 Cólera + Verminoses  + Malária + Hepatite +
              + Febre.amarela , data=regressão)

modelo2 <- lm(formula = Administração.direta.do.poder.público ~
                 Verminoses  + Malária
                , data=regressão)

summary(modelo1)
summary(modelo2)

#testes de normalidade dos resíduos

u <- modelo1$residuals
u2 <-modelo2$residuals

dgof::ks.test(u, 'pnorm', mean(u), sd(u))
dgof::ks.test(u2, 'pnorm', mean(u2), sd(u2))

shapiro.test(u)
shapiro.test(u2)

#teste de homocedasticidade

white(modelo1)
white(modelo2)
breusch_pagan(modelo1)
breusch_pagan(modelo2)

#teste de multicolinearidade

car::vif(modelo1)
car::vif(modelo2)

modelo3 <- lm(formula = Sociedade.de.economia.mista ~ 
                + Malária +
                + Febre.amarela , data=regressão)
summary(modelo3)
u3 <- modelo3$residuals
dgof::ks.test(u3, 'pnorm', mean(u3), sd(u3))
shapiro.test(u3)
white(modelo3)
breusch_pagan(modelo3)
car::vif(modelo3)
