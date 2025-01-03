library(esquisse)
library(ggplot2)
library(readxl)
library(knitr)
library(dplyr)

setwd("C:/Users/lvfli/Desktop/RStudio/Projeto/")
reclamacoes <- read.csv("rec2016.csv")


summary(reclamacoes)
esquisser(reclamacoes)

reclamacoes$DataArquivamento = as.Date(reclamacoes$DataArquivamento)
reclamacoes$DataAbertura = as.Date(reclamacoes$DataAbertura)
reclamacoes$strRazaoSocial= as.factor(reclamacoes$strRazaoSocial)
reclamacoes$UF = as.factor(reclamacoes$UF)
reclamacoes$CodigoProblema= as.factor(reclamacoes$CodigoProblema)

#arquivamentos2= as.double(arquivamentos[4,'freq'])
#mean(arquivamentos2(n))
#pct <- paste(round(arquivamentos$freq * 100), '%', sep = '')


# As 10 reclama��es mais frequentes 

frequenciaproblemas <- count(reclamacoes, CodigoProblema)
frequenciaproblemas <- arrange(frequenciaproblemas, -n)
kable(head(frequenciaproblemas, 10)) 

#tabelalista = as.data.frame(frequenciaproblemas[1:10,])


# As 10 empresas que mais recebem reclama��es

topempresas <- count(reclamacoes, strRazaoSocial,)
topempresas <- arrange(topempresas, desc(n))
kable(head(topempresas, 10))
tabelalista2 = as.data.frame(topempresas[1:10,])

# grau de resolu��o de atendimentos.
#summarise cria data frame e agrupa dados, juntando todas as informa��es da coluna atendida
#mutate literalmente muta colunas adicionando novas vari�veis preservando as j� existentes
resolvido <- reclamacoes %>%
        group_by(Atendida) %>%
        summarise (n = n()) %>%
        mutate(freq = n / sum(n))

grauderesolu��o <- as.double(resolvido[2, 'freq'])
pct <- paste(round(resolvido$freq * 100), '%', sep = '')


pie(resolvido$freq, paste(resolvido$resolvido, pct, sep = ' - '))


# frequencia pelo genero
sexo <- reclamacoes %>%
        group_by(SexoConsumidor) %>%
        summarise (n = n()) %>%
        mutate(freq = n / sum(n))

M = as.double(sexo[2, 'freq'])

pct <- paste(round(sexo$freq * 100), '%', sep = '')
pie(sexo$freq, paste(sexo$sexo, pct, sep = ' - '))


#51% das reclama��es foram feitas por mulheres enquanto 47% foram realizados por
#homens, 2% das pessoas n�o informaram seu g�nero no momento da fundamenta��o da 
#reclama��o


#teste de quick plot por estado

qplot(x = reclamacoes$UF, data = reclamacoes)






esquisser(reclamacoes)

library(dplyr)

#Plot 1 : Quantas pessoas foram ou n�o atendidas pelas empresas em 2016
#Gr�fico dividido por regi�es.

reclamacoes %>%
 filter(!(Atendida %in% "")) %>%
 filter(!(SexoConsumidor %in% "")) %>%
 filter(!(FaixaEtariaConsumidor %in% 
 "")) %>%
 ggplot() +
 aes(x = Atendida, weight = CodigoProblema) +
 geom_bar(fill = "#112446") +
 labs(x = "Foi Atendido?", 
 y = "Quantidade") +
 theme_bw() +
 facet_wrap(vars(Regiao), nrow = 1L)

#Plot 2 Pessoas atendidas ou n�o por sexo e regi�o.

reclamacoes %>%
        filter(!(Atendida %in% "")) %>%
        filter(SexoConsumidor %in% c("M", "F")) %>%
        filter(!(FaixaEtariaConsumidor %in% 
                         "")) %>%
        ggplot() +
        aes(x = Atendida, fill = SexoConsumidor) +
        geom_bar(position = "dodge") +
        scale_fill_brewer(palette = "Set1", 
                          direction = 1) +
        labs(x = "Foi Atendido?", y = "Quantidade", title = "Atendimentos em Rela��o ao Sexo") +
        theme_bw() +
        facet_wrap(vars(Regiao), nrow = 1L)


#Plot3
esquisser(reclamacoes)

reclamacoes %>%
        filter(!(Atendida %in% "")) %>%
        filter(!(SexoConsumidor %in% "")) %>%
        filter(!(FaixaEtariaConsumidor %in% 
                         c("Nao Informada", ""))) %>%
        ggplot() +
        aes(x = FaixaEtariaConsumidor, fill = Atendida) +
        geom_bar(position = "dodge") +
        scale_fill_brewer(palette = "Set1", direction = 1) +
        labs(x = "Faixa Etaria Consumidores", y = "Quantidade") +
        theme_bw() +
        facet_wrap(vars(Regiao), ncol = 1L)


#Plot 4 - Atendimentos por Estado

reclamacoes %>%
        filter(!(Atendida %in% "")) %>%
        filter(SexoConsumidor %in% c("M", "F")) %>%
        filter(!(FaixaEtariaConsumidor %in% 
                         "")) %>%
        ggplot() +
        aes(x = Atendida, fill = Atendida) +
        geom_bar(position = "dodge") +
        scale_fill_brewer(palette = "Set2", direction = 1) +
        labs(
                x = "Sexo Consumidor",
                y = "Quantidade",
                title = "Atendimentos por Estado"
        ) +
        theme_bw() +
        facet_wrap(vars(UF), nrow = 1L)

#Plot 5 reclama��es UF

reclamacoes %>%
        filter(!(Atendida %in% "")) %>%
        filter(SexoConsumidor %in% c("M", "F")) %>%
        filter(!(FaixaEtariaConsumidor %in% 
                         "")) %>%
        ggplot() +
        aes(x = UF) +
        geom_bar(fill = "#440154") +
        labs(y = "Quantidade", title = "Reclama��es por Estado") +
        theme_minimal()

#Plot 6 reclama��es df atendidos e n�o atendidos

reclamacoes %>%
        filter(!(Atendida %in% "")) %>%
        filter(!(SexoConsumidor %in% "")) %>%
        filter(!(FaixaEtariaConsumidor %in% 
                         "")) %>%
        ggplot() +
        aes(x = Atendida, fill = Atendida) +
        geom_bar() +
        scale_fill_brewer(palette = "Set1", direction = 1) +
        labs(
                x = "Foi Atendido",
                y = "Quantidade",
                title = "Quantidade Atendimentos por Estado"
        ) +
        theme_bw() +
        facet_wrap(vars(UF), nrow = 1L)

