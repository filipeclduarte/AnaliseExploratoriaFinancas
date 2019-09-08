# Séries Financeiras
# limpando a memória
rm(list=ls())

library(quantmod) # cotações das ações e do Ibov
library(Quandl) # Selic BACEN
library(ggplot2) # para gerar os gráficos
library(gridExtra) # para varios graficos juntos
library(dplyr)
library(tidyr)

env <- new.env()

getSymbols(c("PETR4.SA","^BVSP", "MGLU3.SA", "RADL3.SA"),
          env = env, periodicity='daily', 
           from='2017-01-01',
           to=Sys.Date())

bvsp <- env$BVSP
petr4 <- env$PETR4.SA
mglu3 <- env$MGLU3.SA
radl3 <- env$RADL3.SA

periodicity(petr4)

bvsp
head(petr4)
tail(petr4)

g1 <-  ggplot(petr4, aes(time(petr4), petr4$PETR4.SA.Adjusted)) + geom_line() +
  scale_x_date(date_labels =  "%d/%m/%Y", date_breaks = "2 months", 
               limits=c(min(time(petr4)),max(time(petr4)))) +
  xlab("") + ylab("PETR4") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
g1

g2 <- ggplot(bvsp, aes(time(bvsp), bvsp$BVSP.Adjusted)) + geom_line() +
  scale_x_date(date_labels =  "%d/%m/%Y", date_breaks = "2 months", 
               limits=c(min(time(bvsp)),max(time(bvsp)))) +
  xlab("") + ylab("BVSP") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
g2

g3 <- ggplot(mglu3, aes(time(mglu3), mglu3$MGLU3.SA.Adjusted)) + geom_line() +
  scale_x_date(date_labels =  "%d/%m/%Y", date_breaks = "2 months", 
               limits=c(min(time(mglu3)),max(time(mglu3)))) +
  xlab("") + ylab("MGLU3") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
g3

g4 <- ggplot(radl3, aes(time(radl3), radl3$RADL3.SA.Adjusted)) + geom_line() +
  scale_x_date(date_labels =  "%d/%m/%Y", date_breaks = "2 months", 
               limits=c(min(time(mglu3)),max(time(radl3)))) +
  xlab("") + ylab("RADL3") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
g4

# os dois graficos de cotacoes juntos  
grid.arrange(g1, g2, g3, g4,nrow = 2)

# Selic
dados <- merge(dailyReturn(petr4[,6],type='log')[-1,], 
               dailyReturn(bvsp[,6],type="log")[-1,])
dados
dados2 <- merge(dailyReturn(mglu3[,6],type='log')[-1,], 
               dailyReturn(radl3[,6],type="log")[-1,])
dados2

dados_f <- merge(dados, dados2)

names(dados_f) <- c("petr", "ibov", "mglu", "radl")

summary(dados_f)  
dados_f <- na.omit(dados_f)
dados_f_cum <- cumprod(1+dados_f)-1
summary(dados_f_cum)


acf(dados_f$petr)
acf(dados_f$ibov)
acf(dados_f$mglu)
acf(dados_f$radl)

ccf(as.numeric(dados_f$petr),as.numeric(dados_f$mglu))
ccf(as.numeric(dados_f$petr),as.numeric(dados_f$ibov))
ccf(as.numeric(dados_f$petr),as.numeric(dados_f$radl))
ccf(as.numeric(dados_f$ibov),as.numeric(dados_f$mglu))
ccf(as.numeric(dados_f$ibov),as.numeric(dados_f$radl))

# pegando a taxa selic mensal
Quandl.api_key('TC1ow5j6G7s4SFHTzgDz') # set your API key = Comando necessário pra acessar o Quandl
selic <- Quandl("BCB/4390",type = 'xts') # importando a serie do selic do Bacen

# juntando os dados petr4, ibov e selic
dados <- merge(dados_f,as.xts(selic/100),join="inner")

dados
# renomeando as colunas
names(dados)[5] <- "selic"


g1 <- ggplot(dados, aes(time(dados), dados$selic)) + geom_line() +
  scale_x_date(date_labels =  "%d/%m/%Y", date_breaks = "1 year", 
               limits=c(min(time(dados)),max(time(dados)))) +
  xlab("") + ylab("Selic") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
g1

# petr4
g2 <- ggplot(dados, aes(time(dados), dados$petr)) + geom_line() +
  scale_x_date(date_labels =  "%d/%m/%Y", date_breaks = "2 month", 
               limits=c(min(time(dados)),max(time(dados)))) +
  xlab("") + ylab("PETR4") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
g2
# ibov
g3 <- ggplot(dados, aes(time(dados), ibov)) + geom_line() +
  scale_x_date(date_labels =  "%m/%Y", date_breaks = "1 year", 
               limits=c(min(time(dados)),max(time(dados)))) +
  xlab("") + ylab("IBOV") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

grid.arrange(g1, g2, g3, nrow = 3)

# Pacotes no R para acesso a dados
# Fontes de dados nacionais


# GetTDData: Dados do Tesouro Direto (link)
# GetDFPData: Demostrativos financeiros de empresas listadas na B3 (link)
# GetHFTData: Dados de negociações em alta frequência da B3
# BETS: Brazilian economic time series
# Fontes de dados internacionais
# Quandl: The world’s most powerful data lives on Quandl (link).
# quantmod: Framework para modelagem em finanças quantitativas.
# tidyquant: Bringing financial analysis to the tidyverse.

acf()
