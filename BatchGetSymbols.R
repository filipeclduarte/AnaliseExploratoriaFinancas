# Carregando o pacote BatchGetSymbols para baixar dados de cotações de ações da B3
library(BatchGetSymbols)

# Carregando outras biliotecas
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(stringr)

# Importando dados da Petrobrás - PETR4
acao <- "PETR4.SA"
# Data inicial
first.date <- Sys.Date() - 60
# Data final - hoje
last.date <- Sys.Date()
bench <- "^BVSP"

petr4 <- BatchGetSymbols(acao, first.date = first.date, last.date = last.date, 
                         bench.ticker = bench)
glimpse(petr4)

bvsp <- BatchGetSymbols(bench, first.date = first.date, last.date = last.date)
glimpse(bvsp)

# Plotando o gráfico da PETR4
ggplot(petr4$df.tickers, aes(x = ref.date, y = price.close))+
  geom_line()+
  xlab('')+ylab('Preço')+
  labs(title='Petrobrás',
       caption='Fonte: dados do Yahoo Finance.')

# Plotando o gráfico do Ibov
ggplot(bvsp$df.tickers, aes(x = ref.date, y = price.close))+
  geom_line()+
  xlab('')+ylab('Índice')+
  labs(title='Ibovespa',
       caption='Fonte: dados do Yahoo Finance.')


# Selecionando apenas a data, cotação de abertura e de fechamento, e retorno de fechamento
base_petr4 <- petr4$df.tickers %>% 
  select(ref.date, price.open, price.close, ret.closing.prices)

base_bvsp <- bvsp$df.tickers %>% 
            select(ref.date, price.open, price.close, ret.closing.prices)

glimpse(base_petr4)
glimpse(base_bvsp)

base <- base_bvsp %>% 
            right_join(base_petr4, by = "ref.date")

names(base) <- c("data", "price.open.bvsp", "price.close.bvsp", 
                 "ret.closing.prices.bvsp", "price.open.petr4", 
                 "price.close.petr4", "ret.closing.prices.petr4")

glimpse(base)

dados <- base %>%
  select_all() %>% 
  filter(!is.na(ret.closing.prices.petr4))

dados

# empilhando

dados_ret <- dados %>% 
  select(data,ret.closing.prices.bvsp, ret.closing.prices.petr4) %>% 
  gather(key = "ativo", value = "retorno", -data, 
         ret.closing.prices.bvsp, ret.closing.prices.petr4)


dados_ret$ativo <- dados_ret$ativo %>% 
                        str_replace_all(c("ret.closing.prices.bvsp" = "bvsp", 
                                          "ret.closing.prices.petr4" = "petr4"))

dados_ret

# gráfico do retorno
ggplot(dados_ret, aes(x = data, y = retorno, color = as.factor(ativo))) +
  geom_line() + ylab("Retorno") + xlab("Dia") + ggtitle("Retorno diário") +
  scale_color_discrete(name = "Legenda", labels = c("Ibovespa", "Petrobrás"))
  
# com facet_wrap()
ggplot(dados_ret, aes(x = data, y = retorno, color = as.factor(ativo))) +
  geom_line() + ylab("Retorno") + xlab("Dia") + ggtitle("Retorno diário") + 
  scale_color_discrete(name = "Legenda", labels = c("Ibovespa", "Petrobrás")) +
  facet_wrap(~ativo, ncol = 2)





