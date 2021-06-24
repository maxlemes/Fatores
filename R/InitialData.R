rm(list=ls())

source("shiny/myfunctions.R")

file <- "data-raw/IBRADia_24-06-21.csv"

b3_ibra(file)

load("data/ibra.rda")

#----------------- Baixando Dados do yahoo --------------------------
getdata(ibra$yahoo)

#------------------------- Lendos dados salvos -----------------------
load("data/yahoo_data.Rdata")
load("data/yahoo_returns.Rdata")

#---------------------- Arrumando o enviroment -------------------
yahoo_prices(ibra$yahoo, yahoo_data)

#Criando o xts dos preços ajustados
adjusted_prices <- do.call(merge, lapply(ibra$yahoo, get, pos = yahoo_data))
save(adjusted_prices, file = "data/adjusted_prices.Rdata")

#Criando o xts dos retornos
returns <- do.call(merge, lapply(ibra$yahoo, get, pos = yahoo_returns))
save(returns, file = "data/returns.Rdata")

last_date <- format(max(zoo::index(returns)), format = "%d/%m/%Y")
save(last_date, file = "shiny/last_date.rda")

#calcula a volatilidade dos ativos
vol <- volatilidade(returns)
vol <- dplyr::left_join(ibra[,1:2], vol)
vol <- vol[,-4]

# Salvando o arquivo com a volatilidade
ibra <- vol
save(ibra, file = "shiny/ibra.rda")

#------------------- Definindo o Momentum  ---------------------------

# Escolha o tipo de momento, 
# 1 - 12m-1m, 2 - 6m-2s, 4 - 3m - 1s 
tipo_mom <- 6

# Calculando o Momentum do período
roc <- mom(adjusted_prices, tipo_mom)

# Selecionando as 60 melhores
roc <- roc %>%
  dplyr::arrange(-mom)%>%
  .[1:60,]

# Calculando o percentual de positivos e negativos no período
fip <- pos_neg(returns, tipo_mom)

#das 60 anteriores selecionamos as 30 melhores
quality_mom <- dplyr::left_join(roc, fip)
quality_mom <-  quality_mom %>%
  dplyr::arrange(-pos)%>%
  .[1:30,c(1,2,3)]

#calcula a volatilidade dos 30 ativos
vol <- dplyr::left_join(quality_mom, vol)
vol <- vol[, c(1,4,2,3,5,6)]

# Arrumando a tabela para publicação
momentum <- table_momentum(vol)

# Salvando o arquivo com o momentum
save(momentum, file = "shiny/momentum.rda")
