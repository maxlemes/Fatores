rm(list=ls())

source("shiny/myfunctions.R")

file <- "data-raw/IBRADia_01-07-21.csv"

b3_ibra(file)

load("data/ibra.rda")
load("data/segmentos.rda")
ibra <- dplyr::left_join(ibra,segmentos[,2:5])

#----------------- Baixando Dados do yahoo --------------------------
#getdata(ibra$yahoo)

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
vol <- dplyr::left_join(ibra[,c(1:2,8)], vol)
vol <- vol[,-5]

# Salvando o arquivo com a volatilidade
ibra <- vol
save(ibra, file = "shiny/ibra.rda")

#------------------- Baixando os dados  ---------------------------

urlstatus <- readr::read_file("shiny/status.txt")
status <- readr::read_csv2(urlstatus)

status <- status %>%
  dplyr::rename(Ticker = "TICKER", Preço = "PRECO")

status <- dplyr::left_join(ibra,status)

#------------------- Fatores  ---------------------------

Dividendos <- div(status, 30)

Quality <- qualy(status, 30)

Valor <- val(status, 30)

Size <- siz(status, 30)

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
fip <- pos_neg(returns)

#das 60 anteriores selecionamos as 30 melhores
quality_mom <- dplyr::left_join(roc, fip)
quality_mom <-  quality_mom %>%
  dplyr::arrange(-pos)%>%
  .[1:30,c(1,2,3)]

#calcula a volatilidade dos 30 ativos
vol <- dplyr::left_join(quality_mom, status[,1:6])
vol <- vol[, c(1,4,5,8,2,3,6,7)]

# Arrumando a tabela para publicação
momentum <- table_momentum(vol)

# Salvando o arquivo com o momentum
save(momentum, file = "shiny/momentum.rda")


all_factors <- Dividendos[,1:2]
all_factors[,2] <- Quality[,1]
colnames(all_factors) <- c("Dividendos", "Quality")
all_factors$Valor <- Valor[,1]
all_factors$Size <- Size[,1]
all_factors$Momentum <- momentum[[1]]

save(all_factors, file = "data/all_factors.rda")

fatores <- all_returns(all_factors, returns)

load("data/ibra.rda")

ibra <- ibra[,c(1,4)]
ibra$Fatia <- ibra$Fatia/100
aux <- returns[(nrow(returns)-20):nrow(returns),ibra[,1]]
aux <- zoo::na.locf(aux)
aux <- sweep(aux, 2, ibra$Fatia, "*")

aux$IBrA <- rowSums(aux)
fatores <- fatores[,1:5]
fatores <- cbind(fatores, aux$IBrA)

save(fatores, file = "shiny/fatores.rda")

load("shiny/fatores.rda")

PerformanceAnalytics::charts.PerformanceSummary(fatores[,c(5,6)], colorset = c(1:5))

