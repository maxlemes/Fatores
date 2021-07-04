rm(list=ls())

library(tidyverse)
library(tidyquant)
#source("shiny/myfunctions.R")

b3_ibra <- function(file){
  df <- read.table(file, sep = ";", dec = ",", header = FALSE, fill = TRUE)
  
  df <- df[3:(nrow(df)-2),c(1,2,4,5)]
  
  colnames(df) <- c("Ticker", "Nome", "Qtde", "Fatia")
  
  df$Fatia <- as.numeric(sub(",", ".", df$Fatia, fixed = TRUE))
  
  df$yahoo <- paste0(df$Ticker, ".SA")
  
  ibra <- df
  save(ibra, file = "data/ibra.rda")
}

#Criando o tibble com o percentual dos retornos
pos_neg <- function(df){
  t_1 <- nrow(df) - 126
  t_2 <- nrow(df)
  ##Separando os retornos positivos (1) e negativos (0)
  df <- (1 + sign(df))/2 
  df <- replace(df, list = is.na(df), values = 0)
  #-----------------------
  #df <- df[t_1:t_2,]  # separando o intervalo de interesse
  df[nrow(df),] <- raster::colSums(df)/nrow(df) # calculando o percentual de dias positivos
  df <- df[nrow(df),]  # separando a linha com o o percentual de dias positivos
  df <- tidyr::gather(dplyr::as_tibble(df), yahoo, pos) # transfomando em um tibble
  #df$neg <- 1 - df$pos # calculando os percentuais negativos
  return(df)
}
#-------- Atualizando o IBrA ----------------------
file <- "data-raw/IBRADia_01-07-21.csv"

b3_ibra(file)

load("data/ibra.rda")
load("data/segmentos.rda")
ibra <- dplyr::left_join(ibra,segmentos[,2:5])
#-------------------  FIM  ------------------------


# Definindo o período a ser analisado -------------
data_fim <- as.Date("2021-07-01")
data_ini <- data_fim - lubridate::days(365) #pull(Ra.SA[nrow(Ra.SA),2]) 

# Ra armazena os retornos dos ativos------------------
# Baixando os dados
Ra.SA <- ibra$yahoo %>%
  tidyquant::tq_get(get  = "stock.prices",
                    from = data_ini,
                    to   = data_fim) %>%
  dplyr::group_by(symbol) 


# Organizando os dados
Ra <- Ra.SA %>%
  tidyquant::tq_transmute(select = adjusted, 
                          mutate_fun = periodReturn, 
                          period     = "monthly", 
                          col_rename = "Ra") %>%
  rename(yahoo = "symbol")
save(RaSa, file = "data/RaSA.rda")


RaD <- Ra.SA %>%
  tidyquant::tq_transmute(select = adjusted, 
                          mutate_fun = periodReturn, 
                          period     = "daily", 
                          col_rename = "Ra") %>%
  rename(yahoo = "symbol")
save(Ra, file = "data/Ra.rda")

#Construindo o portfolio do IBrA ----------------------
wts <- tibble(
  yahoo = ibra$yahoo,
  weights = ibra$Fatia
)

ibra_returns_monthly <- Ra %>%
  tq_portfolio(assets_col  = yahoo, 
               returns_col = Ra, 
               weights     = wts, 
               col_rename  = "Ra")
# 
# #gráfico de performance
# ibra_returns_monthly %>%
#   ggplot(aes(x = date, y = Ra)) +
#   geom_line(size = 2, color = palette_light()[[1]]) +
#   labs(title = "Portfolio Growth",
#        subtitle = "50% PETR4, 0% WEGE3, and 50% ABEV3",
#        caption = "Now we can really visualize performance!",
#        x = "", y = "Portfolio Value") +
#   geom_smooth(method = "loess") +
#   theme_tq() +
#   scale_color_tq() +
#   scale_y_continuous(labels = scales::dollar)

# Rb armazena os retornos do Benchmark ----------------
Rb <- "^BVSP" %>%
  tq_get(get  = "stock.prices",
         from = data_ini,
         to   = data_fim) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Rb")

#Juntando os dados em RaRb ----------------------------
RaRb <- left_join(Ra, Rb, by = c("date" = "date"))

#Calculando o CAPM
RaRb_capm <- RaRb %>%
  tq_performance(Ra = Ra, 
                 Rb = Rb, 
                 performance_fun = table.CAPM)

#Calculando o Sharpe
Ra_sharpe <- Ra %>%
  tq_performance(
    Ra = Ra, 
    Rb = NULL, 
    performance_fun = SharpeRatio, 
    Rf = 0.0425 / 12, 
    p  = 0.99
  )

#Tabela dos retornos anuais dos ativos
RaRb_annualized_returns <- RaRb  %>%
  tq_performance(Ra = Ra, Rb = NULL, 
                 Rf = 0.0425 / 12,  
                 performance_fun = table.AnnualizedReturns)


# Arrumando os retornos e o sharpe ----------------------------------

SR <- left_join(ibra[,c(1,2,5,8)], RaRb_annualized_returns)
SR <- SR[,c(1,2,4:7)] 
colnames(SR) <- gsub("Annualized","", colnames(SR))

#------------------- Definindo o Momentum  ---------------------------
# Considerando os retornos diário dos últimos 6 meses ----------

Ra6M <- Ra.SA %>% 
  dplyr::arrange(desc(date)) %>%
  .[1:(126*nrow(ibra)),]

Ra6M <- Ra6M %>%
  tidyquant::tq_transmute(select = adjusted, 
                          mutate_fun = dailyReturn, 
                          col_rename = "Ra") %>%
  rename(yahoo = "symbol")

ret <- pivot_wider(Ra6M,names_from = yahoo, values_from = Ra)
pos_returns <- pos_neg(ret[,2:ncol(ret)])

Ra6M <- Ra6M %>%
  group_by(yahoo)%>%
  summarise(mom = prod(1+Ra)-1) 

mom <- left_join(Ra6M, pos_returns, by = ("yahoo" = "yahoo"))
mom <- left_join(ibra[,c(1,2,5,8)], mom, by = ("yahoo" = "yahoo"))
SR <- left_join(SR, mom[,-3])
#------------------- Baixando os dados  ---------------------------

urlstatus <- readr::read_file("shiny/status.txt")
status <- readr::read_csv2(urlstatus)

status <- status %>%
  dplyr::rename(Ticker = "TICKER", Preço = "PRECO")

status <- dplyr::left_join(SR,status, by = ("Ticker" = "Ticker"))

status[is.na(status)] <- 0

#------------------- Fatores  ---------------------------

Dividendos <- div(status, 30)

Quality <- qualy(status, 30)

Valor <- val(status, 30)

Size <- siz(status, 30)



