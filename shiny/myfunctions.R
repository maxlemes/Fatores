#' @title Organizado o Ibra
#' @name b3_ibra
#'
#' @description Organiza a planilha (csv) com o índice Ibra no site da B3.
#'
#' @param file <- "data-raw/nome_do_arquivo.csv".
#'
#' @details Baixe a planilha no site http://www.b3.com.br/pt_br/market-data-e-indices/indices/indices-amplos/indice-brasil-amplo-ibra-composicao-da-carteira.htm e salve na pasta "data-raw/"
#'
#' @return Retorna um data frame com os dados do Ibra na pasta "data/".
#'
#' @author Max Lemes
#'
#' @examples
#' file <- "data-raw/IBRADia_22-06-21.csv"
#' b3_ibra(file)
#'
#' @export

require(magrittr)

b3_ibra <- function(file){
  df <- read.table(file, sep = ";", dec = ",", header = FALSE, fill = TRUE)
  
  df <- df[3:(nrow(df)-2),c(1,2,4,5)]
  
  colnames(df) <- c("Ticker", "Nome", "Qtde", "Fatia")
  
  df$Fatia <- as.numeric(sub(",", ".", df$Fatia, fixed = TRUE))
  
  df$yahoo <- paste0(df$Ticker, ".SA")
  
  ibra <- df
  save(ibra, file = "data/ibra.rda")
}

#Get the data from yahoo
getdata<- function(symbols){
  yahoo_data <- new.env()
  quantmod::getSymbols(
    symbols, 
    src = "yahoo", 
    from =  Sys.Date() - lubridate::days(365),
    to = Sys.Date(), 
    env = yahoo_data)
  save(yahoo_data, file = "data/yahoo_data.Rdata")
  yahoo_returns <- as.environment(eapply(yahoo_data, quantmod::dailyReturn))
  save(yahoo_returns, file = "data/yahoo_returns.Rdata")
}

#Drop all columns except Adjusted Close inside enviroment
yahoo_prices <- function(symbols, yahoo_data){
  for(symbol in symbols) {
    #-------------------- Filtrando apenas os Preços Ajustados -----------------------
    x <- get(symbol, pos = yahoo_data)
    x <- x[,6] #drops all columns except Adjusted Close which is 6th column
    colnames(x) <- gsub(".SA.Adjusted", "", colnames(x))
    assign(symbol, x, pos = yahoo_data)
    #---------------------- Renomeando as colunas dos retornos -----------------------
    y <- get(symbol, pos = yahoo_returns)
    colnames(y) <- colnames(x)
    assign(symbol, y, pos = yahoo_returns)
  }
}

# Calculando os retornos para o cálculo do momentum 
mom <- function(df, n){
  n_ini <- 252/n
  n_fim <- as.integer(19/n)
  n_fim <- 0
  df <- TTR::ROC(df, n = n_ini - n_fim, type = "discrete")
  df <- df[nrow(df)-n_fim,]
  df <- tidyr::gather(dplyr::as_tibble(df), Ticker, mom) # transfomando em um tibble
  return(df)
}

#Criando o tibble com o percentual dos retornos
pos_neg <- function(df, n){
  t_1 <- nrow(df) - 252/n
  t_2 <- nrow(df) - as.integer(19/n)
  ##Separando os retornos positivos (1) e negativos (0)
  df <- (1 + sign(df))/2 
  df <- replace(df, list = is.na(df), values = 0)
  #-----------------------
  df <- df[t_1:t_2,]  # separando o intervalo de interesse
  df[nrow(df),] <- raster::colSums(df)/nrow(df) # calculando o percentual de dias positivos
  df <- df[nrow(df),]  # separando a linha com o o percentual de dias positivos
  df <- tidyr::gather(dplyr::as_tibble(df), Ticker, pos) # transfomando em um tibble
  df$neg <- 1 - df$pos # calculando os percentuais negativos
  return(df)
}

# Prepara a Tabela para publicação
table_momentum <- function(df){
  df %>%
    dplyr::mutate(
      pos = formattable::percent(pos),
      mom = formattable::percent(mom),
      vol = formattable::percent(vol),
      Sharpe = round(Sharpe,2)
    )%>%
    dplyr::rename(
      "% de Dias Positivos" = pos,
      "Rendimento no Período" = mom,
      "Volatilidade" = vol, 
    )%>%
    dplyr::arrange(Volatilidade)
}

momentum <- function(tipo_mom){
  
  # Calculando o Momentum do período
  roc <-mom(adjusted_prices, tipo_mom)

  
  # Calculando o percentual de positivos e negativos no período
  fip <- pos_neg(returns, tipo_mom)
    
  # Juntando o Momentum com o % Positivos
  fic_roc <- quality_mom(fip, roc)
  
  # Arrumando a tabela para publicação
  return(table_momentum(fic_roc))
}

periodo <- function(tipo_mom){
  data_ini <-  zoo::index(returns[nrow(returns) - 252/tipo_mom])
  data_fim <-  zoo::index(returns[nrow(returns) - as.integer(19/tipo_mom)])
  aux <- paste0("Período: ", format(data_ini, "%d-%b-%Y"), " até ", 
                format(data_fim, "%d-%b-%Y"))
  return(aux)
}

#calcula a volatilidade dos ativos
volatilidade <- function(returns){
  df <- returns[1:3,]
  for (i in 1:ncol(returns)){
    aux <- na.omit(returns[,i])
    df[1, i] <- sqrt(252)*sd(aux)
    df[2, i] <- 252*mean(aux)
    df[3, i] <- sqrt(252)*mean(aux)/sd(aux)
  }
  
  aux <- tidyr::pivot_longer(dplyr::as_tibble(df), 
                             cols = 1:ncol(df), 
                             names_to = "Ticker", 
                             values_to = "vol")
  aux1 <- aux[1:ncol(df),]
  
  aux2 <- aux[(ncol(df)+1):(2*ncol(df)),]
  colnames(aux2) <- c("Ticker","Mean")
  
  aux3 <- aux[(2*ncol(df)):nrow(aux),]
  colnames(aux3) <- c("Ticker","Sharpe")
  
  aux <- dplyr::left_join(aux1, aux2)
  aux <- dplyr::left_join(aux, aux3)
  
  aux %>% dplyr::arrange(-Sharpe)
  
  return(aux)
}

div <- function(df, n){
  df %>%
    dplyr::filter(`DIVIDA LIQUIDA / EBIT`< 3, `CAGR LUCROS 5 ANOS` > 0)%>%
    dplyr::arrange(-DY)%>%
    dplyr::distinct(`Nome`, .keep_all = TRUE)%>%
    .[1:n,c(1:3,6,7,4,5)] %>%
    dplyr::mutate(
      DY = formattable::percent(DY/100),
      vol = formattable::percent(vol),
      Sharpe = round(Sharpe,2)
    )%>%
    dplyr::rename(
      "Volatilidade" = vol, 
    )%>%
    dplyr::arrange(Volatilidade)
}

qualy <-  function(df, n){
  df %>%
    dplyr::mutate(rank1  = rank(ROE),
                  rank2 =  rank(`CAGR LUCROS 5 ANOS`),
                  rank3 = rank(-`DIV. LIQ. / PATRI.`),
                  score = rank1 + rank2 + rank3
    )%>%
    dplyr::distinct(`Nome`, .keep_all = TRUE)%>%
    dplyr::arrange(-score)%>%
    .[1:n,c(1:3,6,22,29,17,4,5)]%>%
    dplyr::mutate(
      ROE = formattable::percent(ROE/100),
      `CAGR LUCROS 5 ANOS` = formattable::percent(`CAGR LUCROS 5 ANOS`/100),
      `DIV. LIQ. / PATRI.` = formattable::percent(`DIV. LIQ. / PATRI.`),
      vol = formattable::percent(vol),
      Sharpe = round(Sharpe,2)
    )%>%
    dplyr::rename(
      "Volatilidade" = vol, 
    )%>%
    dplyr::arrange(Volatilidade)
}


val <- function(df, n){
  df %>%
    dplyr::filter(`CAGR LUCROS 5 ANOS` > 0,
                  `ROE` > 0)%>%
    dplyr::arrange(`P/VP`)%>%
    dplyr::distinct(`Nome`, .keep_all = TRUE)%>%
    .[1:n,c(1:3,6,9,4,5)]%>%
    dplyr::mutate(
      vol = formattable::percent(vol),
      Sharpe = round(Sharpe,2)
    )%>%
    dplyr::rename(
      "Volatilidade" = vol, 
    )%>%
    dplyr::arrange(Volatilidade)
}

siz <- function(df, n){
  df %>%
    dplyr::arrange(`VALOR DE MERCADO`)%>%
    .[1:as.integer((nrow(df)/2+1)),] %>%
    dplyr::filter( 
      `CAGR LUCROS 5 ANOS` > 0, `ROE` > 0, `PEG Ratio` > 0)%>%
    dplyr::mutate(rank1  = rank(-`PEG Ratio`),
                  rank2 =  rank(`CAGR RECEITAS 5 ANOS`),
                  score = rank1 + rank2)%>%
    dplyr::arrange(-score)%>%
    dplyr::distinct(`Nome`, .keep_all = TRUE)%>%
    .[1:n,c(1:3,6,33,28,34,4,5)]%>%
    dplyr::mutate(
      `PEG Ratio` = formattable::percent(`PEG Ratio`),
      `CAGR RECEITAS 5 ANOS` = formattable::percent(`CAGR RECEITAS 5 ANOS`/100),
      vol = formattable::percent(vol),
      Sharpe = round(Sharpe,2)
    )%>%
    dplyr::rename(
      "Volatilidade" = vol, 
    )%>%
    dplyr::arrange(Volatilidade)
}

