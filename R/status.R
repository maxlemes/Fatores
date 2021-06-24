#'  @author Max Lemes, \email{max@@ufg.br}
#'
#' Solving the problems with the tickers of companies;
#'
#'
#' @return the data cadastro.rds
#'
#' @depends  tidyverse, dplyr, GetDFPData
#'
#' @export


rm(list=ls())

source("R/myfunctions.R")

load("shiny/ibra.rda")
ibra <- ibra%>%
  dplyr::rename(TICKER = "Ticker",
                NOME = "Nome")

urlstatus <- readr::read_file("shiny/status.txt")
status <- readr::read_csv2(urlstatus)

status <- dplyr::left_join(ibra,status)

status[is.na(status)] <- 0

Dividendos <- div(status, 30)

Quality <- qualy(status, 30)

Valor <- val(status, 30)

Size <- siz(status, 30)
