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

urlsegmentos <- "http://www.b3.com.br/lumis/portal/file/fileDownload.jsp?fileId=8AA8D0975A2D7918015A3C81693D4CA4"

download.file(urlsegmentos, "data-raw/ClassifSetorial.zip")
unzip("data-raw/ClassifSetorial.zip",exdir="data-raw/")
file <- list.files(path="data-raw/", pattern=".xlsx")
file.rename(paste0("data-raw/",file), "data-raw/ClassifSetorial.xlsx")

df <- readxl::read_excel("data-raw/ClassifSetorial.xlsx")
colnames(df) <- c("Setor", "Subsetor", "Nome", "Ticker", "Segmento")

df$Segmento <- as.character(NA)

for (i in which(is.na(df$Ticker))){
 df[i,5] <-  df[i,3]
 df[i,3] <- "Nome"
 df[i,4] <- "Ticker"
}

df <- df[,c(3,1,2,5)]

df <- zoo::na.locf(df)

df <- df %>%
  dplyr::filter(Ticker != "LISTAGEM",
                Ticker !=  "CÓDIGO",
                Ticker !=  "Ticker")

segmentos <- df

save(segmentos, file = "data/segmentos.rda")


urlsocial <- "http://bvmf.bmfbovespa.com.br/CapitalSocial"

CapitalSocial <-  xml2::read_html(urlsocial) %>% 
  rvest::html_table()

CapitalSocial <- CapitalSocial[[1]]
