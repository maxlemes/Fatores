library(shiny)
library(magrittr)

source("myfunctions.R", local = TRUE)

#-------------------------------------------------------------------
load("ibra.rda")
load("last_date.rda")
load("momentum.rda")

#------------------- Baixando os dados  ---------------------------

urlstatus <- readr::read_file("status.txt")
status <- readr::read_csv2(urlstatus)

status <- status %>%
  dplyr::rename(Ticker = "TICKER", Preço = "PRECO")

status <- dplyr::left_join(ibra,status)

status[is.na(status)] <- 0

#------------------- Fatores  ---------------------------

Dividendos <- div(status, 30)

Quality <- qualy(status, 30)

Valor <- val(status, 30)

Size <- siz(status, 30)
Size <- na.omit(Size)

#----------------------------------- Pagina do usuario -------------------
ui <- fluidPage(
  title = "Factor Investing",
  
  # Application title
  headerPanel(HTML("<b><center>Carteiras para Factor Investing</b></center></br>")),
  
  
  fluidRow(
    conditionalPanel(
      'input.dataset === "Dividendos"',
      column(6, wellPanel(style = "height:180px;background:#40E0D0;",
                          h2("Fator Dividendos"),
                          br(),
                          h5(paste0("Atualização Diária"))),
             align="center"),
      column(6, wellPanel(style = "height:180px;background:#40E0D0;",
                          h3("Critérios Utilizados:"),
                          h5("CAGR LUCROS 5 ANOS > 0"),
                          h5("Dívida Líquida/EBIT < 3"),
                          h5("Score: Dividend Yield"),
                          
      ),
      align="left")
    ),     
    conditionalPanel(
      'input.dataset === "Qualidade"',
      column(6, wellPanel(style = "height:180px;background:#40E0D0;",
                          h2("Fator Qualidade"),
                          br(),
                          h5(paste0("Atualização Diária"))),
             align="center"),
      column(6, wellPanel(style = "height:180px;background:#40E0D0;",
                          h3("Critérios Utilizados:"),
                          h5("Rank1 = ROE"),
                          h5("Rank2 = CAGR LUCROS 5 ANOS"),
                          h5("Rank3 = Dívida Líquida/Patrimônio Líquido"),
                          h5("Score: A soma dos ranks")
                          
      ),
      align="left")
    ),
    conditionalPanel(
      'input.dataset === "Valor"',
      column(6, wellPanel(style = "height:180px;background:#40E0D0;",
                          h2("Fator Valor"),
                          br(),
                          h5(paste0("Atualização Diária"))),
             align="center"),
      column(6, wellPanel(style = "height:180px;background:#40E0D0;",
                          h3("Critérios Utilizados:"),
                          h5("CAGR LUCROS 5 ANOS > 0"),
                          h5("ROE > 0"),
                          h5("Score: P/VP")
                          
      ),
      align="left")
    ),
    conditionalPanel(
      'input.dataset === "Tamanho"',
      column(6, wellPanel(style = "height:180px;background:#40E0D0;",
                          h2("Fator Tamanho"),
                          br(),
                          h5(paste0("Atualização Diária"))),
             align="center"),
      column(6, wellPanel(style = "height:180px;background:#40E0D0;",
                          h3("Critérios Utilizados:"),
                          h5("CAGR LUCROS 5 ANOS > 0 e PEG > 0"),
                          h5("Rank1 = PEG"),
                          h5("Rank2 = CAGR RECEITAS 5 ANOS"),
                          h5("Score: Soma dos ranks")
                          ),
      align="left")
    ),
    conditionalPanel(
      'input.dataset === "Momentum"',
      column(6, wellPanel(style = "height:180px;background:#40E0D0;",
                          h2("Fator Momentum"),
                          br(),
                          h5(paste0("Atualização Mensal")),
                          h5(paste("Última Atualização: ", last_date))),
             align="center"),
      column(6, wellPanel(style = "height:180px;background:#40E0D0;",
                          h3("Critérios Utilizados:"),
                          h5("Seleciona as 60 empresas com melhor momentum"),
                          h5("Dentre as 60 seleciona as com maior percentual de dias positivos"),
                          h5("Ordena por Volatilidade")
                          ),
      align="left")
    )
  ),
  
  fluidRow(
    tabsetPanel(
      id = 'dataset',
      tabPanel("Dividendos", DT::dataTableOutput("mytable1", width = "70%"),align="center"),
      tabPanel("Qualidade", DT::dataTableOutput("mytable2", width = "70%"),align="center"),
      tabPanel("Valor", DT::dataTableOutput("mytable3", width = "70%"),align="center"),
      tabPanel("Tamanho", DT::dataTableOutput("mytable4", width = "70%"),align="center"),
      tabPanel("Momentum", DT::dataTableOutput("mytable5", width = "70%"),align="center")
    )
  )
)

#--------------------------------------------------------------------------

server <- function(input, output, session){
  
  # choose columns to display
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(Dividendos,
                  extensions = c('FixedColumns',"Buttons"), 
                  options = list(dom = 'lftBpr', 
                                # scrollX = TRUE, 
                                 #paging=FALSE,
                                 #fixedHeader=FALSE,
                                 #fixedColumns = list(leftColumns = 2, rightColumns = 0),
                                 lengthMenu = c(5, 10, 15, 20), 
                                 pageLength = 10,
                                 buttons = c('copy', 'csv', 'excel', 'pdf')),
                  rownames =  FALSE
    )%>%
      DT::formatPercentage(c("Volatilidade", "DY"), 2) %>%
      DT::formatRound(c("Sharpe"), 2) %>%
      DT::formatCurrency(c("Preço"),currency = "R$ ", digits = 2)
    
  })
  
  # sorted columns are colored now because CSS are attached to them
  output$mytable2 <- DT::renderDataTable({
    DT::datatable(Quality, extensions = c('FixedColumns',"Buttons"), 
                  options = list(dom = 'lftBpr',
                                 lengthMenu = c(5, 10, 15, 20), 
                                 pageLength = 10,
                                 buttons = c('copy', 'csv', 'excel', 'pdf')),
                  rownames =  FALSE
    )%>%
      DT::formatPercentage(c("Volatilidade", "ROE", "CAGR LUCROS 5 ANOS"), 2) %>%
      DT::formatRound(c("Sharpe", "DIV. LIQ. / PATRI."), 2) %>%
      DT::formatCurrency(c("Preço"),currency = "R$ ", digits = 2)
  })
  
  # customize the length drop-down menu; display 5 rows per page by default
  output$mytable3 <- DT::renderDataTable({
    DT::datatable(Valor, extensions = c("Buttons"), 
                  options = list(dom = 'lftBpr', 
                                 lengthMenu = c(5, 10, 15, 20), 
                                 pageLength = 10,
                                 buttons = c('copy', 'csv', 'excel', 'pdf')),
                  rownames =  FALSE
    )%>%
      DT::formatPercentage(c("Volatilidade"), 2) %>%
      DT::formatRound(c("Sharpe","P/VP"), 2) %>%
      DT::formatCurrency(c("Preço"),currency = "R$ ", digits = 2)
  })
  
  # customize the length drop-down menu; display 5 rows per page by default
  output$mytable4 <- DT::renderDataTable({
    DT::datatable(Size, 
                  extensions = c("Buttons"), 
                  options = list(dom = 'lftBpr',
                                 lengthMenu = c(5, 10, 15, 20), 
                                 pageLength = 10,
                                 buttons = c('copy', 'csv', 'excel', 'pdf')),
                  rownames =  FALSE
    )%>%
      DT::formatPercentage(c("Volatilidade", "PEG Ratio","CAGR RECEITAS 5 ANOS"), 2) %>%
      DT::formatRound(c("Sharpe"), 2) %>%
      DT::formatCurrency(c("Preço", "VALOR DE MERCADO"),currency = "R$ ", digits = 2)
  })
  
  # customize the length drop-down menu; display 5 rows per page by default
  output$mytable5 <- DT::renderDataTable({
    DT::datatable(momentum,
                  #caption = paste("Última atualização: ", last_date),
                  options = list(lengthMenu = c(10, 15, 20),
                                 pageLength = 10,
                                 dom = 'lftBpr',
                                 buttons = c('copy', 'csv', 'excel', 'pdf')),
                  rownames =  FALSE,
                  extensions = "Buttons"
    )%>%
      DT::formatPercentage(c("% de Dias Positivos", "Rendimento no Período", "Volatilidade"), 2) %>%
      DT::formatRound(c("Sharpe"), 2) %>%
      DT::formatCurrency(c("Preço"),currency = "R$ ", digits = 2)
  })
 
}

shinyApp(ui, server)
