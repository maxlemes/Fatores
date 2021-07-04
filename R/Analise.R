rm(list=ls())

library(tidyverse)
library(tidyquant)

# para ver as funções disponíves do tidyquant
tq_performance_fun_options()


#período analisado
data_ini <- "2015-01-01"
data_fim <- "2021-12-31"

# Ra armazena os dados dos ativos
Ra <- c("PETR4.SA", "WEGE3.SA", "ABEV3.SA") %>%
  tidyquant::tq_get(get  = "stock.prices",
         from = data_ini,
         to   = data_fim) %>%
  dplyr::group_by(symbol) %>%
  tidyquant::tq_transmute(select = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Ra")

# Rb armazena os dados do Benchmark
Rb <- "^BVSP" %>%
  tq_get(get  = "stock.prices",
         from = data_ini,
         to   = data_fim) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Rb")

#juntando os dados
RaRb <- left_join(Ra, Rb, by = c("date" = "date"))

#Calculando o CAPM
RaRb_capm <- RaRb %>%
  tq_performance(Ra = Ra, 
                 Rb = Rb, 
                 performance_fun = table.CAPM)

RaRb_capm %>% select(symbol, Alpha, Beta)

#customizando o SharpeRatio
args(SharpeRatio)

#Calculando o Sharpe
Ra_sharpe <- Ra %>%
  tq_performance(
    Ra = Ra, 
    Rb = NULL, 
    performance_fun = SharpeRatio, 
    Rf = 0.0425 / 12, 
    p  = 0.99
  )

# Analisando um portfolio

# Ra armazena os dados dos ativos
stock_returns_monthly <- c("PETR4.SA", "WEGE3.SA", "ABEV3.SA") %>%
  tidyquant::tq_get(get  = "stock.prices",
                    from = data_ini,
                    to   = data_fim) %>%
  dplyr::group_by(symbol) %>%
  tidyquant::tq_transmute(select = adjusted, 
                          mutate_fun = periodReturn, 
                          period     = "monthly", 
                          col_rename = "Ra")

# Rb armazena os dados do Benchmark
baseline_returns_monthly <- "^BVSP" %>%
  tq_get(get  = "stock.prices",
         from = data_ini,
         to   = data_fim) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Rb")

# Definindo os pesos dos ativos
wts <- c(0.5, 0.0, 0.5)

#Construindo o portfolio
portfolio_returns_monthly <- stock_returns_monthly %>%
  tq_portfolio(assets_col  = symbol, 
               returns_col = Ra, 
               weights     = wts, 
               col_rename  = "Ra")

#Outra maneira de construir o portfolio
#Neste método se o simbolo nao aparece seu peso é ZERO
wts_map <- tibble(
  symbols = c("PETR4.SA", "WEGE3.SA"),
  weights = c(0.5, 0.5)
)

stock_returns_monthly %>%
  tq_portfolio(assets_col  = symbol, 
               returns_col = Ra, 
               weights     = wts_map, 
               col_rename  = "Ra_using_wts_map")

#Mescalando o portfolio com o benchmark
RaRb_single_portfolio <- left_join(portfolio_returns_monthly, 
                                   baseline_returns_monthly,
                                   by = "date")
#Calculando o CAPM do portfolio
RaRb_single_portfolio %>%
  tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.CAPM)

#Calculando o Sharpe
RaRb_single_portfolio %>%
  tq_performance(
    Ra = Ra, 
    Rb = NULL, 
    performance_fun = SharpeRatio, 
    Rf = 0.0425 / 12, 
    p  = 0.99
  )

# Multiplos Portfolios
#Primeiramente repetimos as colunas dos retornos dos ativos
stock_returns_monthly_multi <- stock_returns_monthly %>%
  tq_repeat_df(n = 3) # n=3 é o numero de portifolios desejados
stock_returns_monthly_multi

#definindo os pesos
weights <- c(
  0.50, 0.25, 0.25,
  0.25, 0.50, 0.25,
  0.25, 0.25, 0.50
)

# os ativos
stocks <- c("PETR4.SA", "WEGE3.SA", "ABEV3.SA")

# e a tabela dos pesos
weights_table <-  tibble(stocks) %>%
  tq_repeat_df(n = 3) %>%
  bind_cols(tibble(weights)) %>%
  group_by(portfolio)

weights_table

# agora os n portofolios
portfolio_returns_monthly_multi <- stock_returns_monthly_multi %>%
  tq_portfolio(assets_col  = symbol, 
               returns_col = Ra, 
               weights     = weights_table, 
               col_rename  = "Ra")

portfolio_returns_monthly_multi

# mesclando e avaliando o desempenho
RaRb_multiple_portfolio <- left_join(portfolio_returns_monthly_multi, 
                                     baseline_returns_monthly,
                                     by = "date")

# CAPM
RaRb_multiple_portfolio %>%
  tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.CAPM)

# Sharpe Ratio
RaRb_multiple_portfolio %>%
  tq_performance(Ra = Ra, Rb = NULL, performance_fun = SharpeRatio)

# dados estatísticos
RaRb_multiple_portfolio %>%
  tq_performance(Ra = Ra, Rb = NULL, performance_fun = table.Stats)

# retornos anualizados
RaRb_multiple_portfolio %>%
  tq_performance(Ra = Ra, Rb = NULL, performance_fun = table.AnnualizedReturns)

# Correlação
RaRb_multiple_portfolio %>%
  tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.Correlation)

# Downsize risk
RaRb_multiple_portfolio %>%
  tq_performance(Ra = Ra, Rb = NULL, performance_fun = table.DownsideRisk)

# Downsize risk ratio
RaRb_multiple_portfolio %>%
  tq_performance(Ra = Ra, Rb = NULL, performance_fun = table.DownsideRiskRatio)

# Higher moments
RaRb_multiple_portfolio %>%
  tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.HigherMoments)

# informationRatio
RaRb_multiple_portfolio %>%
  tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.InformationRatio)

# Variabilidade
RaRb_multiple_portfolio %>%
  tq_performance(Ra = Ra, Rb = NULL, performance_fun = table.Variability)

# VaR
RaRb_multiple_portfolio %>%
  tq_performance(Ra = Ra, Rb = NULL, performance_fun = VaR)


# GRaficos
# Considere o portfolio unico
wts <- c(0.5, 0.0, 0.5)
portfolio_returns_monthly <- stock_returns_monthly %>%
  tq_portfolio(assets_col  = symbol, 
               returns_col = Ra, 
               weights     = wts, 
               col_rename  = "Ra")

#gráfico dos retornos mensais
portfolio_returns_monthly %>%
  ggplot(aes(x = date, y = Ra)) +
  geom_bar(stat = "identity", fill = palette_light()[[1]]) +
  labs(title = "Portfolio Returns",
       subtitle = "50% PETR4, 0% WEGE3, and 50% ABEV3",
       caption = "Shows an above-zero trend meaning positive returns",
       x = "", y = "Monthly Returns") +
  geom_smooth(method = "lm") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::percent)


wts <- c(0.5, 0, 0.5)
portfolio_growth_monthly <- stock_returns_monthly %>%
  tq_portfolio(assets_col   = symbol, 
               returns_col  = Ra, 
               weights      = wts, 
               col_rename   = "investment.growth",
               wealth.index = TRUE) %>%
  mutate(investment.growth = investment.growth * 10000)

#gráfico de performance
portfolio_growth_monthly %>%
  ggplot(aes(x = date, y = investment.growth)) +
  geom_line(size = 2, color = palette_light()[[1]]) +
  labs(title = "Portfolio Growth",
       subtitle = "50% PETR4, 0% WEGE3, and 50% ABEV3",
       caption = "Now we can really visualize performance!",
       x = "", y = "Portfolio Value") +
  geom_smooth(method = "loess") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar)

# Agora para multiplos portfolios

portfolio_growth_monthly_multi <- stock_returns_monthly_multi %>%
  tq_portfolio(assets_col   = symbol, 
               returns_col  = Ra, 
               weights      = weights_table, 
               col_rename   = "investment.growth",
               wealth.index = TRUE) %>%
  mutate(investment.growth = investment.growth * 10000)

portfolio_growth_monthly_multi %>%
  ggplot(aes(x = date, y = investment.growth, color = factor(portfolio))) +
  geom_line(size = 2) +
  labs(title = "Portfolio Growth",
       subtitle = "Comparing Multiple Portfolios",
       caption = "Portfolio 3 is a Standout!",
       x = "", y = "Portfolio Value",
       color = "Portfolio") +
  geom_smooth(method = "loess") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar)
