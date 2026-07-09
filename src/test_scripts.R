# tform_data
s500_is %>%
  tidyr::unnest(reports) %>%
  dplyr::mutate(
    year = ifelse(day(date) <= 10 & month(date) == 1, year(date) - 1, year(date)),
    interestExpense = ifelse(is.na(interestExpense), 0, interestExpense),
    is_zombie = (netIncome - abs(interestExpense)) <= 0
  ) %>% dplyr::group_by(ticker) %>%
  dplyr::mutate(min_date = min(date, na.rm = T),
                max_date = max(date, na.rm = T)) %>%
  # dplyr::filter(year(date) == 2019, is_zombie) %>%
  dplyr::select(
    ticker,
    weight,
    sector,
    date,
    min_date,
    max_date,
    year,
    totalRevenue,
    interestExpense,
    netIncome,
    is_zombie
  ) %>%
  group_by(year, is_zombie) %>%
  dplyr::mutate(zy_wts = weight / sum(weight)) %>%
  ungroup(.) -> s500_zombie_years

# s500_zombie_years%>%dplyr::filter(ticker %in% c('LHX','LDOS','TRMB'))%>%
  # dplyr::select(ticker,date,year)
# tform_data
s600_is %>%
  tidyr::unnest(reports) %>%
  dplyr::mutate(
    year = year(date),
    interestExpense = ifelse(is.na(interestExpense), 0, interestExpense),
    is_zombie = (netIncome - abs(interestExpense)) <= 0
  ) %>% dplyr::group_by(ticker) %>%
  dplyr::mutate(min_date = min(date, na.rm = T),
                max_date = max(date, na.rm = T)) %>%
  # dplyr::filter(year(date) == 2019, is_zombie) %>%
  dplyr::select(
    ticker,
    weight,
    sector,
    date,
    min_date,
    max_date,
    year,
    totalRevenue,
    interestExpense,
    netIncome,
    is_zombie
  ) %>%
  group_by(year, is_zombie) %>%
  dplyr::mutate(zy_wts = weight / sum(weight)) %>%
  ungroup(.) -> s600_zombie_years



s500_is %>%
  dplyr::select(daily_data) %>%
  tidyr::unnest(daily_data) %>%
  group_by(symbol) %>%
  tq_transmute(
    select     = adjusted,
    mutate_fun = periodReturn,
    period     = "monthly",
    col_rename = "Ra"
  )%>%unique(.) -> s500_stocks_monthly_returns


s500_2019_zombies_portfolio <- (
  s500_zombie_years %>%
    dplyr::filter(year == 2019, is_zombie) %>%
    dplyr::select(symbol = ticker, weights = zy_wts) -> s500_19_wts
) %>%
  inner_join(s500_stocks_monthly_returns, by = "symbol") %>% 
  dplyr::filter(date>="2020-01-01")%>%
  tq_portfolio(assets_col = symbol, returns_col = Ra,weights = s500_19_wts,col_rename= "Ra_w_weights")
s500_2019_nzombies_portfolio <- (
  s500_zombie_years %>%
    dplyr::filter(year == 2019, !is_zombie) %>%
    dplyr::select(symbol = ticker, weights = zy_wts) -> s500_19_wts
) %>%
  inner_join(s500_stocks_monthly_returns, by = "symbol") %>% 
  dplyr::filter(date>="2020-01-01")%>%
  tq_portfolio(assets_col = symbol, returns_col = Ra,weights = s500_19_wts,col_rename= "Ra_w_weights")

s500_2020_zombies_portfolio <- (
  s500_zombie_years %>%
    dplyr::filter(year == 2020, is_zombie) %>%
    dplyr::select(symbol = ticker, weights = zy_wts) -> s500_19_wts
) %>%
  inner_join(s500_stocks_monthly_returns, by = "symbol") %>% 
  dplyr::filter(date>="2021-01-01")%>%
  tq_portfolio(assets_col = symbol, returns_col = Ra,weights = s500_19_wts,col_rename= "Ra_w_weights")
s500_2020_nzombies_portfolio <- (
  s500_zombie_years %>%
    dplyr::filter(year == 2020, !is_zombie) %>%
    dplyr::select(symbol = ticker, weights = zy_wts) -> s500_19_wts
) %>%
  inner_join(s500_stocks_monthly_returns, by = "symbol") %>% 
  dplyr::filter(date>="2021-01-01")%>%
  tq_portfolio(assets_col = symbol, returns_col = Ra,weights = s500_19_wts,col_rename= "Ra_w_weights")
s500_2021_zombies_portfolio <- (
  s500_zombie_years %>%
    dplyr::filter(year == 2021, is_zombie) %>%
    dplyr::select(symbol = ticker, weights = zy_wts) -> s500_19_wts
) %>%
  inner_join(s500_stocks_monthly_returns, by = "symbol") %>% 
  dplyr::filter(date>="2022-01-01")%>%
  tq_portfolio(assets_col = symbol, returns_col = Ra,weights = s500_19_wts,col_rename= "Ra_w_weights")
s500_2021_nzombies_portfolio <- (
  s500_zombie_years %>%
    dplyr::filter(year == 2021, !is_zombie) %>%
    dplyr::select(symbol = ticker, weights = zy_wts)%>%unique(.) -> s500_19_wts
) %>%
  inner_join(s500_stocks_monthly_returns, by = "symbol") %>% 
  dplyr::filter(date>="2022-01-01")%>%
  tq_portfolio(assets_col = symbol, returns_col = Ra,weights = s500_19_wts,col_rename= "Ra_w_weights")
s500_2022_zombies_portfolio <- (
  s500_zombie_years %>%
    dplyr::filter(year == 2022, is_zombie) %>%
    dplyr::select(symbol = ticker, weights = zy_wts) -> s500_19_wts
) %>%
  inner_join(s500_stocks_monthly_returns, by = "symbol") %>% 
  dplyr::filter(date>="2023-01-01")%>%
  tq_portfolio(assets_col = symbol, returns_col = Ra,weights = s500_19_wts,col_rename= "Ra_w_weights")
s500_2022_nzombies_portfolio <- (
  s500_zombie_years %>%
    dplyr::filter(year == 2022, !is_zombie) %>%
    dplyr::select(symbol = ticker, weights = zy_wts)%>%unique(.) -> s500_19_wts
) %>%
  inner_join(s500_stocks_monthly_returns, by = "symbol") %>% 
  dplyr::filter(date>="2023-01-01")%>%
  tq_portfolio(assets_col = symbol, returns_col = Ra,weights = s500_19_wts,col_rename= "Ra_w_weights")

Rb <- "SPY" %>%
  tq_get(get  = "stock.prices",
         from = min(s500_2019_zombies_portfolio$date)) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Rb")


s500_2019_zombies_portfolio%>%
  left_join(Rb,by = "date")%>%
  tq_performance(Ra = Ra_w_weights, Rb = Rb, performance_fun = table.CAPM)
s500_2019_zombies_portfolio%>%
  left_join(Rb,by = "date")%>%
  tq_performance(Ra = Ra_w_weights, Rb = NULL, performance_fun = table.Stats)
s500_2019_zombies_portfolio%>%
  left_join(Rb,by = "date")%>%
  tq_performance(Ra = Ra_w_weights, Rb = NULL, performance_fun = table.AnnualizedReturns)
s500_2019_nzombies_portfolio%>%
  left_join(Rb,by = "date")%>%
  tq_performance(Ra = Ra_w_weights, Rb = NULL, performance_fun = table.AnnualizedReturns)
s500_2020_zombies_portfolio%>%
  left_join(Rb,by = "date")%>%
  tq_performance(Ra = Ra_w_weights, Rb = Rb, performance_fun = table.CAPM)
s500_2020_zombies_portfolio%>%
  left_join(Rb,by = "date")%>%
  tq_performance(Ra = Ra_w_weights, Rb = NULL, performance_fun = table.AnnualizedReturns)
s500_2020_nzombies_portfolio%>%
  left_join(Rb,by = "date")%>%
  tq_performance(Ra = Ra_w_weights, Rb = NULL, performance_fun = table.AnnualizedReturns)
s500_2021_zombies_portfolio%>%
  left_join(Rb,by = "date")%>%
  tq_performance(Ra = Ra_w_weights, Rb = Rb, performance_fun = table.CAPM)
s500_2021_zombies_portfolio%>%
  left_join(Rb,by = "date")%>%
  tq_performance(Ra = Ra_w_weights, Rb = NULL, performance_fun = table.AnnualizedReturns)
s500_2021_nzombies_portfolio%>%
  left_join(Rb,by = "date")%>%
  tq_performance(Ra = Ra_w_weights, Rb = NULL, performance_fun = table.AnnualizedReturns)


s500_2022_zombies_portfolio%>%
  left_join(Rb,by = "date")%>%
  tq_performance(Ra = Ra_w_weights, Rb = Rb, performance_fun = table.CAPM)
s500_2022_zombies_portfolio%>%
  left_join(Rb,by = "date")%>%
  tq_performance(Ra = Ra_w_weights, Rb = NULL, performance_fun = table.AnnualizedReturns)
s500_2022_nzombies_portfolio%>%
  left_join(Rb,by = "date")%>%
  tq_performance(Ra = Ra_w_weights, Rb = NULL, performance_fun = table.AnnualizedReturns)


