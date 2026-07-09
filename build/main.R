# load_data
if (!require(tidyquant))
  install.packages('tidyquant', dependencies = T)
if (!require(finreportr))
  install.packages('finreportr', dependencies = T)
if (!require(finreportr))
  install.packages('quantmod', dependencies = T)
if (!require(purrr))
  install.packages('purrr', dependencies = T)
if (!require(dplyr))
  install.packages('dplyr', dependencies = T)
if (!require(yfinance))
  remotes::install_github("ljupch0/yfinance")
if (!require(tidyr))
  install.packages('tidyr')

# source(here::here("src","1_load_data.R"))
indexes <-
  c("VFIAX",
    "VSMAX",
    "VTMGX",
    "VEMAX",
    "VBTLX",
    "VWEAX",
    "VTABX",
    "VUSXX",
    "VGSLX",
    "IAU")

tibble(symbol = indexes) %>%
  dplyr::mutate(daily_data = purrr::map2(.x = symbol, .y = "1960-01-01",
                                         ~ {
                                           tq_get(.x, get = "stock.prices", from = .y)
                                         })) -> index_data

index_data %>%
  dplyr::select(daily_data) %>%
  tidyr::unnest(daily_data) %>%
  group_by(symbol) %>%
  tq_transmute(
    select     = adjusted,
    mutate_fun = periodReturn,
    period     = "yearly",
    type       = "arithmetic"
  )#%>%
dplyr::mutate(date = lubridate::year(date))#%>%tidyr::spread(symbol,yearly.returns)


fred_data <- tibble(symbol = "FPCPITOTLZGUSA") %>%
  dplyr::mutate(annual_data = purrr::map(.x = symbol, .f = ~ {
    tq_get(x = .x, get = "economic.data", from = "1960-01-01") %>%
      dplyr::mutate(price = price / 100)
  })) %>% dplyr::select(annual_data)

fred_data %>%
  tidyr::unnest(annual_data, names_repair = "universal") %>%
  dplyr::mutate(date = year(date)) %>%
  inner_join(
    index_data %>%
      dplyr::select(daily_data) %>%
      tidyr::unnest(daily_data) %>%
      group_by(symbol) %>%
      tq_transmute(
        select     = adjusted,
        mutate_fun = periodReturn,
        period     = "yearly",
        type       = "arithmetic"
      ) %>%
      dplyr::mutate(date = lubridate::year(date)),
    by = 'date'
  ) %>%
  dplyr::mutate(adjusted_returns = (1 + yearly.returns) / (1 + price) -
                  1) %>%
  dplyr::select(-symbol.x, -yearly.returns) %>% tidyr::spread(symbol.y, adjusted_returns)
print(n = 100)
