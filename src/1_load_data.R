
#SP500 Large Cap Index
s500 <- tq_index("SP500")

start <- Sys.time()
s500 %>%
  dplyr::mutate(
    #Income Statement to a data frame
    reports = purrr::map(symbol, yfinance::get_income),
    #Gather a stock summary from yahoo
    summary = purrr::map(symbol, yfinance::get_summaries),
    sector = purrr::map_if(
      .x = summary,
      .f = ~ .x$sector,
      .p = ~ length(.x$sector) > 0,
      .else = "-"
    ),
    #Minimum date from the data
    min_date = purrr::map_chr(reports,  ~ min(.x$date)),
    #All of the daily data for each of the s500 companies for the time frame up to date
    daily_data = purrr::map2(.x = symbol, .y = min_date,
                             ~ {
                               tq_get(.x, get = "stock.prices", from = .y)
                             })
  )  -> s500_is
end <- Sys.time()
end - start
#Save the data for faster access and reference
saveRDS(s500_is, file = here::here("data", paste0(
  "s500_is_", format(Sys.Date(), "%Y%m%d"), ".RDS"
)))

require(rvest)
"AAPL" -> sample_stock
yfinance::get_summaries(sample_stock)
# sample_link<-paste0("https://finance.yahoo.com/quote/",sample_stock,"/profile?p=",sample_stock)
# read_html(sample_link)%>%html_nodes('')
#SP 600 Index of small caps
s600 <- tq_index("SP600")
start <- Sys.time()
s600 %>%
  dplyr::mutate(
    #Income Statement to a data frame
    reports = purrr::map(symbol, yfinance::get_income, report_type = 'annual'),
    #Gather a stock summary from yahoo
    summary = purrr::map(symbol, yfinance::get_summaries),
    sector = purrr::map_if(
      .x = summary,
      .f = ~ .x$sector,
      .p = ~ length(.x$sector) > 0,
      .else = "-"
    ),
    #Minimum date from the data
    min_date = purrr::map_chr(reports,  ~ min(.x$date)),
    #All of the daily data for each of the s500 companies for the time frame up to date
    daily_data = purrr::map2(.x = symbol, .y = min_date,
                             ~ {
                               tq_get(.x, get = "stock.prices", from = .y)
                             })
  ) -> s600_is
end <- Sys.time()
end - start

saveRDS(s600_is, file = here::here("data", paste0(
  "s600_is_", format(Sys.Date(), "%Y%m%d"), ".RDS"
)))
