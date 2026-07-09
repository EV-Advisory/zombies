# tform_data
s500_is %>%
  tidyr::unnest(reports) %>%
  tidyr::unnest(sector,keep_empty = T) %>%
  dplyr::mutate(
    interestExpense = ifelse(is.na(interestExpense), 0, interestExpense),
    is_zombie = (netIncome - abs(interestExpense)) <= 0
  ) %>% dplyr::group_by(ticker) %>%
  dplyr::mutate(min_date = min(date, na.rm = T),
                max_date = max(date, na.rm = T)) %>%
  # dplyr::filter(year(date) == 2019, is_zombie) %>%
  dplyr::select(
    ticker,
    sector,
    date,
    min_date,
    max_date,
    totalRevenue,
    interestExpense,
    netIncome,
    is_zombie
  ) ->s500_z_reports

# tform_data
s600_is %>%
  tidyr::unnest(reports) %>%
  tidyr::unnest(sector,keep_empty = T)  %>%
  dplyr::mutate(
    interestExpense = ifelse(is.na(interestExpense), 0, interestExpense),
    is_zombie = (netIncome - abs(interestExpense)) <= 0
  ) %>% dplyr::group_by(ticker) %>%
  dplyr::mutate(min_date = min(date, na.rm = T),
                max_date = max(date, na.rm = T)) %>%
  # dplyr::filter(year(date) == 2019, is_zombie) %>%
  dplyr::select(
    ticker,
    sector,
    date,
    min_date,
    max_date,
    totalRevenue,
    interestExpense,
    netIncome,
    is_zombie
  )->s600_z_reports
