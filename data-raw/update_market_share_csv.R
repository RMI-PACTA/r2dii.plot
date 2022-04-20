library(dplyr)

out <- r2dii.data::loanbook_demo %>%
  r2dii.match::match_name(r2dii.data::ald_demo) %>%
  r2dii.match::prioritize() %>%
  r2dii.analysis::target_market_share(
    r2dii.data::ald_demo,
    r2dii.data::scenario_demo_2020,
    r2dii.data::region_isos_demo) %>%
  filter(
    .data$region == "global"
  )

readr::write_csv(out, here::here("data-raw", "market_share.csv"))
