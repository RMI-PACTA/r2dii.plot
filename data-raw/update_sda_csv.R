library(dplyr)

out <- r2dii.match::match_name(
  r2dii.data::loanbook_demo,
  r2dii.data::ald_demo) %>%
    r2dii.match::prioritize() %>%
    r2dii.analysis::target_sda(
      ald = r2dii.data::ald_demo,
      co2_intensity_scenario = r2dii.data::co2_intensity_scenario_demo,
      region_isos = r2dii.data::region_isos_demo)

readr::write_csv(out, here::here("data-raw", "sda.csv"))
