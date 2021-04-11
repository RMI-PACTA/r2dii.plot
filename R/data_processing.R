#' Prepares PACTA total_portfolio data for meta-report security type bar chart
#'
#' This function aggregates and prepares one of PACTA analysis result files
#' ".._total_portfolio.rda" from "30_Processed_Inputs" folder to form an input
#' that can be used for plotting meta-report security type coverage per investor
#' type bar chart.
#'
#' @param data_total_portfolio Dataframe in the shape of
#'   ".._total_portfolio.rda" dataset from PACTA analysis output in
#'   "30_Processed_Inputs" folder (dataframe).
#' @param other_asset_types Array of character strings that should be summed up
#'   as "Other" asset type (array of character strings).
#'
#' @export

prepare_for_metareport_security_type_chart <- function(data_total_portfolio,
                                                       other_asset_types = c(
                                                         "Funds", "Others",
                                                         "Unclassifiable"
                                                       )) {
  data_aggregated_all <- data_total_portfolio %>%
    select(.data$investor_name, .data$asset_type, .data$value_usd) %>%
    group_by(.data$investor_name, .data$asset_type) %>%
    summarise(total_value = sum(.data$value_usd, na.rm = T)) %>%
    ungroup() %>%
    group_by(.data$investor_name) %>%
    mutate(total_peergroup = sum(.data$total_value, na.rm = T)) %>%
    ungroup() %>%
    mutate(share = .data$total_value / .data$total_peergroup) %>%
    select(.data$investor_name, .data$asset_type, .data$share)

  data_aggregated_in_analysis <- data_aggregated_all %>%
    filter(!(.data$asset_type %in% other_asset_types))

  data_aggregated_other <- data_aggregated_all %>%
    filter(.data$asset_type %in% other_asset_types) %>%
    group_by(.data$investor_name) %>%
    summarise(share = sum(.data$share)) %>%
    mutate(asset_type = "Others")

  data_aggregated <- rbind(data_aggregated_in_analysis, data_aggregated_other) %>%
    arrange(.data$investor_name)
}

#' Prepares PACTA overview data for meta-report PACTA sectors bar chart
#'
#' @param data_overview Dataframe in the shape of ".._overview_portfolio.rda"
#'   dataset from PACTA analysis output in "30_Processed_Inputs" folder
#'   (dataframe).
#'
#' @return Dataframe prepared for the plot.
#' @export
#'
#' @examples
#'
#' # TODO
prepare_for_pacta_sectors_chart <- function(data_overview) {
  climate_relevant <- data_overview %>%
    filter(.data$financial_sector != "Other" & .data$valid_input == TRUE) %>%
    group_by(.data$investor_name, .data$portfolio_name, .data$asset_type) %>%
    summarise(
      climate_sum = sum(.data$valid_value_usd),
      total = mean(.data$asset_value_usd)
    ) %>%
    ungroup() %>%
    group_by(.data$investor_name, .data$asset_type) %>%
    summarise(
      climate_value = sum(.data$climate_sum),
      total_value = sum(.data$total)
    ) %>%
    ungroup() %>%
    filter((.data$asset_type %in% c("Bonds", "Equity")) &
      .data$investor_name != "Meta Investor") %>%
    mutate(share_climate_relevant = .data$climate_value / .data$total_value) %>%
    select(.data$investor_name, .data$asset_type, .data$share_climate_relevant)
}


#' Prepares PACTA overview data for meta-report PACTA sectors mix chart
#'
#' @param data_overview Dataframe in the shape of ".._overview_portfolio.rda"
#'   data set from PACTA analysis output in "30_Processed_Inputs" folder
#'   (dataframe).
#'
#' @return Dataframe prepared for the plot.
#' @export
#'
#' @examples
#' # TODO
prepare_for_metareport_pacta_sectors_mix_chart <- function(data_overview) {
  data_sectors_mix <- data_overview %>%
    filter(.data$financial_sector != "Other" &
      .data$valid_input == TRUE &
      .data$asset_type %in% c("Equity", "Bonds")) %>%
    group_by(.data$investor_name, .data$asset_type) %>%
    mutate(total_climate_value_usd = sum(.data$valid_value_usd, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(.data$investor_name, .data$asset_type, .data$financial_sector) %>%
    summarise(
      technology_value = sum(.data$valid_value_usd, na.rm = TRUE),
      total_climate_value_usd = .data$total_climate_value_usd
    ) %>%
    ungroup() %>%
    mutate(share = .data$technology_value / .data$total_climate_value_usd) %>%
    distinct() %>%
    select(.data$investor_name, .data$asset_type,
      sector = .data$financial_sector, .data$share
    )
}

#' Prepares results data per asset type for distribution chart plot
#'
#' @param data_asset_type Dataframe in the shape of
#'   "Equity/Bonds_results_portfolio.rda" dataset from PACTA analysis output in
#'   "30_Results" folder (dataframe).
#' @param sectors_filter Sector to be used for filtering (character string or a
#'   vector of character strings).
#' @param technologies_filter Technologies to be used for filtering (character
#'   string or a vector of character strings).
#' @param year_filter Year to be used for filtering (integer).
#' @param value_to_plot Variable to be used as value for plotting (character
#'   string).
#'
#' @return Dataframe with columns investor_name, portfolio_name, value.
#' @export
#'
#' @examples
#' # TODO
prepare_for_metareport_distribution_chart <- function(data_asset_type,
                                                      sectors_filter,
                                                      technologies_filter,
                                                      year_filter,
                                                      value_to_plot =
                                                        "plan_carsten") {
  unique_investor_names <- data_asset_type %>%
    filter(.data$investor_name != "Meta Investor") %>%
    select(.data$investor_name, .data$portfolio_name) %>%
    distinct()

  data_distribution <- data_asset_type %>%
    filter(
      .data$ald_sector %in% sectors_filter,
      .data$technology %in% technologies_filter,
      .data$scenario == data_asset_type$scenario[1], # this choice is only made to extract a distinct set of data
      .data$year == year_filter,
      .data$allocation == "portfolio_weight",
      .data$investor_name != "Meta Investor",
      .data$scenario_geography ==
        data_asset_type$scenario_geography[1] # this choice is only made to extract a distinct set of data
    ) %>%
    select(.data$investor_name, .data$portfolio_name, value = !!value_to_plot) %>%
    group_by(.data$investor_name, .data$portfolio_name) %>%
    summarise(value = sum(.data$value, na.rm = TRUE)) %>%
    ungroup() %>%
    right_join(unique_investor_names) %>%
    mutate(
      value =
        if_else(is.na(.data$value), 0.001, .data$value)
    ) %>%
    arrange(desc(.data$value))
}

#' Prepares results data per asset type for bubble chart
#'
#' This function aggregates and prepares one of PACTA analysis result file
#' "Bonds/Equity_results_portfolio.rda" from "40_Results" folder to form an
#' input that can be used for plotting meta-report bubble chart showing a
#' planned technology build-out as percentage of build-out required by a
#' scenario against the current technology share.
#'
#' @param data_asset_type Dataframe in the shape of
#'   "Equity/Bonds_results_portfolio.rda" dataset from PACTA analysis output in
#'   "40_Results" folder (dataframe).
#' @param asset_type Asset type of the data (character string).
#' @param start_year Year to be used as a start year in difference calculations
#'   (integer).
#' @param technologies_filter Technologies to be used for filtering (character
#'   string or a vector of character strings).
#' @param scenario_filter Scenario to be used in the calculations (character
#'   string).
#' @param scenario_geography_filter Scenario geography to be used in the
#'   calculations (character string)
#'
#' @return Dataframe with columns investor_name, portfolio_name, value_x,
#'   value_y.
#' @export
#'
#' @examples
#' # TODO
prepare_for_metareport_bubble_chart <- function(data_asset_type,
                                                asset_type,
                                                start_year,
                                                technologies_filter,
                                                scenario_filter,
                                                scenario_geography_filter) {
  if (asset_type == "Equity") {
    allocation_choice <- "ownership_weight"
  } else {
    allocation_choice <- "portfolio_weight"
  }

  end_year <- start_year + 5

  data_bubble <- data_asset_type %>%
    filter(
      .data$technology %in% technologies_filter,
      .data$scenario == scenario_filter,
      .data$year == start_year | .data$year == end_year,
      .data$scenario_geography == scenario_geography_filter,
      .data$allocation == allocation_choice,
      .data$investor_name != "Meta Investor"
    ) %>%
    select(
      .data$investor_name,
      .data$portfolio_name,
      .data$year,
      .data$plan_tech_share,
      .data$plan_alloc_wt_tech_prod,
      .data$scen_alloc_wt_tech_prod
    ) %>%
    group_by(.data$investor_name, .data$portfolio_name) %>%
    arrange(.data$investor_name, .data$portfolio_name, .data$year,
      .by_group = TRUE
    ) %>%
    mutate(
      difference_port =
        lead(.data$plan_alloc_wt_tech_prod) - .data$plan_alloc_wt_tech_prod,
      difference_scen =
        lead(.data$scen_alloc_wt_tech_prod) - .data$scen_alloc_wt_tech_prod
    ) %>%
    ungroup() %>%
    filter(.data$year == start_year) %>%
    mutate(share_build_out = .data$difference_port / .data$difference_scen) %>%
    mutate(share_build_out = if_else(.data$share_build_out > 1.0,
      1.0,
      .data$share_build_out
    )) %>%
    select(.data$investor_name,
      .data$portfolio_name,
      value_x = .data$plan_tech_share,
      value_y = .data$share_build_out
    )
}

#' Prepares results data per asset type for map plot
#'
#' @param data_map_asset_type Dataframe in the shape of
#'   "Equity/Bonds_results_map.rda" dataset from PACTA analysis output in
#'   "40_Results" folder (dataframe).
#' @param asset_type Asset type of the data used to specify default allocation
#'   method (character string).
#' @param technology_filter Technology to be used for filtering (character
#'   string).
#' @param year_filter Year to be used for filtering (integer).
#' @param value_divisor Number by which to divide the values to be plotted
#'   (integer).
#' @param allocation_method Allocation method used to calculate the production
#'   values. If none is specified "portfolio_weight" is used for Bonds and
#'   "ownership_weight" is used for Equity.
#'
#' @return Dataframe with columns (only important columns are listed): long,
#'   lat, group, value, unit, abbreviation_divisor.
#' @export
#'
#' @examples
#' # TODO
prepare_for_map_chart <- function(data_map_asset_type,
                                  asset_type,
                                  technology_filter,
                                  year_filter,
                                  value_divisor = 1,
                                  allocation_method = NULL) {
  if (is.null(allocation_method)) {
    if (asset_type == "Equity") {
      allocation_method <- "ownership_weight"
    } else {
      allocation_method <- "portfolio_weight"
    }
  }

  world_map <- map_data(map = "world") %>%
    mutate(iso2c = iso.alpha(world_map$region, n = 2))

  data_map <- data_map_asset_type %>%
    filter(
      .data$technology == technology_filter,
      .data$allocation == allocation_method,
      .data$year == year_filter,
      .data$equity_market == "Global"
    ) %>%
    group_by(.data$ald_location) %>%
    summarise(
      value = sum(.data$plan_alloc_wt_tech_prod, na.rm = TRUE) / value_divisor,
      unit = max(.data$ald_production_unit)
    ) %>%
    ungroup() %>%
    na.omit() %>%
    mutate(abbreviation_divisor = case_when(
      !!value_divisor == 1 ~ "",
      !!value_divisor == 10^3 ~ "k",
      !!value_divisor == 10^6 ~ "M",
      !!value_divisor == 10^9 ~ "B",
      TRUE ~ as.character(value_divisor)
    ))

  joined_map <- left_join(world_map, data_map, by = c("iso2c" = "ald_location"))

  joined_map
}
