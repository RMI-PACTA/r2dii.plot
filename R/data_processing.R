#' Performs the initial processing on raw input data in banks' format
#'
#' @param data Raw input data in the format of banks' output.
#'
#' @description This function processes the data in banks' format so that it can
#' be used later in data filtering functions for charts. 'metric_type' variable
#' is added which depends on 'metric' and the 'metric' values themselves are
#' edited for plotting purposes.
#'
#' @return A dataframe with additional column: metric type and modified metric
#' @export
#'
#' @examples
#' # TODO
process_input_data <- function(data) {
  data <- data %>%
    mutate(metric_type = case_when(
      .data$metric == "projected" ~ "portfolio",
      grepl("target", .data$metric) ~ "scenario",
      TRUE ~ "benchmark"
    )) %>%
    mutate(metric = if_else(
      grepl("target", .data$metric),
      sub(".*_", "", .data$metric),
      .data$metric
    ))
}

#' Prepares pre-processed data for plotting a trajectory chart
#'
#' @param data_preprocessed Pre-processed input data.
#' @param sector_filter Sector for which to filter the data (character string).
#' @param technology_filter Technology for which to filter the data (character
#'   string).
#' @param region_filter Region for which to filter the data (character string).
#' @param scenario_source_filter Scenario source for which to filter the data
#'   (character string).
#' @param value_name The name of the value to be plotted in the trajectory chart
#'   (character string).
#' @param end_year_filter Cut-off year for the chart (an integer).
#' @param normalize_to_start_year Flag indicating whether the values should be
#'   normalized (boolean).
#'
#' @export

prepare_for_trajectory_chart <- function(data_preprocessed,
                                         sector_filter,
                                         technology_filter,
                                         region_filter,
                                         scenario_source_filter,
                                         value_name,
                                         end_year_filter = 2025,
                                         normalize_to_start_year = TRUE) {
  data_filtered <- data_preprocessed %>%
    filter(.data$sector == !!sector_filter) %>%
    filter(.data$technology == !!technology_filter) %>%
    filter(.data$region == !!region_filter) %>%
    filter(.data$scenario_source == !!scenario_source_filter) %>%
    filter(.data$year <= end_year_filter) %>%
    select(.data$year, .data$metric_type, .data$metric, .data$technology,
      value = !!value_name
    )

  if (normalize_to_start_year) {
    data_filtered <- left_join(data_filtered,
      data_filtered[data_filtered$year == min(data_filtered$year), ],
      by = c("metric_type", "metric")
    ) %>%
      mutate(value = .data$value.x / .data$value.y) %>%
      select(
        year = .data$year.x, .data$metric_type,
        .data$metric, .data$value, technology = .data$technology.x
      )
  } else {
    data_filtered
  }
}

#' Prepares pre-processed data for plotting a tech-mix chart
#'
#' @param data_preprocessed Pre-processed input data.
#' @param sector_filter Sector for which to filter the data (character string).
#' @param years_filter Years to plot in the graph (array of integer values).
#' @param region_filter Region for which to filter the data (character string).
#' @param scenario_source_filter Scenario source for which to filter the data
#'   (character string).
#' @param scenario_filter Scenario to plot in the graph (character string).
#' @param value_name The name of the value to be plotted as a bar chart
#'   (character string).
#'
#' @export

prepare_for_techmix_chart <- function(data_preprocessed,
                                      sector_filter,
                                      years_filter,
                                      region_filter,
                                      scenario_source_filter,
                                      scenario_filter,
                                      value_name) {
  data_filtered <- data_preprocessed %>%
    filter(.data$sector == !!sector_filter) %>%
    filter(.data$region == !!region_filter) %>%
    filter(.data$year %in% !!years_filter) %>%
    filter(.data$scenario_source == !!scenario_source_filter) %>%
    filter(.data$metric_type %in% c("portfolio", "benchmark") |
      (.data$metric_type == "scenario" & .data$metric == scenario_filter)) %>%
    mutate(
      metric_type = paste0(.data$metric_type, "_", as.character(.data$year))
    ) %>%
    select(.data$technology, .data$metric_type, .data$metric,
      value = !!value_name
    )
}

#' Prepares PACTA total_portfolio data for meta-report security type bar chart
#'
#' @param data_total_portfolio Dataframe in the shape of
#'   ".._total_portfolio.rda" dataset from PACTA analysis output in
#'   "30_Processed_Inputs" folder (dataframe).
#' @param other_asset_types Array of character strings that should be summed up
#'   as "Other" asset type (array of character strings).
#'
#' @description This function aggregates and prepares one of PACTA analysis
#'   result files ".._total_portfolio.rda" from "30_Processed_Inputs" folder to
#'   form an input that can be used for plotting meta-report security type
#'   coverage per investor type bar chart
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
#' @return dataframe prepared for the plot
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
#' @return dataframe prepared for the plot
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
#' @return dataframe with columns investor_name, portfolio_name, value
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

#' Prepares results data per asset type for bubble chart plot
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
#' @description This function aggregates and prepares one of PACTA analysis
#'   result file "Bonds/Equity_results_portfolio.rda" from "40_Results" folder
#'   to form an input that can be used for plotting meta-report bubble chart
#'   showing a planned technology build-out as percentage of build-out required
#'   by a scenario against the current technology share.
#'
#' @return dataframe with columns investor_name, portfolio_name,
#'   value_x, value_y
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

prepare_for_map_chart <- function(data_map_asset_type,
                                  asset_type,
                                  technology_filter,
                                  year_filter,
                                  value_divisor = 1,
                                  allocation_method = NULL) {

  if (is.null(allocation_method)) {
    if (asset_type == "Equity") {
      allocation_method = "ownership_weight"
    } else {
      allocation_method = "portfolio_weight"
    }
  }

  data_map <- data_map_asset_type %>%
    filter(.data$technology == technology_filter,
    .data$allocation == allocation_method,
    .data$year == year_filter,
    .data$equity_market == "Global") %>%
    group_by(.data$ald_location) %>%
    summarise(value = sum(.data$plan_alloc_wt_tech_prod, na.rm=TRUE)/value_divisor,
              unit = max(.data$ald_production_unit)) %>%
    ungroup() %>%
    na.omit() %>%
    mutate(country = iso.expand(.data$ald_location, regex=TRUE)) %>%
    mutate(country = case_when(
      .data$country == "(^France)|(^Clipperton Island)" ~ "France",
      .data$country == "(^China(?!:Hong Kong|:Macao))|(^Paracel Islands)" ~ "China",
      .data$country == "Norway(?!:Bouvet|:Svalbard|:Jan Mayen)" ~ "Norway",
      .data$country == "UK(?!r)" ~ "UK",
      .data$country == "(^Spain)|(^Canary Islands)" ~ "Spain",
      .data$country == "(^Trinidad)|(^Tobago)" ~ "Trinidad", # assign to Trinidad because it is bigger
      TRUE ~ .data$country
    )) %>%
    mutate(abbreviation_divisor = case_when(
      !!value_divisor == 1 ~ "",
      !!value_divisor == 10^3 ~ "k",
      !!value_divisor == 10^6 ~ "M",
      !!value_divisor == 10^9 ~ "B",
      TRUE ~ as.character(value_divisor)
     ))

  joined_map <- left_join(map_data("world"), data_map, by = c("region" = "country"))

}
