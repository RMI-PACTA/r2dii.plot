#' Get the predefined technology colors for a sector
#'
#' @param sector Sector for which we want to retrieve colors (character string).
#'
#' @export
#' @examples
#' get_r2dii_technology_colours("power")
get_r2dii_technology_colours <- function(sector) {
  # styler: off
  all_colours <- tribble(
         ~sector,     ~technology,                         ~label, ~colour_hex,
         "Power",       "CoalCap",                "Coal Capacity",  "#7A2701",
         "Power",        "OilCap",                 "Oil Capacity",  "#a63603",
         "Power",        "GasCap",                 "Gas Capacity",  "#e6550d",
         "Power",    "NuclearCap",             "Nuclear Capacity",  "#fd8d3c",
         "Power",      "HydroCap",               "Hydro Capacity",  "#fdbe85",
         "Power", "RenewablesCap",          "Renewables Capacity",  "#ffd4ad",
    "Automotive",      "Electric",                     "Electric",  "#548995",
    "Automotive",  "Electric_HDV", "Electric Heavy Duty Vehicles",  "#609cab",
    "Automotive",      "FuelCell",                     "FuelCell",  "#6cb0c0",
    "Automotive",        "Hybrid",                       "Hybrid",  "#78c4d6",
    "Automotive",    "Hybrid_HDV",   "Hybrid Heavy Duty Vehicles",  "#93cfde",
    "Automotive",           "ICE",                          "ICE",  "#aedbe6",
    "Automotive",       "ICE_HDV",      "ICE Heavy Duty Vehicles",  "#c9e7ee",
       "Oil&Gas",           "Gas",                          "Gas",  "#b9b5b0",
       "Oil&Gas",           "Oil",                          "Oil",  "#181716",
  "Fossil Fuels",           "Gas",                          "Gas",  "#b9b5b0",
  "Fossil Fuels",           "Oil",                          "Oil",  "#181716",
  "Fossil Fuels",          "Coal",                         "Coal",  "#4e3b37"
  )
  # styler: on

  all_colours <- all_colours %>%
    mutate(
      sector = tolower(.data$sector),
      technology = tolower(.data$technology)
    )

  colours <- all_colours %>%
    filter(.data$sector == !!sector) %>%
    select(.data$technology, .data$label, colour = .data$colour_hex)

  colours
}

#' Get the 2DII colour palette
#'
#' @export
#' @examples
#' r2dii_palette_colours()
r2dii_palette_colours <- function() {
  # styler: off
  tribble(
         ~label, ~colour_hex,
    "dark_blue",   "#1b324f",
        "green",   "#00c082",
       "orange",   "#ff9623",
         "grey",   "#d0d7e1",
  "dark_purple",   "#574099",
       "yellow",   "#f2e06e",
    "soft_blue",   "#78c4d6",
     "ruby_red",   "#a63d57",
   "moss_green",   "#4a5e54"
  )
  # styler: on
}

#' Get the 2DII sector colour palette
#'
#' @return Tribble with sector labels and colour hex strings.
#' @export
#' @examples
#' r2dii_sector_colours()
r2dii_sector_colours <- function() {
  # styler: off
  tribble(
        ~label, ~colour_hex,
  "automotive",   "#78C4D6",
    "aviation",   "#00c082",
      "cement",   "#c1b358",
        "coal",   "#4e3b37",
     "oil&gas",   "#181716",
       "power",   "#a63603",
    "shipping",   "#574099",
       "steel",   "#a63d57"
  )
  # styler: on
}
