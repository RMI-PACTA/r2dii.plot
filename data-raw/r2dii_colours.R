library(tibble)

# styler: off
palette_colours <- tribble(
         ~label,      ~hex,
    "dark_blue", "#1b324f",
        "green", "#00c082",
       "orange", "#ff9623",
         "grey", "#d0d7e1",
  "dark_purple", "#574099",
       "yellow", "#f2e06e",
    "soft_blue", "#78c4d6",
     "ruby_red", "#a63d57",
   "moss_green", "#4a5e54"
)

scenario_colours <- tribble(
          ~label,      ~hex,
    "dark_green", "#768555",
   "light_green", "#9CAB7C",
   "dark_yellow", "#FFFFCC",
  "light_yellow", "#FDE291",
           "red", "#E07B73"
)

sector_colours <- tribble(
        ~label,      ~hex,
  "automotive", "#78C4D6",
    "aviation", "#00c082",
      "cement", "#c1b358",
        "coal", "#4e3b37",
     "oil&gas", "#181716",
       "power", "#a63603",
    "shipping", "#574099",
       "steel", "#a63d57"
)

technology_colours <- tribble(
        ~sector,     ~technology,      ~hex,
        "power",       "coalcap", "#7A2701",
        "power",        "oilcap", "#a63603",
        "power",        "gascap", "#e6550d",
        "power",    "nuclearcap", "#fd8d3c",
        "power",      "hydrocap", "#fdbe85",
        "power", "renewablescap", "#ffd4ad",
   "automotive",       "ice_hdv", "#548995",
   "automotive",           "ice", "#609cab",
   "automotive",    "hybrid_hdv", "#6cb0c0",
   "automotive",        "hybrid", "#78c4d6",
   "automotive",      "fuelcell", "#93cfde",
   "automotive",  "electric_hdv", "#aedbe6",
   "automotive",      "electric", "#c9e7ee",
      "oil&gas",           "gas", "#b9b5b0",
      "oil&gas",           "oil", "#181716",
 "fossil fuels",           "gas", "#b9b5b0",
 "fossil fuels",           "oil", "#181716",
 "fossil fuels",          "coal", "#4e3b37"
)
# styler: on

usethis::use_data(
  palette_colours,
  scenario_colours,
  sector_colours,
  technology_colours,
  internal = FALSE,
  overwrite = TRUE
)
