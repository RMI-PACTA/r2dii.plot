# if `data` is not a data frame errors gracefully

    is.data.frame(data) is not TRUE

# if `data` is not market_share-like errors gracefully

    `data` must have all the expected names.
    x Missing names: metric, production, region, scenario_source, technology.
    i Is your data `market_share`-like?

# with zero-row data errors gracefully

    `zero_row` must have some rows.
    x `zero_row` has zero rows.

# with too many sectors errors gracefully

    `bad_sector` must have a single value of `sector`.
    i Do you need to pick one value? E.g. pick 'a' with: `subset(bad_sector, sector == 'a')`.
    x Provided: a, b.

# with too many technologies errors gracefully

    `bad_tech` must have a single value of `technology`.
    i Do you need to pick one value? E.g. pick 'a' with: `subset(bad_tech, technology == 'a')`.
    x Provided: a, b.

# with too many regions errors gracefully

    `bad_region` must have a single value of `region`.
    i Do you need to pick one value? E.g. pick 'a' with: `subset(bad_region, region == 'a')`.
    x Provided: a, b.

# with too many scenario_source errors gracefully

    `bad_scenario_source` must have a single value of `scenario_source`.
    i Do you need to pick one value? E.g. pick 'a' with: `subset(bad_scenario_source, scenario_source == 'a')`.
    x Provided: a, b.

# with too many scenarios errors gracefully

    `metric` must have between 1 and 4 scenarios.
    x Provided 8 scenarios: target_cps, target_sds, target_sps, target_a, target_b, target_c, target_d, target_e

# with too many metrics errors gracefully

    The number of lines to plot must be 5 or less.
    i Do you need to split the data over multiple plots?
    x Found 8 lines: a, b, c, corporate_economy, d, e, f, projected.

# informs that values are normalized

    Code
      invisible(qplot_trajectory(data))
    Message <message>
      Normalizing `production` values to 2020 -- the start year.

# Wraps the title as expected

    Production Trajectory of Technology: Electric
    in the Automotive Sector

# Wraps the subtitle as expected

    The coloured areas indicate trajectories in reference to a scenario.
    The red area indicates trajectories not aligned with any sustainble scenario.

# Prints axis labels as expected

    [1] "Year"

---

    Production Rate (normalized to 2020)

