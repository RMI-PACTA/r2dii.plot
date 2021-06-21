# if `data` is not a data frame errors gracefully

    is.data.frame(data) is not TRUE

# if `data` is not market_share-like errors gracefully

    Must have missing names:
    metric, region, scenario_source, technology
    Is your data `market_share`-like?

# with zero-row data errors gracefully

    `zero_row` must have some rows but has none.

# with too many sectors errors gracefully

    `bad_sector` must have a single value of `sector` but has: a, b.
    Pick one value, e.g. 'a', with:
      subset(bad_sector, sector == 'a')

# with too many technologies errors gracefully

    `bad_tech` must have a single value of `technology` but has: a, b.
    Pick one value, e.g. 'a', with:
      subset(bad_tech, technology == 'a')

# with too many regions errors gracefully

    `bad_region` must have a single value of `region` but has: a, b.
    Pick one value, e.g. 'a', with:
      subset(bad_region, region == 'a')

# with too many scenario_source errors gracefully

    `bad_scenario_source` must have a single value of `scenario_source` but has: a, b.
    Pick one value, e.g. 'a', with:
      subset(bad_scenario_source, scenario_source == 'a')

# with too many scenarios errors gracefully

    `metric` must have between 1 and 4 scenarios, not 8: cps, sds, sps, a, b, c, d, e

# informs that values are normalized

    Code
      invisible(plot_trajectory(data))
    Message <message>
      Normalizing `production` values to 2020 -- the start year.

