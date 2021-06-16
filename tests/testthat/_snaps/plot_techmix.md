# without `market_share` data errors gracefully

    Must have missing names:
    metric, region, scenario_source, technology, technology_share
    Is your data `market_share`-like?

# with zero-row data errors gracefully

    `zero_row` must have some rows but has none.

# with more than one scenario errors gracefully

    `prep$metric` must have a single scenario not 3: target_cps, target_sds, target_sps.
    You may pick one scenario, e.g. 'target_cps' with:
      subset(prep, metric %in% c('projected', 'corporate_economy', 'target_cps'))

# with too many sectors errors gracefully

    `bad_sector` must have a single value of `sector` but has: a, b.
    Pick one value, e.g. 'a', with:
      subset(bad_sector, sector == 'a')

# with too many regions errors gracefully

    `bad_region` must have a single value of `region` but has: a, b.
    Pick one value, e.g. 'a', with:
      subset(bad_region, region == 'a')

# with too many scenario_source errors gracefully

    `bad_scenario_source` must have a single value of `scenario_source` but has: a, b.
    Pick one value, e.g. 'a', with:
      subset(bad_scenario_source, scenario_source == 'a')

# with too few scenarios errors gracefully

    `too_few$metric` must have one scenario but has none.

