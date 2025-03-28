# with zero-row data errors gracefully

    `zero_row` must have some rows.
    x `zero_row` has zero rows.

# without `market_share` data errors gracefully

    `data` must have all the expected names.
    x Missing names: metric, technology, technology_share.
    i Is your data `market_share_demo`-like?
    Caused by error in `abort_if_missing_names()`:
    ! `data` must have all the expected names.
    x Missing names: metric, technology, technology_share.

# with more than one scenario errors gracefully

    `prep$metric` must have a single scenario not 3.
    i Do you need to pick one scenario? E.g. pick 'target_cps' with: `subset(prep, metric %in% c('projected', 'corporate_economy', 'target_cps'))`.
    x Provided: target_cps, target_sds, target_sps.

# with too many sectors errors gracefully

    `bad_sector` must have a single value of `sector`.
    i Do you need to pick one value? E.g. pick 'a' with: `subset(bad_sector, sector == 'a')`.
    x Provided: a, b.

# with too many regions errors gracefully

    `bad_region` must have a single value of `region`.
    i Do you need to pick one value? E.g. pick 'a' with: `subset(bad_region, region == 'a')`.
    x Provided: a, b.

# with too many scenario_source errors gracefully

    `bad_scenario_source` must have a single value of `scenario_source`.
    i Do you need to pick one value? E.g. pick 'a' with: `subset(bad_scenario_source, scenario_source == 'a')`.
    x Provided: a, b.

# with too few scenarios errors gracefully

    `too_few$metric` must have one scenario.
    x It has none.

