# if `data` is not a data frame errors gracefully

    is.data.frame(data) is not TRUE

# if `data` is not sda-like errors gracefully

    Must have missing names:
    emission_factor_metric, emission_factor_value

# if `data` has zero rows errors gracefully

    `zero_row` must have some rows but has none.

# with too many sectors errors gracefully

    `data` must have a single value of `sector` but has: a, b.
    Pick one value, e.g. 'a', with:
      subset(data, sector == 'a')

# with bad `extrapolate` errors gracefully

    is.logical(extrapolate) is not TRUE

