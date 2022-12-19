# is sensitive to `value_col`

    

# with bad `perc_y_scale` errors gracefully

    is.logical(perc_y_scale) is not TRUE

# throws expected warning about API change

    The `data` argument of `plot_trajectory()` must be prepped already as of r2dii.plot 0.4.0.
    i From the next release you will need to call `r2dii.plot::plot_trajectory(data)` prior to calling `r2dii.plot::plot_trajectory()`.
    i Alternatively custom data preparation will also become possible.

