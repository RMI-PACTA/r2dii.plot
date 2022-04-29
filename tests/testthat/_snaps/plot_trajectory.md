# is sensitive to `value_col`

    

# with bad `perc_y_scale` errors gracefully

    is.logical(perc_y_scale) is not TRUE

# throws expected warning about API change

    The `data` argument of `plot_trajectory()` must be prepped already as of r2dii.plot 0.4.0.
    * From the next release the call to the plot functions will change to:
    data %>% r2dii.plot::prep_trajectory() %>% r2dii.plot::plot_trajectory().
    * Custom data preparation will also become possible.

