library(tibble)
library(usethis)

paths <- list.files("data-raw/data_dictionary", full.names = TRUE)

out <- lapply(
  paths,
  function(.x) {
    utils::read.csv(
      .x,
      colClasses = "character",
      na.strings = c("", "NA"),
      stringsAsFactors = FALSE
    )
  }
)
out <- Reduce(rbind, out)
out <- tibble::as_tibble(out[order(out$dataset, out$column), , drop = FALSE])

data_dictionary <- out
usethis::use_data(data_dictionary, overwrite = TRUE)
