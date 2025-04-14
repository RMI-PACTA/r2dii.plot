#' Title
#'
#' @param .data A data frame like the outputs of
#'   `r2dii.match::calculate_match_success_rate()`.
#' @param ... Additional grouping columns.
#' @param .x The name of the column to be used as the x-axis.
#'
#' @returns An object of class "ggplot".
#'
#' @export
#'
#' @examples
#' coverage <-
#' r2dii.data::loanbook_demo %>%
#'   r2dii.match::match_name(r2dii.data::abcd_demo) %>%
#'   r2dii.match::prioritize() %>%
#'   r2dii.match::calculate_match_success_rate(loanbook = r2dii.data::loanbook_demo)
#'
#' plot_match_success_rate(coverage)
#'
#' @importFrom dplyr %>%

plot_match_success_rate <- function(.data, ..., .x = NULL) {
  if (is.null(.x)) {
    .x <- as.symbol("matched")
  } else {
    .x <- as.symbol(.x)
  }

  .data %>%
    dplyr::summarize(
      loan_size_outstanding = sum(.data$loan_size_outstanding, na.rm = TRUE),
      .by = c("matched", ...)
    ) %>%
    dplyr::arrange(.data[["matched"]], ...) %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(
      mapping = ggplot2::aes(
        x = {{.x}},
        y = .data[["loan_size_outstanding"]],
        fill = .data[["matched"]]
      )
    ) +
    theme_2dii() +
    scale_fill_r2dii() +
    ggplot2::theme(legend.position = "top")
}
