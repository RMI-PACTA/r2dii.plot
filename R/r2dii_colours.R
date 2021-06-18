#' Colour datasets
#'
#' All datasets have at least two columns:
#' * `label`: Text label of the colour.
#' * `hex`: Hex code of the colour.
#'
#' In `scenario_colours`, colours are ordered from red to green to be used in
#' trajectory charts.
#'
#' @name r2dii_colours
#' @keywords internal
#'
#' @examples
#' r2dii.plot:::palette_colours
#'
#' r2dii.plot:::scenario_colours
#'
#' r2dii.plot:::sector_colours
#'
#' r2dii.plot:::technology_colours
#' @noRd
NULL

#' @rdname r2dii_colours
"palette_colours"
#' @rdname r2dii_colours
"scenario_colours"
#' @rdname r2dii_colours
"sector_colours"
#' @rdname r2dii_colours
"technology_colours"
