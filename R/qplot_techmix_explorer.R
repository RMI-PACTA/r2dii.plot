#' Interactive `qplot_techmix` options explorer
#'
#' @inherit qplot_techmix
#'
#' @description
#' An interactive shiny widget to explore the `qplot_techmix` options.
#'
#' @examples
#' qplot_techmix_explorer(market_share)
#'
#' @export

qplot_techmix_explorer <- function(data) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("You must have {shiny} installed to use `qplot_techmix_explorer()`")
  }

  obj_name <- deparse(substitute(data))

  ui <- shiny::fluidPage(
    shiny::inputPanel(
      shiny::selectInput(
        inputId = "sector",
        label = "sector:",
        choices = unique(data[["sector"]])
      ),
      shiny::selectInput(
        inputId = "region",
        label = "region:",
        choices = unique(data[["region"]])
      ),
      shiny::selectInput(
        inputId = "scenario_source",
        label = "scenario_source:",
        choices = unique(data[["scenario_source"]])
      ),
      shiny::selectInput(
        inputId = "target",
        label = "target:",
        choices = setdiff(unique(market_share$metric), c("projected", "corporate_economy"))
      ),
      shiny::downloadButton("download_jpg", "save JPG"),
      shiny::downloadButton("download_png", "save PNG")
    ),
    shiny::plotOutput("qplot_out")
  )

  server <- function(input, output) {
    output$qplot_out <-
      shiny::renderPlot(
        {
          qplot_techmix(
            dplyr::filter(
              data,
              .data[["sector"]] == input[["sector"]],
              .data[["region"]] == input[["region"]],
              .data[["scenario_source"]] == input[["scenario_source"]],
              .data[["metric"]] %in% c(input[["target"]], "projected", "corporate_economy")
            )
          )
        }
      )

    output$download_jpg <- shiny::downloadHandler(
      filename = function() {
        paste0(obj_name, ".jpg")
      },
      content = function(file) {
        ggplot2::ggsave(
          filename = file,
          plot = qplot_techmix(
            dplyr::filter(
              data,
              .data[["sector"]] == input[["sector"]],
              .data[["region"]] == input[["region"]],
              .data[["scenario_source"]] == input[["scenario_source"]],
              .data[["metric"]] %in% c(input[["target"]], "projected", "corporate_economy")
            )
          )
        )
      }
    )

    output$download_png <- shiny::downloadHandler(
      filename = function() {
        paste0(obj_name, ".png")
      },
      content = function(file) {
        ggplot2::ggsave(
          filename = file,
          plot = qplot_techmix(
            dplyr::filter(
              data,
              .data[["sector"]] == input[["sector"]],
              .data[["region"]] == input[["region"]],
              .data[["scenario_source"]] == input[["scenario_source"]],
              .data[["metric"]] %in% c(input[["target"]], "projected", "corporate_economy")
            )
          )
        )
      }
    )
  }

  shiny::shinyApp(ui = ui, server = server)
}
