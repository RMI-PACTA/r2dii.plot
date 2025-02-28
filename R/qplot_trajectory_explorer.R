#' Interactive `qplot_trajectory` options explorer
#'
#' @param data a network description in one of numerous forms (see details)
#'
#' @description
#' An interactive shiny widget to explore the `qplot_trajectory` options.
#'
#' @export

qplot_trajectory_explorer <- function(data) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("You must have {shiny} installed to use `qplot_trajectory_explorer()`")
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
        inputId = "technology",
        label = "technology:",
        choices = unique(data[["technology"]])
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
      shiny::downloadButton("download_jpg", "save JPG"),
      shiny::downloadButton("download_png", "save PNG")
    ),
    shiny::plotOutput("qplot_out")
  )

  server <- function(input, output) {
    output$qplot_out <-
      shiny::renderPlot(
        {
          qplot_trajectory(
            dplyr::filter(
              data,
              .data[["sector"]] == input[["sector"]],
              .data[["technology"]] == input[["technology"]],
              .data[["region"]] == input[["region"]],
              .data[["scenario_source"]] == input[["scenario_source"]]
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
          plot = qplot_trajectory(
            dplyr::filter(
              data,
              .data[["sector"]] == input[["sector"]],
              .data[["technology"]] == input[["technology"]],
              .data[["region"]] == input[["region"]],
              .data[["scenario_source"]] == input[["scenario_source"]]
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
          plot = qplot_trajectory(
            dplyr::filter(
              data,
              .data[["sector"]] == input[["sector"]],
              .data[["technology"]] == input[["technology"]],
              .data[["region"]] == input[["region"]],
              .data[["scenario_source"]] == input[["scenario_source"]]
            )
          )
        )
      }
    )
  }

  shiny::shinyApp(ui = ui, server = server)
}
