library(r2dii.ggplot)

example_data <- get_example_data()
example_data <- process_input_data(example_data)

data_trajectory <- filter_data_for_trajectory_chart(data = example_data,sector = "power", technology = "oilcap",
                                                    region = "global",scenario_source = "demo_2020",
                                                    value_name = "production", end_year = 2025,
                                                    normalize_to_start_year = TRUE)

scenario_specs <- data.frame(scenario = c("sds","sps","cps","worse"),
                             color = c("#9CAB7C","#FFFFCC","#FDE291","#E07B73"),
                             label = c("SDS","STEPS","CPS","worse"))

main_line_metric <- data.frame(metric = "projected", label = "Portfolio")
additional_line_metrics <- data.frame(metric = "corporate_economy", label = "Corporate Economy")

plot <- plot_trajectory_chart(data_trajectory,
                              plot_title = "Production trajectory of Oil Capacity technology \n in the Power sector",
                              x_title = "Year", y_title = "Production rate (normalized to 2020)",
                              annotate_data = FALSE,
                              scenario_specs_good_to_bad = scenario_specs,
                              main_line_metric, additional_line_metrics)


plot



data_techmix_power <- filter_data_for_techmix_chart(example_data,sector = "power",
                                                    years = c(2020,2025),region = "global",
                                                    scenario_source = "demo_2020",
                                                    scenario = "sds",value_name = "technology_share")

tech_colors_power <- get_sector_colors("power")
bars_labels_specs <- data.frame("metric_type" = c("portfolio_2020", "benchmark_2020","portfolio_2025","benchmark_2025","scenario_2025"),
                 "label" = c("Portfolio 2020", "Benchmark 2020", "Portfolio 2025", "Benchmark 2025", "Target SDS 2025"))

plot_techmix_power <- plot_techmix_chart(data_techmix_power,"Technology mix for the Power sector",
                                         show_legend = TRUE, tech_colors_power, bars_labels_specs)
plot_techmix_power

data_techmix_auto <- filter_data_for_techmix_chart(example_data,sector = "automotive",
                                                   years = c(2020,2025),region = "global",
                                                   scenario_source = "demo_2020",
                                                  scenario = "sds",value_name = "technology_share")

tech_colors_auto <- get_sector_colors("automotive")

plot_techmix_auto <- plot_techmix_chart(data_techmix_auto,
                                        "Technology mix for the Automotive sector",
                                        show_legend = TRUE,
                                   tech_colors_auto, bars_labels_specs)
plot_techmix_auto
