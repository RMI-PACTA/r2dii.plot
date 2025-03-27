convert_label_docs <- function(func, default_val) {
  glue::glue(
    "A symbol. The unquoted name of a function to apply to y-axis labels. For example:",
    "  * To convert labels to uppercase use `convert_label = toupper`.",
    "  * To get the default behavior of `{func}` use `convert_label = {default_val}`.",
    .sep = "\n"
  )
}
