#' Hadley's Dataset Module
#'
#' @param id Namespace of the module.
#' @param filter Filter the datasets (e.g. `is.data.frame`).
#'
#' @return Shiny module.
#' @export
dataset_ui <- function(id, filter = NULL) {
  names <- ls("package:datasets")
  if (!is.null(filter)) {
    data <- lapply(names, get, "package:datasets")
    names <- names[vapply(data, filter, logical(1))]
  }

  selectInput(NS(id, "dataset"), "Pick a dataset", choices = names)
}
#' @rdname dataset_ui
#'
#' @export
dataset_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive(get(input$dataset, "package:datasets"))
  })
}
