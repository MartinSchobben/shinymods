#' Download module
#'
#' The module is constructed to allow creating multiple download buttons.
#'
#' @param id Namespace of the module.
#' @param label On download button.
#' @param n The number of download handles.
#' @param data The output data.
#' @param name Name of the output file.
#' @param sep Determines the type of file `,` for a csv-file and `\t` for a tab-
#'  delimited text file.
#'
#' @return Shiny module.
#' @export
output_ui <- function(id, label = "Download", n) {
  downloadButton(NS(id, paste0("download", n)), label)
}
#' @rdname output_ui
#'
#' @export
output_server <- function(id, data, name, n, sep) {

  stopifnot(is.reactive(data))
  stopifnot(!is.reactive(name))
  stopifnot(!is.reactive(n))
  stopifnot(!is.reactive(sep))

  moduleServer(id, function(input, output, session) {

    observe({
      message(glue::glue("{str(input$download1)}"))
      purrr::walk(
        1:n,
        ~{
          output_ext <- switch(sep, `,` = "csv", `\t` = "txt")
          output_name <- paste0("download", .x)
          output[[output_name]] <- downloadHandler(
            filename = function() paste(name, output_ext, sep = "."),
            content = function(file) {
              utils::write.table(data(), file, sep = sep)
            }
          )
        }
      )
    })
  })
}
