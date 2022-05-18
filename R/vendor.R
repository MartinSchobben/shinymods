#' Vendor the shiny modul
#'
#' Vendoring makes the code available as a copy for you to use in your own
#' package.
#'
#' @param module Character vector for the module (`"wizard"`,
#'  `"conditional_filter"`)
#' @param path Path to your package (default is the current working directory).
#'
#' @return The file path to the vendored code (invisibly).
#' @export
shinymods_vendor <- function(module, path = ".") {

  # check whether module is supplied
  stopifnot(is.character(module))

  # module
  module <- paste0(module, "-module.R")

  # system path
  old <- system.file("R", module, package = "shinymods", mustWork = TRUE)

  # new path to R directory file
  new <- file.path(path, "R", module)

  if (file.exists(new)) {
    stop("'", new, "' already exists\n * run file.remove('", new,
         "')", call. = FALSE)
  }

  # check if location has R directory
  if (!dir.exists(dirname(new))) {
    stop("R directory has not been found.", call. = FALSE)
  }

  # vendor to R directory
  file.copy(old, dirname(new))

  invisible(new)
}
