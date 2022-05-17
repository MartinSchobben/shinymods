filterStore <- R6::R6Class(
  # my own class
  "filterStore",

  # variables and functions
  public = list(
    # arguments
    original = NULL,
    augmented = NULL,
    vars = reactiveValues(),
    ignore = NULL,
    shinyjs = NULL,
    remove_na = NULL,

    # initialize (new)
    initialize = function(original, vars, ignore, shinyjs, remove_na) {
      # checks
      stopifnot(is.data.frame(original))
      stopifnot(is.reactivevalues(vars))
      stopifnot(is.character(ignore))
      stopifnot(rlang::is_bool(shinyjs))
      stopifnot(rlang::is_bool(remove_na))

      self$original <- original
      self$vars <- vars
      self$ignore <- ignore
      self$shinyjs <- shinyjs
      self$remove_na <- remove_na
    },
    addvars = function(var) {
      newvars <- append(reactiveValuesToList(self$vars),  reactiveValuesToList(var))
      self$vars <- reactiveValues(!!! newvars)
    },
    filter = function(val) {
      lgl <- purrr::map(
        names(self$vars),
        ~filter_var(self$original[[.x]], val[[.x]], remove_na = self$remove_na)
      ) %>%
        purrr::reduce(`&`)
      self$augmented <- self$original[lgl, ,drop = FALSE]
    }
  )
)

# filter operation on the dataset based on variable class
filter_var <- function(x, val = NULL, remove_na = FALSE) {

  # # shortcut with val `NULL`
  if (is.null(x) & is.null(val))  return(TRUE)

  if (is.numeric(x)) {
    y <- x >= val[1] & x <= val[2]
  } else if (is.character(x) | is.factor(x)) {
    y <- x %in% val
  } else if (is.logical(x) & is.null(val)) {
    y <- x
  } else {
    # No control, so don't filter
    y <- TRUE
  }
  if (isTRUE(remove_na)) y  & !is.na(x) else y | is.na(x)
}
