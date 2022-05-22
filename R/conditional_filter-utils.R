#' Build a controller based on data
#'
#' @param dat The data supplied as `dataframe` or tibble.
#' @param session Namespace of the module or session.
#' @param labels Labels for the controllers (defaults to the variable names).
#'  Must be a named vector where the names refer to the names of the original
#'  data.
#' @param external `reactiveValues` assigning external filter variables to the
#'  module.
#' @param ignore Character string/vector for variables to be ignored for
#'  filtering of the dataset.
#' @param remove_na Logical indicating whether NAs should be removed during
#'  filtering (defaults to FALSE).
#' @param update Logical indicating whether ui controllers are ui- or
#'  server-side for updating the controllers ranges.
#'
#' @return Shiny controller call.
#' @export
detect_controller <- function(
    dat,
    session,
    labels = character(1),
    external = character(1),
    remove_na = FALSE,
    update = FALSE
    ) {

  st <- function(x, y, label) {
    # reconstruct original dataframe subset
    dt <- rlang::parse_expr(paste0(deparse(sys.call(1)[[2]]), "$", y))
    # execute
    rlang::inject(detect_controller_(!!dt, session, label, remove_na, update))
  }

  # augment labels in case of logical columns
  labels <- ifelse(names(dat) %in% names(labels), labels, names(dat))

  # vectorize gui controller build
  ctrls <- rlang::inject(
    purrr::pmap(list(!!substitute(dat), names(!!substitute(dat)), labels), st)
  ) |>
    # remove empty controllers
    purrr::compact()

  # discard external controllers
  ctrls[!names(ctrls) %in% external]
}

detect_controller_ <- function(col, session, label, remove_na = FALSE,
                               update = FALSE) {

  # save expression
  col <- rlang::enexpr(col)
  # inject preserve expression
  args <- rlang::inject(col_vals(!!col, remove_na))
  # make id
  id <- gsub("(.)*\\$", "", deparse(substitute(col)))
  # label
  if (label == "") label <- id

  # update requires some changes to controller and args
  if (all(!is.null(args[[1]]), isTRUE(update))) {
    # change name
    substring(args[[1]], 1) <- toupper(substring(args[[1]], 1, 1))
    args[[1]] <- paste0("update", args[[1]])
    # add session and id
    args[[2]] <- append(c(session = session, inputID = id), args[[2]])
  } else {
    # add id and optional label
    args[[2]] <- append(c(inputID = NS(session, id), label = label), args[[2]])
  }

  # make controller
  if (!is.null(args[[1]])) rlang::call2(args[[1]], !!!args[[2]]) else NULL
}
#' @rdname detect_controller
#'
#' @export
logical_controller <- function(
    dat,
    session,
    labels = "Various",
    external = character(1),
    ignore = character(1),
    remove_na = FALSE,
    update = FALSE
) {

  if (length(labels) > 1) {
    stop("Only vectors `labels` of length one are accepted.", call. = FALSE)
  }

  # make the call for obtaining logical values
  call_detect_lgl <- rlang::call2("detect_lgl", substitute(dat),
                                 ignore = ignore, external = external)
  # make the isolate call
  logi_iso <- rlang::call2("isolate", rlang::expr(input$logi), .ns = "shiny")

  # javascript for selectize
  js_lgl <- list(
    placeholder = "select",
    onInitialize = I('function() { this.setValue(null); }')
  )

  # core arguments for logical selection
  logi <- list(
    session  = if (isTRUE(update)) session else NULL,
    inputId = if (isTRUE(update)) "logi" else NS(session, "logi"),
    label =  labels,
    choices = call_detect_lgl,
    selected = if (isTRUE(update)) logi_iso else NULL,
    multiple = if (isTRUE(update))  NULL else TRUE,
    options = if (isTRUE(update)) NULL else js_lgl
  ) |>
  # remove empyt elements
    purrr::compact()

  # make controller
  if (isTRUE(update)) sh <-  "updateSelectizeInput" else sh <- "selectizeInput"
  rlang::call2(sh, !!!logi)
}
#' @rdname detect_controller
#'
#' @export
switch_controller <- function(
    dat,
    method = "length",
    val = 1,
    external = character(1),
    ignore = character(1),
    remove_na = FALSE,
    logical = FALSE
  ) {

  if (isTRUE(logical)) {
    cl <- rlang::call2("detect_lgl", substitute(dat), ignore = ignore,
                       external = external)
    ctrls <- switch_controller_(cl, method, val)

  } else {

  st <- function(dat, nm) {
    # reconstruct original dataframe subset
    dt <- rlang::parse_expr(paste0(deparse(sys.call(1)[[2]]), "$", nm))
    # execute
    rlang::inject(switch_controller_(!!dt, method, val))
  }

  # vectorize gui controller build
  ctrls <- rlang::inject(
    purrr::pmap(list(!!substitute(dat), names(!!substitute(dat))), st)
  ) |>
    # remove empty controllers
    purrr::compact()
  }

  ctrls
}

switch_controller_ <- function(col, method = "length", val = 1,
                               remove_na = FALSE) {

  if (is.numeric(col)) {
    x <- detect_rng("unique", substitute(col))
    # make id
    id <- gsub("(.)*\\$", "", deparse(substitute(col)))
  } else if (is.factor(col) || is.character(col)) {
    x <- detect_lvls(substitute(col), remove_na = remove_na)
    # make id
    id <- gsub("(.)*\\$", "", deparse(substitute(col)))
  } else if (is.call(col)) {
    x <- col
    id <- "logi"
  } else {
    # short-cut for other types
    return(NULL)
  }


  # function to use
  fun <- deparse1(rlang::call2(method, x), collapse = " ")
  # condition for switch
  cond <- rlang::parse_expr(paste(fun, ">", val))
  # make switch call
  rlang::call2("toggleState", id, cond, .ns = "shinyjs")

}

#' Base filter method based on the class of column
#'
#' @param col Column
#' @param remove_na Remove NA's
#'
#' @return List containing the ingredients to build a Shiny controller.
#' @export
col_vals <- function(col, remove_na = FALSE) {

  UseMethod("col_vals")
}
#' @rdname col_vals
#'
#' @export
col_vals.numeric <- function(col, remove_na = FALSE) {

  # quote col
  col <- substitute(col)

  # body of arguments
  vls <- rlang::exprs(
    min = !!detect_rng("min", col, na.rm = TRUE),
    max = !!detect_rng("max", col, na.rm = TRUE),
    value = !!detect_rng("range", col, na.rm = TRUE)
  )

  # shiny controller
  cnr <- "sliderInput"

  # return
  list(cnr, vls)
}
#' @rdname detect_controller
#'
#' @export
col_vals.character <- function(col, remove_na = FALSE) {

  # quote col
  col <- substitute(col)

  # body of arguments
  vls <- rlang::exprs(
    choices = !!detect_lvls(col, remove_na = remove_na),
    selected = !!detect_lvls(col, remove_na = remove_na),
    multiple = TRUE
  )

  # shiny controller
  cnr <- "selectInput"

  # return
  list(cnr, vls)
}
#' @rdname detect_controller
#'
#' @export
col_vals.factor <- col_vals.character
#' @rdname detect_controller
#'
#' @export
col_vals.logical <- function(col, remove_na = FALSE) NULL

#-------------------------------------------------------------------------------
# helper functions
#-------------------------------------------------------------------------------
# generate expression for character/logical toggle switch condition
detect_lvls <- function(col, remove_na) {
  if (isTRUE(remove_na)) {
    col <- rlang::call2("na.omit", col)
  }
  rlang::call2("levels", rlang::call2("as.factor", col))
}

# generate expression for numeric toggle switch condition
detect_rng <- function(stat, col, ...) {
  rlang::call2(stat, col, ...)
}

# generating expression for logical vars toggle switch condition
detect_lgl <- function(dat, external = character(1), ignore = character(1)) {

  col_specs <- col_spec(dat, ignore = ignore, external = external)
  vrs <- names(col_specs)[col_specs == "logical"]

  # check if columns have any `TRUE`
  chk <- vapply(dat[, vrs, drop = FALSE], any, logical(1), na.rm = TRUE)

  # if lenght = 0 return FALSE
  if (length(vrs[chk]) == 0) FALSE else vrs[chk]
}

# variable class data
col_spec <- function(dat, external = character(1), ignore = character(1)) {

  # optionally remove columns
  dat <- dat[, !(names(dat) %in% c(external, ignore)), drop = FALSE]

  # class of each column
  vapply(dat, class, character(1))
}
