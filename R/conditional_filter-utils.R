#' Build a controller based on data
#'
#' @param dat The data supplied as `dataframe` or `tibble`.
#' @param session Shiny session object.
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
#' @param method Method to use for switch_controller condition (defaults to
#'  `length`).
#' @param val Value to use for switch_controller condition (defaults to
#'  `1`)
#' @param logical Boolean to determine whether the switch_controller is
#'  constructed for logical values.
#'
#' @return Shiny controller call.
#' @export
detect_controller <- function(
    dat,
    session = getDefaultReactiveDomain(),
    labels = character(1),
    external = character(1),
    remove_na = FALSE,
    update = FALSE
    ) {

  # defuse
  dat <- rlang::enquo(dat)

  st <- function(var, label) {

    # reconstruct original dataframe subset call
    dt <- rlang::call2("$", rlang::quo_get_expr(dat), rlang::parse_expr(var))

    # make new quo
    dt <- rlang::new_quosure(dt, env = rlang::quo_get_env(dat))

    # execute
    detect_controller_(dt, session, label, remove_na, update)
  }

  # original names columns
  nms <- names(rlang::eval_tidy(dat))
  # augment labels in case of logical columns
  labels <- ifelse(nms %in% names(labels), labels, nms)

  # vectorize gui controller build
  ctrls <- purrr::map2(nms, labels, st) |>
    # set names
    rlang::set_names(nms) |>
    # remove empty controllers
    purrr::compact()

  # discard external controllers
  ctrls[!names(ctrls) %in% external]
}

# col = qusoure
detect_controller_ <- function(col, session, label, remove_na = FALSE,
                               update = FALSE) {

  # inject preserve expression
  args <- col_vals(
    rlang::eval_tidy(col),
    rlang::quo_get_expr(col),
    rlang::quo_get_env(col),
    remove_na,
    update
  )
  # make id
  id <- gsub("(.)*\\$", "", rlang::as_label(col))
  # label
  if (label == "") label <- id

  # update requires some changes to controller and args
  if (all(!is.null(args[[1]]), isTRUE(update))) {
    # change name
    substring(args[[1]], 1) <- toupper(substring(args[[1]], 1, 1))
    args[[1]] <- paste0("update", args[[1]])
    # add session and id
    args[[2]] <- append(c(session = session, inputId = id), args[[2]])
    # set choices and ranges to NULL as these won't change now
    args[[2]]$choices <- args[[2]]$min <- args[[2]]$max <- args[[2]]$multiple <- NULL
  } else {
    # add id and optional label
    args[[2]] <- append(
      c(inputId = session$ns(id), label = label),
      args[[2]]
    )
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

  # defuse
  dat <- rlang::enquo(dat)

  # make the call for obtaining logical values
  detect_lgl <- rlang::call2("detect_lgl", rlang::quo_get_expr(dat),
                             ignore = ignore, external = external)

  # eval if update
  if (isFALSE(update)) {
    detect_lgl <- rlang::eval_tidy(detect_lgl , env =rlang::quo_get_env(dat))
  }

  # make the isolate call
  logi_iso <- rlang::call2("isolate", rlang::parse_expr("input$logi"), .ns = "shiny")

  # javascript for selectize
  js_lgl <- list(
    placeholder = "select",
    onInitialize = I('function() { this.setValue(null); }')
  )

  # core arguments for logical selection
  logi <- list(
    session  = if (isTRUE(update)) session else NULL,
    inputId = if (isTRUE(update)) "logi" else session$ns( "logi"),
    label =  labels,
    choices = detect_lgl,
    selected = if (isTRUE(update)) logi_iso else NULL,
    multiple = if (isTRUE(update))  NULL else TRUE,
    options = if (isTRUE(update)) NULL else js_lgl
  ) |>
  # remove empty elements
    purrr::compact()

  # make controller
  if (isTRUE(update)) sh <-  "updateSelectizeInput" else sh <- "selectizeInput"

  # make logi controller
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

  # defuse
  dat <- rlang::enquo(dat)

  # shinyjs for logical columns
  if (isTRUE(logical)) {

    cl <- rlang::call2("detect_lgl", rlang::quo_get_expr(dat), ignore = ignore,
                       external = external)
    ctrls <- switch_controller_(cl, method, val)

  # shinjs for other columns types
  } else {

  st <- function(var) {
    # reconstruct original dataframe subset call
    dt <- rlang::call2("$", rlang::quo_get_expr(dat), var)
    # execute
    rlang::inject(switch_controller_(!!dt, method, val))
  }

  # original names columns
  nms <- names(rlang::eval_tidy(dat))

  # vectorize gui controller build
  ctrls <- purrr::map(nms, st) |>
    # set names
    rlang::set_names(nms) |>
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
#' @param col Column of a dataframe.
#' @param expr The expression of param `col`.
#' @param env The original environment of param `col`.
#' @param remove_na Remove NA's.
#' @param update Logical indicating whether ui controllers are ui- or
#'  server-side for updating the controllers ranges.
#'
#' @return List containing the ingredients to build a Shiny controller.
#' @export
col_vals <- function(col, expr, env, remove_na = FALSE, update = FALSE) {

  UseMethod("col_vals")
}
#' @rdname col_vals
#'
#' @export
col_vals.numeric <- function(col, expr, env, remove_na = FALSE, update = FALSE) {

  # body of arguments
  vls <- rlang::exprs(
    min = !!detect_rng("min", expr, na.rm = TRUE),
    max = !!detect_rng("max", expr, na.rm = TRUE),
    value = !!detect_rng("range", expr, na.rm = TRUE)
  )

  # eval if update
  if (isFALSE(update)) vls <- purrr::map(vls, rlang::eval_tidy, env = env)

  # shiny controller
  cnr <- "sliderInput"

  # return
  list(cnr, vls)
}
#' @rdname col_vals
#'
#' @export
col_vals.character <- function(col, expr, env, remove_na = FALSE,
                               update = FALSE) {

  # body of arguments
  vls <- rlang::exprs(
    choices = !!detect_lvls(expr, remove_na = remove_na),
    selected = !!detect_lvls(expr, remove_na = remove_na),
    multiple = TRUE
  )

  # eval if update
  if (isFALSE(update)) vls <- purrr::map(vls, rlang::eval_tidy, env = env)

  # shiny controller
  cnr <- "selectInput"

  # return
  list(cnr, vls)
}
#' @rdname col_vals
#'
#' @export
col_vals.factor <- col_vals.character
#' @rdname col_vals
#'
#' @export
col_vals.logical <- function(col, expr, env, remove_na = FALSE, update = FALSE)NULL

#-------------------------------------------------------------------------------
# helper functions
#-------------------------------------------------------------------------------
# generate expression for character/logical toggle switch condition
detect_lvls <- function(col, remove_na) {
  if (isTRUE(remove_na)) {
    col <- rlang::call2("na.omit", col)
  }
  rlang::call2("unique", rlang::call2("as.factor", col))
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
  vapply(dat, function(x) utils::tail(class(x), 1), character(1))
}
