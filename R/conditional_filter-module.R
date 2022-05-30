#' Conditional filter module
#'
#' The module is constructed so that the filter controls are updated based on
#' the data range.
#'
#' @param id Namespace of the module.
#' @param dat The data supplied as `dataframe` or tibble.
#' @param labels Labels for the controllers (defaults to the variable names).
#' @param logi Label for the controller of logical variables (defaults to
#'  `"Various"`.
#' @param update Logical indicating whether ui controllers are ui- or
#'  server-side for updating the controllers ranges.
#' @param session The shiny session domain.
#' @param shinyjs A logical indicating whether shinyjs `ToggleState()` is
#'  included to disable controllers.
#' @param external `reactiveValues` assigning external filter variables to the
#'  module.
#' @param ignore Character string/vector for variables to be ignored for
#'  filtering of the dataset.
#' @param remove_na Logical indicating whether NAs should be removed during
#'  filtering (defaults to FALSE).
#'
#' @return Shiny module.
#' @export
filter_ui <- function(id) {
  uiOutput(NS(id, "ctrls"))
}

filter_controllers <- function(
    dat,
    id,
    labels = character(1),
    external = character(1),
    logi = character(1),
    ignore = character(1),
    update = FALSE,
    shinyjs = FALSE,
    remove_na = FALSE
  ) {

  # check for shinyjs
  if (all(isTRUE(shinyjs), !requireNamespace("shinyjs", quietly = TRUE))) {
    stop(
      "Package \"shinyjs\" must be installed to use this function.",
      call. = FALSE
    )
  }

  # defuse
  dat <- rlang::enquo(dat)

  # create automatically detected controllers
  ctrls <- rlang::inject(detect_controller(!!dat, id, labels, external, remove_na, update))

  # add shinyjs switch controls
  if (all(isTRUE(shinyjs), isTRUE(update))) {
    swth <- rlang::inject(switch_controller(!!dat, remove_na = remove_na))
  }

  # create logical controller (needs updating)
  # if (!isFALSE(detect_lgl(dat, ignore, external))) {
  if (FALSE) {

    logi <- rlang::inject(
      logical_controller(!!substitute(dat), id, logi, external, ignore,
                         remove_na, update)
    )

    # add to other controllers
    ctrls$logi <- logi

    # add shinyjs switch controls (logical)
    if (all(isTRUE(shinyjs), isTRUE(update))) {

      swth_lgl <- switch_controller(dat, external= external, ignore = ignore,
                                    logical = TRUE)

      # add to additional switches
      swth$logi <- swth_lgl
    }
  }

  # add shinjs
  if (all(isTRUE(shinyjs), isFALSE(update))) {
    ctrls <- append(
      list(rlang::call2("useShinyjs", .ns = "shinyjs")),
      ctrls
    )
  }

  # merge controllers and switchers by name
  try(ctrls <- purrr::list_merge(swth, !!!ctrls), silent = TRUE)

  ctrls
}

#' @rdname filter_ui
#'
#' @export
filter_server <- function(
    dat,
    id,
    labels = character(1),
    external = reactiveValues(),
    logi = character(1),
    ignore = character(1),
    update = FALSE,
    shinyjs = FALSE,
    remove_na = FALSE
  ) {

  stopifnot(is.reactive(dat))
  stopifnot(is.reactivevalues(external))
  stopifnot(is.character(ignore))

  moduleServer(id, function(input, output, session) {


    # render the controllers
    output$ctrls <- renderUI({

      # prevent flickering when switching dataset
      req(dat(), cancelOutput = TRUE)

      # make controllers based on data
      ctrls <- filter_controllers(dat(), id, labels, names(external), logi,
                                  ignore, update, shinyjs, remove_na)
      # create HTML
      purrr::map(ctrls, eval)

    }) |>
      bindEvent(dat())

    # store input in custom `reactivalues`
    input2 <- reactiveValues()
    # bind input in custom `reactivalues`
    observe({
      purrr::walk(names(input),  ~{input2[[.x]] <- input[[.x]]})
      purrr::walk(names(external),  ~{input2[[.x]] <- external[[.x]]})
    })

    # variable names
    vars <- reactive({variable_names(dat(), ignore = ignore)})

    # filter observations
    obs <- reactive({

      purrr::map(
        c(vars(), input2$logi),
        ~filter_var(dat()[[.x]], input2[[.x]], remove_na = remove_na)
      ) %>%
        purrr::reduce(`&`)
    })

    # return filtered data
    filter <- reactive({

      # this requires a vector of length > 0
      req(obs())


      dat()[obs(), , drop = FALSE]
    })

    # update the controllers to match the new data ranges
    observeEvent(dat(), {

      # prevent flickering when switching dataset
      req(filter())

      # update controls
      ctrls <- filter_controllers(filter(), session, labels, names(external), logi,
                                  ignore, update = TRUE, shinyjs, remove_na)

      # if logical variables exist "logi" is appended
      if (detect_lgl(filter(), names(external), ignore)) {
        vars <- c(vars(), "logi")
      } else {
        vars <- vars()
      }

      # execute
      purrr::walk(vars, ~observe_builder(.x, y = ctrls, dat = filter()))
    })

    # return only non-logical column vars
    reactive({filter()[, col_spec(filter()) != "logical", drop = FALSE]})

  })
}

#-------------------------------------------------------------------------------
# helper functions
#-------------------------------------------------------------------------------

# reactive value names for input
variable_names <- function(dat, ignore = character(1)) {
  # no logical columns
  nms <- names(col_spec(dat))[col_spec(dat) != "logical"]
  # discard ignored columns
  nms[!nms %in% ignore]
}

# filter operation on the dataset based on variable class
filter_var <- function(x, val = NULL, remove_na = FALSE) {

  # shortcut with val `NULL` or other none Truthy vals
  if (all(!isTruthy(x), !isTruthy(val)))  return(TRUE)

  if (is.numeric(x)) {
    y <- x >= val[1] & x <= val[2]
  } else if (any(is.character(x), is.factor(x))) {
    y <- x %in% val
  } else if (all(is.logical(x), is.null(val))) {
    y <- x
  } else {
    # No control, so don't filter
    y <- TRUE
  }
  if (isTRUE(remove_na)) y  & !is.na(x) else y | is.na(x)
}

observe_builder <- function(x, y, dat, show = FALSE) {

  # build expression `observeEvent`
  sel <- y[!names(y) %in% x]
  nms <- names(y)[!names(y) %in% x]

  # event
  evt <- rlang::call2("$", rlang::sym("input2"), x)

  # original data
  dt <- rlang::enexpr(dat)
  org <- rlang::call2("$", dt, x)

  # exit by `req` validation of filter operation
  if (x == "logi") {

    # logical columns available
    filter <- rlang::call2(
      "detect_lgl",
      dt,
      external = rlang::sym("external"),
      ignore = rlang::sym("ignore")
    )

    # filter <- rlang::expr(length(!!filter_cols) > 0)

  } else {

    # levels or ranges in variables available
    filter <- rlang::expr(
      any(!!rlang::call2("filter_var", org , evt, remove_na = FALSE))
    )

  }

  exit <- rlang::call2("req", filter, cancelOutput = TRUE)

  # combine handle
  xprs <- rlang::list2(exit, !!!rev(rlang::flatten(unname(sel))))

  # for debugging purposes also enable viewing the `observeEvent` expression
  if (isTRUE(show)) {

    rlang::call2(
      "observeEvent",
      eventExpr = evt,
      handlerExpr = rlang::expr({!!!xprs}),
      .ns = "shiny"
    )

  } else {

    shiny::observeEvent(
      eventExpr = evt,
      handlerExpr = rlang::expr({!!!xprs}),
      event.env = rlang::caller_env(),
      event.quoted = TRUE,
      handler.env = rlang::caller_env(),
      handler.quoted = TRUE,
      ignoreInit = TRUE
    )

  }
}

