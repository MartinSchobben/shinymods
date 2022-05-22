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
filter_ui <- function(
    dat,
    session,
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
  # check that shinyjs is only used for the server side (update)
  if (all(isTRUE(shinyjs), isFALSE(update))) {
    stop(
      "Shinyjs only work on server side and requires `update = TRUE`.",
      call. = FALSE
    )
  }

  # create automatically detected controllers
  ctrls <- rlang::inject(
    detect_controller(!!substitute(dat), session, labels, external, remove_na,
                      update)
  )

  # add shinyjs switch controls
  if (isTRUE(shinyjs)) {
    swth <- rlang::inject(
      switch_controller(!!substitute(dat), remove_na = remove_na)
    )
  }

  # create logical controller
  if (!isFALSE(detect_lgl(dat, ignore, external))) {

    logi <- rlang::inject(
      logical_controller(!!substitute(dat), session, logi, external, ignore,
                         remove_na, update)
    )

    # add to other controllers
    ctrls$logi <- logi

    # shinyjs
    if (isTRUE(shinyjs)) {

      swth_lgl <- rlang::inject(
        switch_controller(!!substitute(dat), external= external,
                          ignore = ignore, logical = TRUE)
      )
      # add to additional switches
      swth$logi <- swth_lgl
    }
  }

  # merge controllers and switchers by name
  purrr::list_merge(swth, !!!ctrls)
}

#' @rdname filter_ui
#'
#' @export
filter_server <- function(id, dat, external = reactiveValues(),
                          ignore = character(0), shinyjs = FALSE,
                          remove_na = FALSE) {

  stopifnot(is.reactive(dat))
  stopifnot(is.reactivevalues(external))
  stopifnot(is.character(ignore))

  moduleServer(id, function(input, output, session) {


    # render the controllers
    output$ctrl <- renderUI({
      # original <- substitute(dat())
      filter_controls(id = id, dat = dat(), external = names(external),
                      ignore = ignore, shinyjs = shinyjs, remove_na = remove_na)
    })

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

    # return data
    filter <- reactive({
      dat()[obs(), , drop = FALSE]
    })

    # update the controllers to match the new data ranges
    observeEvent(filter(), {
      hdl <- filter_controls(dat = filter(), external = names(external),
                             update = TRUE, session = session,
                             ignore = ignore, shinyjs = shinyjs,
                             remove_na = remove_na)
      # if logical variables exist "logi" is appended
      if (length(logi_cols(filter(), names(external), ignore)) > 0) {
        vars <- c(vars(), "logi")
      } else {
        vars <- vars()
      }
      purrr::walk(vars, ~observe_builder(.x, y = hdl, dat = filter()))
    },
    once = TRUE
    )

    # return only non-logical column vars
    reactive({filter()[, col_spec(filter()) != "logical", drop = FALSE]})
  })
}

#-------------------------------------------------------------------------------
# helper functions
#-------------------------------------------------------------------------------


# filter operation on the dataset based on variable class
filter_var <- function(x, val = NULL, remove_na = FALSE) {

  # # shortcut with val `NULL`
  if (all(is.null(x), is.null(val)))  return(TRUE)

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
  org <- rlang::call2("$", rlang::expr(!!dt), x)

  # exit by `req` validation of filter operation
  if (x == "logi") {
    # logical columns available
    filter_cols <- rlang::call2(
      "logi_cols",
      rlang::parse_expr(paste0(dt, "()")),
      external = rlang::sym("external"),
      ignore = rlang::sym("ignore")
      )
    filter <- rlang::expr(length(!!filter_cols) > 0)
  } else {
    # levels or ranges in variables available
    filter <- rlang::expr(any(!!rlang::call2("filter_var", org , evt, remove_na = FALSE)))
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

