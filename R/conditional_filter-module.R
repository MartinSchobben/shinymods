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
#'
#' @return Shiny module.
#' @export
filter_ui <- function(id, dat, external = NULL, labels = NULL, logi = NULL,
                      update = FALSE, session =  getDefaultReactiveDomain(),
                      shinyjs = FALSE) {

  # classes
  col_specs <- col_spec(dat, external = external)

  # variable names and types
  var <- names(col_specs)[col_specs != "logical"] # remove logicals and externals from names
  type <- detect_control(dat, update = update, external = external)
  var_exprs <- paste0(deparse(substitute(dat)),"$", var)
  ls_args <- list(
    class = col_specs[col_specs != "logical"],
    col = var_exprs,
    id = if (isTRUE(update)) var else NS(id, var)
    )
  if (isFALSE(update)) {
    if (!is.null(labels)) {
      ls_args$label <- labels
      ls_args
    } else {
      ls_args$label <- var
      ls_args
    }
  }

  # make body of arguments for shiny controllers
  args <- purrr::pmap(ls_args, col_vals, update = update, shinyjs = shinyjs)

  # logical selection
  if (any(col_specs == "logical")) {
    args$logical <- list(
      inputId = if (isTRUE(update)) "logi" else NS(id, "logi"),
      choices = names(col_specs)[col_specs == "logical"]
    )

    if (isFALSE(update)) {
      if (!is.null(logi)) {
        args$logical$label <- logi
      } else {
        args$logical$label <- "Various"
      }
      args$logical$multiple <- TRUE
      args$logical$options <- list(
        placeholder = "select",
        onInitialize = I('function() { this.setValue(null); }')
      )
    }
  }

  # append session and get exit
  if (isTRUE(update)) {
    args <- purrr::map(args, ~append(.x, c(session = session)))
  }

  # gui controllers
  if(isTRUE(shinyjs)) {
    swth <- purrr::map(args, list("switch"))
    args <- purrr::map(args, ~purrr::list_modify(.x, "switch" = NULL))
  }
  sls <- purrr::map2(type, args, ~rlang::call2(.x, !!!.y, .ns = "shiny"))

  # return gui controllers
  if (isTRUE(update)) {
    svr <- rlang::set_names(sls, nm = purrr::map_chr(args, list("inputId")))
    # use shinyjs
    if (isTRUE(shinyjs) & requireNamespace("shinyjs"))  {
      purrr::list_merge(svr, !!!swth)
    } else {
      svr
    }
  } else {
    ui <- purrr::map(sls, rlang::eval_tidy)
    # use shinyjs
    if (isTRUE(shinyjs) & requireNamespace("shinyjs"))  {
      tagAppendChild(ui, shinyjs::useShinyjs())
    } else {
      ui
    }
  }
}
#' @rdname filter_ui
#'
#' @export
filter_server <- function(id, dat, external = reactiveValues(), shinyjs = FALSE) {

  stopifnot(is.reactive(dat))
  stopifnot(is.reactivevalues(external))

  moduleServer(id, function(input, output, session) {

    # store input in custom `reactivalues`
    input2 <- reactiveValues()
    # bind input in custom `reactivalues`
    observe({
      purrr::walk(names(input),  ~{input2[[.x]] <- input[[.x]]})
      purrr::walk(names(external),  ~{input2[[.x]] <- external[[.x]]})
    })

    # variable names
    vars <- reactive({variable_names(dat())})

    # filter observations
    obs <- reactive({
      purrr::map(
        c(vars(), input2$logi),
        ~filter_var(dat()[[.x]], input2[[.x]])
      ) %>%
        purrr::reduce(`&`)
    })

    # return data
    filter <- reactive({
      dat()[obs(), , drop = FALSE]
    })

    # update the controllers to match the new data ranges
    observeEvent(filter(), {
      hdl <- filter_ui(dat = filter(), external = external, update = TRUE,
                       session = session, shinyjs = shinyjs)
      purrr::walk(vars(), ~observe_builder(.x, y = hdl, dat = filter()))
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
# variable class data
col_spec <- function(dat, external = NULL) {
  if (!is.null(external)) {
    dat <- dat[, !(names(dat) %in% external), drop = FALSE]
  }
  vapply(dat, class, character(1))
}

# reactive value names for input
variable_names <- function(dat , external = NULL) {
  names(col_spec(dat))[col_spec(dat) != "logical"]
}

# determine type of gui controller based on variable class
detect_control <- function(data, update = FALSE, external = NULL) {
  control <- c("numeric" = "sliderInput", "character" = "selectInput",
               "factor" = "selectInput")
  # get types of variables except logicals
  types <- col_spec(data, external = external)
  cnr <- control[types[types != "logical"]]
  if (any(col_spec(data, external = external) == "logical")) {
    cnr <- append(cnr, c("logical" = "selectizeInput"))
  }
  if (isTRUE(update)) {
    substring(cnr, 1) <- toupper(substring(cnr, 1, 1))
    paste0("update", cnr)
  } else {
    unname(cnr)
  }
}

# returns body of arguments for gui controllers
col_vals <- function(class, col, id, label = NULL, session = NULL,
                     update = FALSE, shinyjs = FALSE) {

  stopifnot(is.character(col))

  if (class == "numeric") {
    fun_rng <- function(stat, col, ...) {
      rlang::call2(stat, rlang::parse_expr(col), ...)
    }
    vls <- rlang::exprs(
      min = !!fun_rng("min", col, na.rm = TRUE),
      max = !!fun_rng("max", col, na.rm = TRUE),
      value = !!fun_rng("range", col, na.rm = TRUE)
      )
    vls$inputId <- rlang::get_expr(id)
    if (isTRUE(shinyjs) & isTRUE(update)) {
      vls$switch <- switch_controller(col, fun_rng("unique", col), "length", 1)
    }
  } else if (class == "character" | class == "factor") {
    fun_lvls <- function(col) {
      rlang::call2("unique", rlang::parse_expr(col))
    }
    vls <- rlang::exprs(
      choices = !!fun_lvls(col),
      selected = !!fun_lvls(col)
      )
    vls$inputId <- rlang::get_expr(id)
    if (isTRUE(shinyjs) & isTRUE(update)) {
      vls$switch <- switch_controller(col, fun_lvls(col), "length", 1)
    }
  } else {
    # Not supported
    NULL
  }
  if (!is.null(label) & isFALSE(update)) vls$label <- label
  if (isTRUE(update)) vls$session <- session
  if (isFALSE(update) & (class == "character" | class == "factor")) {
    vls$multiple <- TRUE
  }
  vls
}

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

observe_builder <- function(x, y, dat, show = FALSE) {

  # build expression `observeEvent`
  sel <- y[!names(y) %in% x]
  nms <- names(y)[!names(y) %in% x]

  # event
  evt <- rlang::call2("$", rlang::sym("input"), x)
  # original data
  dt <- rlang::enexpr(dat)
  org <- rlang::call2("$", rlang::expr(!!dt), x)

  # exit by `req` validation of filter operation
  filter <- rlang::call2("filter_var", org , evt, remove_na = FALSE)
  exit <- rlang::call2("req", rlang::expr(any(!!filter)), cancelOutput = TRUE)

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

# toggle state
switch_controller <- function(var, x, method, val) {
  var <- gsub("(.)*\\$", "", var)
  cond <- rlang::parse_expr(paste(deparse(rlang::call2(method, x)), ">", val))
  rlang::call2("toggleState", var, cond, .ns = "shinyjs")
}
