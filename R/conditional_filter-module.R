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
#' @param external Character string/vector (filter_ui) or `reactiveValues`
#'  (filter_server) assigning external filter variables to the module.
#' @param ignore Character string/vector for variables to be ignored for
#'  filtering of the dataset.
#' @param remove_na Logical indicating whether NAs should be removed during
#'  filtering (defaults to FALSE).
#'
#' @return Shiny module.
#' @export
filter_ui <- function(id, dat, external = NULL, labels = NULL, logi = NULL,
                      update = FALSE, session =  getDefaultReactiveDomain(),
                      shinyjs = FALSE, ignore = NULL, remove_na = FALSE) {

  # classes
  col_specs <- col_spec(dat, external = external, ignore = ignore)

  # variable names and types
  var <- names(col_specs)[col_specs != "logical"] # remove logicals and externals from names
  type <- detect_control(dat, update = update, external = external, ignore = ignore)
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
  args <- purrr::pmap(ls_args, col_vals, update = update, shinyjs = shinyjs,
                      remove_na = remove_na)

  # logical selection
  if (any(col_specs == "logical")) {
    call_logi_cols <- rlang::call2("logi_cols", substitute(dat), ignore = ignore, external = external)
    args$logi <- list(
      inputId = if (isTRUE(update)) "logi" else NS(id, "logi"),
      choices = if (isTRUE(update)) call_logi_cols else eval(call_logi_cols),
      selected = if (isTRUE(update)) {
        rlang::call2("isolate", rlang::expr(input$logi), .ns = "shiny")
      } else {
        NULL
      }
    )

    if (isFALSE(update)) {
      if (!is.null(logi)) {
        args$logi$label <- logi
      } else {
        args$logi$label <- "Various"
      }
      args$logi$multiple <- TRUE
      args$logi$options <- list(
        placeholder = "select",
        onInitialize = I('function() { this.setValue(null); }')
      )
    } else if (isTRUE(shinyjs)) {
      args$logi$switch <- switch_controller("logi", call_logi_cols, "length", 0)
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
filter_server <- function(id, dat, external = reactiveValues(),
                          ignore = character(0), shinyjs = FALSE,
                          remove_na = FALSE) {

  stopifnot(is.reactive(dat))
  stopifnot(is.reactivevalues(external))
  stopifnot(is.character(ignore))

  moduleServer(id, function(input, output, session) {

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
      hdl <- filter_ui(dat = filter(), external = names(external),
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
# variable class data
col_spec <- function(dat, external = NULL, ignore = NULL) {
  if (!is.null(external) | !is.null(ignore)) {
    dat <- dat[, !(names(dat) %in% c(external, ignore)), drop = FALSE]
  }
  vapply(dat, class, character(1))
}

# reactive value names for input
variable_names <- function(dat , ignore = NULL) {
  names(col_spec(dat))[col_spec(dat) != "logical" & !(names(dat) %in% ignore)]
}

# determine type of gui controller based on variable class
detect_control <- function(data, update = FALSE, external = NULL, ignore = NULL) {
  control <- c("numeric" = "sliderInput", "character" = "selectInput",
               "factor" = "selectInput")
  # get types of variables except logicals
  types <- col_spec(data, external = external, ignore = ignore)
  cnr <- control[types[types != "logical"]]
  if (any(col_spec(data, external = external, ignore = ignore) == "logical")) {
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
                     update = FALSE, shinyjs = FALSE, remove_na = FALSE) {

  stopifnot(is.character(class))
  stopifnot(is.character(col))

  if (class == "numeric") {
    vls <- rlang::exprs(
      inputId = !!rlang::get_expr(id),
      min = !!fun_rng("min", col, na.rm = TRUE),
      max = !!fun_rng("max", col, na.rm = TRUE),
      value = !!fun_rng("range", col, na.rm = TRUE)
      )
    if (isTRUE(update)) {
      # select what is needed for updating
      vls <- vls[c("inputId", "value")]
      if (isTRUE(shinyjs)) {
        vls$switch <- switch_controller(col, fun_rng("unique", col), "length", 1)
      }
    }
  } else if (class == "character" | class == "factor") {
    vls <- rlang::exprs(
      inputId = !!rlang::get_expr(id),
      choices = !!fun_lvls(col, remove_na = remove_na),
      selected = !!fun_lvls(col, remove_na = remove_na)
      )
    if (isTRUE(shinyjs) & isTRUE(update)) {
      vls$switch <- switch_controller(
        col,
        fun_lvls(col, remove_na = remove_na),
        "length",
        1
      )
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

# generate expression for character/logical toggle switch condition
fun_lvls <- function(col, remove_na) {
  if (isTRUE(remove_na)) {
    col <- rlang::parse_expr(col)
  } else {
    col <- rlang::call2("na.omit", rlang::parse_expr(col))
  }
  rlang::call2("unique", col)
}

# generate expression for numeric toggle switch condition
fun_rng <- function(stat, col, ...) {
  rlang::call2(stat, rlang::parse_expr(col), ...)
}

# generating expression for logical vars toggle switch condition
logi_cols <- function(dat, external, ignore) {

  col_specs <- col_spec(dat, ignore = ignore, external = external)
  vrs <- names(col_specs)[col_specs == "logical"]

  # check if columns have any `TRUE`
  chk <- vapply(dat[, vrs, drop = FALSE], any, logical(1), na.rm = TRUE)
  vrs[chk]
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

# toggle state
switch_controller <- function(var, x, method, val) {
  var <- gsub("(.)*\\$", "", var)
  fun <- deparse1(rlang::call2(method, x), collapse = " ")
  cond <- rlang::parse_expr(paste(fun, ">", val))
  rlang::call2("toggleState", var, cond, .ns = "shinyjs")

}
