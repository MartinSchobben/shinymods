filter_ui <- function(id, dat, labels = NULL, logi = NULL, update = FALSE,
                      session =  NULL) {

  col_specs <- col_spec(dat)

  # variable names and types
  var <- names(dat)[col_specs != "logical"]
  type <- detect_control(dat, update = update)

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
  args <- purrr::pmap(ls_args, col_vals)

  # logical selection
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
  } else {
    args <- purrr::map(args, ~append(.x, c(session = session)))
  }
  sls <- purrr::map2(type, args, ~rlang::call2(.x, !!!.y, .ns = "shiny"))

  if (isTRUE(update)) {

    rlang::set_names(sls, nm = names(purrr::map_chr(args, list("inputId"))))

  } else {
    purrr::map(sls, rlang::eval_tidy)
  }
}

filter_server <- function(id, dat) {

  stopifnot(is.reactive(dat))

  moduleServer(id, function(input, output, session) {

    # variable names
    vars <- reactive({variable_names(dat())})

    # filter observations
    obs <- reactive({
      purrr::map(vars(), ~filter_var(dat()[[.x]], input[[.x]])) %>%
        purrr::reduce(`&`)
    })

    # return data
    filter <- reactive({dat()[obs(), , drop = FALSE]})

    # update the controllers to match the new data ranges
    observeEvent(filter(), {
      hdl <- filter_ui(dat = filter(), update = TRUE, session = session)
      message(hdl)
      purrr::walk(vars(), ~observe_builder(.x, hdl))
    },
    once = TRUE
    )


    # return
    filter
  })
}





variable_names <- function(dat) {
  v <- names(col_spec(dat))[col_spec(dat) != "logical"]
  if (any(col_spec(dat) == "logical")) {
    v <- append(v, "logi")
  }
  v
}

detect_control <- function(data, update = FALSE) {
  control <- c("numeric" = "sliderInput", "character" = "selectInput")
  cnr <- control[col_spec(data)[col_spec(data) != "logical"]]
  if (any(col_spec(data) == "logical")) {
    cnr <- append(cnr, c("logical" = "selectInput"))
  }
  if (isTRUE(update)) {
    substring(cnr, 1) <- toupper(substring(cnr, 1, 1))
    paste0("update", cnr)
  } else {
    unname(cnr)
  }
}

col_spec <- function(data) vapply(data, class, character(1))

col_vals <- function(class, col, id, label = NULL, session, update = FALSE) {

  stopifnot(is.character(col))

  if (class == "numeric") {
    fun_rng <- function(stat, col) rlang::call2(stat, rlang::parse_expr(col), na.rm = TRUE)
    vls <- rlang::exprs(
      min = !!fun_rng("min", col),
      max = !!fun_rng("max", col),
      value = !!fun_rng("range", col)
      )
    vls$inputId <- rlang::get_expr(id)
  } else if (class == "character" | class == "factor") {
    fun_lvls <- function(col) {
      rlang::call2("as.factor", rlang::parse_expr(col)) %>%
        rlang::call2("levels", .)
    }
    vls <- rlang::exprs(
      choices = !!fun_lvls(col),
      selected = !!fun_lvls(col)
      )
    vls$inputId <- rlang::get_expr(id)
  } else {
    # Not supported
    NULL
  }
  if (!is.null(label) & isFALSE(update)) vls$label <- label
  if (isTRUE(update)) vls$session <- session
  if (isFALSE(update) & (col == "character" | col == "factor")) {
    vls$multiple <- TRUE
  }
  vls

}


filter_var <- function(x, val = NULL) {

  if (is.numeric(x)) {
    !is.na(x) & x >= val[1] & x <= val[2]
  } else if (is.character(x) | is.factor(x)) {
    !is.na(x) & x %in% val
  } else if (is.logical(x)) {
    !is.na(x) & x
  } else {
    # No control, so don't filter
    TRUE
  }
}

observe_builder <- function(x, y) {

  # build expression `observeEvent`
  sel <- y[!names(y) %in% x]
  nms <- names(y)[!names(y) %in% x]

  make_handle <- function(x, y) substitute({a;b}, env = list(a = x, b = y))

  #make_freeze
  freeze_val <- function(x) {
    rlang::call2("freezeReactiveValue", rlang::sym("input"), x, .ns = "shiny")
  }
  frozen_vals <- purrr::map(nms, freeze_val)

  hdl <- purrr::reduce(sel, make_handle)

  # rlang::call2(
  #   "observeEvent",
  #   eventExpr = call("$", rlang::sym("input"), x),
  #   handlerExpr = hdl,
  #   .ns = "shiny"
  #   )
  shiny::observeEvent(
    eventExpr = call("$", rlang::sym("input"), x),
    handlerExpr = hdl,
    event.env = rlang::caller_env(),
    event.quoted = TRUE,
    handler.env = rlang::caller_env(),
    handler.quoted = TRUE
    )
}

