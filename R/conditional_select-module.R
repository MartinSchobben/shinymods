filter_ui <- function(id, dat, labels = NULL, logi = NULL, update = FALSE,
                      session =  getDefaultReactiveDomain(), shinyjs = FALSE) {

  # classes
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
  args <- purrr::pmap(ls_args, col_vals, update = update, shinyjs = shinyjs)

  # logical selection
  if (any(col_specs == "logical")) {
    args$logical <- list(
      inputId = if (isTRUE(update)) "logi" else NS(id, "logi"),
      choices = names(col_specs)[col_specs == "logical"],
      selected = names(col_specs)[col_specs == "logical"]
      )

    if (isFALSE(update)) {
      args$logical$multiple = TRUE
      if (!is.null(logi)) {
        args$logical$label <- logi
      } else {
        args$logical$label <- "Various"
      }
    }
  }

  # append session and get exit
  if (isTRUE(update)) {
    args <- purrr::map(args, ~append(.x, c(session = session)))
    # exit <- purrr::map(args, list("exit"))
    # args <- purrr::map(args, ~purrr::list_modify(.x, "exit" = NULL))
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
      #%>% purrr::list_merge(!!!exit)
    } else {
      purrr::list_merge(svr) #, !!!exit)
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

filter_server <- function(id, dat, shinyjs = FALSE) {

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
      hdl <- filter_ui(dat = filter(), update = TRUE, session = session,
                       shinyjs = shinyjs)
      purrr::walk(vars(), ~observe_builder(.x, hdl, dat = filter()))
    },
    once = TRUE
    )

    # return
    filter
  })
}


# variable class data
col_spec <- function(data) vapply(data, class, character(1))

# reactive value names for input
variable_names <- function(dat) {
  v <- names(col_spec(dat))[col_spec(dat) != "logical"]
  if (any(col_spec(dat) == "logical")) {
    v <- append(v, "logi")
  }
  v
}

# determine type of gui controller based on variable class
detect_control <- function(data, update = FALSE) {
  control <- c("numeric" = "sliderInput", "character" = "selectInput",
               "factor" = "selectInput")
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

# returns body of arguments for gui controllers
col_vals <- function(class, col, id, label = NULL, session = NULL,
                     update = FALSE, shinyjs = FALSE) {

  stopifnot(is.character(col))

  if (class == "numeric") {
    fun_rng <- function(stat, col) {
      rlang::call2(stat, rlang::parse_expr(col), na.rm = TRUE)
    }
    vls <- rlang::exprs(
      min = !!fun_rng("min", col),
      max = !!fun_rng("max", col),
      value = !!fun_rng("range", col)
      )

    vls$inputId <- rlang::get_expr(id)
    # if (isTRUE(update)) {
    #   exit <- rlang::call2("filter_var", rlang::parse_expr(col), call("$", rlang::sym("input"), gsub("(.)*\\$", "", col)))
    #   vls$exit <- rlang::expr(req(any(!!exit), cancelOutput = TRUE))
    # }
    if (isTRUE(shinyjs) & isTRUE(update)) {
      vls$switch <- switch_controller(col, fun_rng("range", col), method = "diff")
    }
  } else if (class == "character" | class == "factor") {
    fun_lvls <- function(col) {
      rlang::call2("unique", rlang::parse_expr(col))
      # %>%
      #   rlang::call2("levels", .)
    }
    vls <- rlang::exprs(
      choices = !!fun_lvls(col),
      selected = !!fun_lvls(col)
      )
    vls$inputId <- rlang::get_expr(id)
    # if (isTRUE(update)) {
    #   exit <- rlang::call2("filter_var", rlang::parse_expr(col), call("$", rlang::sym("input"), gsub("(.)*\\$", "", col)))
    #   vls$exit <- rlang::expr(req(any(!!exit), cancelOutput = TRUE))
    # }
    if (isTRUE(shinyjs) & isTRUE(update)) {
      vls$switch <- switch_controller(col, fun_lvls(col), method = "length")
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

  if (is.numeric(x)) {
    y <- x >= val[1] & x <= val[2]
  } else if (is.character(x) | is.factor(x)) {
    y <- x %in% val
  } else if (is.logical(x)) {
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
  evt <- call("$", rlang::sym("input"), x)
  org <- call("$", substitute(dat), x)

  # exit
  exit <- rlang::call2("filter_var", org , evt, remove_na = FALSE)
  exit <- rlang::expr(req(any(!!exit), cancelOutput = TRUE))

  # combine handle
  xprs <- rlang::list2(exit, !!!rev(rlang::flatten(unname(sel))))

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
      handler.quoted = TRUE
    )
  }

}

# toggle state
switch_controller <- function(var, x, method) {
  var <- gsub("(.)*\\$", "", var)
  cond <- rlang::parse_expr(paste(deparse(rlang::call2(method, x)), "> 1"))
  rlang::call2("toggleState", var, cond, .ns = "shinyjs")
}
