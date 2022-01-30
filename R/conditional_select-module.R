filter_ui <- function(id, data, labels = NULL, logi = NULL, update = FALSE,
                      session =  NULL) {

  col_specs <- col_spec(data)

  #id
  var <- names(data)[col_specs != "logical"]

  if (isFALSE(update)) {
    ids <- purrr::map_chr(var, ~NS(id, .x))
    id <- NS(id, "logi")
  } else {
    ids <- var
    id <- "logi"
  }

  type <- detect_control(data, update = update)

  ls_args <- list(
    col = data[col_specs != "logical"],
    id = ids
    )

  if (!is.null(labels)) {
    ls_args$label <- labels
    ls_args
  }

  args <- purrr::pmap(ls_args, col_vals)

  # logical selection
  args$logical <- list(
    inputId = id,
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
    sls
  } else {
    purrr::map(sls, eval)
  }
}

filter_server <- function(id, data) {

  stopifnot(is.reactive(data))

  moduleServer(id, function(input, output, session) {

    vars <- reactive({
      v <- names(col_spec(data())[col_spec(data()) != "logical"])
      if (any(col_spec(data()) == "logical")) {
        v <- append(v, "logi")
      }
      v
      })

    obs <- reactive({
      purrr::map(vars(), ~filter_var(data()[[.x]], input[[.x]])) %>%
        purrr::reduce(`&`)
    })

    # return data
    filter <- reactive({data()[obs(), , drop = FALSE]})

    # update the controllers to match the new data ranges
    hdl <- reactive({
      filter_ui(id = id, data = filter(), update = TRUE, session = session)
    })
    observe({
      message(hdl())
      purrr::walk2(vars(), hdl(), ~eval(observe_builder(.x, vars(), .y)))
    })

    # return
    filter
  })
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

col_vals <- function(col, id, label = NULL) {

  if (is.numeric(col)) {
    rng <- range(col, na.rm = TRUE)
    vls <- list(inputId = id, min = rng[1], max = rng[2], value = rng)
  } else if (is.character(col) | is.factor(col)) {
    vls <- list(inputId = id, choices = levels(as.factor(col)))
  } else {
    # Not supported
    NULL
  }
  if (!is.null(label)) vls$label <- label
  vls

}

# cond_server <- function(id, data, vars) {
#
#   stopifnot(is.reactive(data))
#   stopifnot(is.reactive(vars))
#
#   moduleServer(id, function(input, output, session) {
#
#   hdl <- reactive({
#     filter_ui(id = id, data = data(), update = TRUE, session = session)
#   })
#   observe(purrr::walk2(vars(), hdl(), ~observe_builder(.x, data(), .y, vars())))
#
#   })
# }

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

observe_builder <- function(x, y, handle) {

  # build expression `observeEvent`
  sel <- y[!y %in% x]

  make_cond <- function(x, y) substitute({a;b}, env = list(a = x, b = y))
  cond <- purrr::map(
    sel,
    ~call("$", rlang::sym("input"), rlang::sym(.x))
    ) %>%
    purrr::reduce(make_cond)

  fr <- rlang::call2("freezeReactiveValue", rlang::sym("input"), x, .ns = "shiny")

  rlang::call2(
    "observeEvent",
    eventExpr = cond,
    handlerExpr = substitute({fr;handle}),
    #handlerExpr = {rlang::expr(message({{x}}))}, # convenient for testing
    .ns = "shiny"
    )
  # shiny::observeEvent(
  #   eventExpr = cond,
  #   #handlerExpr = handle,
  #   handlerExpr = message(x),
  #   #event.quoted = TRUE,
  #   #handler.quoted = TRUE
  #   )
}

