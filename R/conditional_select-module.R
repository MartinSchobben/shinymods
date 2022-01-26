filter_ui <- function(id, data, labels = NULL, logi = NULL, update = FALSE) {

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

  if (is.null(labels)) ls_args <- append(ls_args, labels)

  args <- purrr::pmap(ls_args, col_vals)

  # logical selection
  args$logical <- list(
    inputID = id,
    choices = names(col_specs)[col_specs == "logical"]
    )

  if (is.null(logi)) args$logical$label <- logi
  purrr::map2(type, args, ~rlang::call2(.x, !!!.y, .ns = "shiny"))
}

filter_server <- function(id, vars) {

  moduleServer(id, function(input, output, session) {

    purrr::iwalk(
      vars,
      ~{
        select <- list()
        select_name <- paste0("select", .y)
        select[[select_name]] <- reactive({
          dplyr::filter(GTS, )
        })
        observeEvent(select[[select_name]](),{
          select[[select_name]]()

        })
      }
    )
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
  if (is.null(label)) vls$label <- label
  vls

}

react_builder <- function(tb, vars, input = list()) {
 filter_expr <- purrr::imap(vars, ~rlang::parse_expr(paste(.x, "== input$", paste0("depth", .y))))
 purrr::map(filter_expr, ~rlang::call2("filter", tb, .x, .ns = "dplyr"))
 # purrr::imap(vars, ~call("dplyr::filter", tb, )
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

observe_filter <- function(x, data, handle, ...) {

  args <- rlang::enquos(...)
  y <- names(args)

  #
  sel <- args[!y %in% names(x)]
  make_cond <- function(x, y) substitute(a|b, env = list(a = x, b = y))
  cond <- purrr::map(1:length(sel), ~call("$", rlang::sym("input"), rlang::sym(names(sel[.x])))) %>%
    purrr::reduce(make_cond)

  rlang::call2("observeEvent", eventExpr = cond, handlerExpr = handle , .ns = "shiny")

  #input[!names(input) %in% names(x)]
  #rlang::call2("$", sym("input"), sym("id-filter"))

}

