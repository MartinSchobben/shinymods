#' Wizard module
#'
#' The module creates a wizard that takes you through a selection of slides.
#'
#' @param id Namespace of the module.
#' @param tabs `Taglist` with elements to be included within the tabs. The
#'  number of elements determines the number of slides in the wizard.
#'
#' @export
wizard_ui <- function(id, tabs) {

  # create tabs
  tabs <- tab_creator(id, tabs)
  # create tab panel
  tabsetPanel(
    id = NS(id, "wizard"),
    type = "hidden",
    !!! tabs
  )

}
#' @rdname wizard_ui
#'
#' @export
wizard_server <- function(id, tabs) {

  moduleServer(id, function(input, output, session) {

    # functions to switch tabs
    switch_tab <- function(from, to) {
      observeEvent(input[[paste0("page_", from, to)]], {
        updateTabsetPanel(session, "wizard", selected = paste0("page_", to))
      })
    }

  # go forward
  purrr::map(seq_len(length(tabs))[-1], ~switch_tab(.x, .x - 1))

  # go back
  purrr::map(seq_len(length(tabs))[-length(tabs)], ~switch_tab(.x, .x + 1))

  })
}

tab_creator <- function(id, tabs) {

  range <- seq_along(tabs)

  # function to populate tab pane with buttons
  tab_buttons <- function(a, b, c, tab, title, id) {
    tabPanel(
      title = paste0("page_", a),
      titlePanel(title = title),
      fluidRow(column(12, tab)),
      fluidRow(
        column(
          6,
          if (!is.na(b))
            actionButton(NS(id, paste0("page_", a, b)), "prev")
        ),
        column(
          6,
          if (!is.na(c))
            actionButton(NS(id, paste0("page_", a, c)), "next")
        )
      )
    )
  }
  # execute function
  purrr::pmap(
    list(
      a = range,
      b = dplyr::lag(range),
      c = dplyr::lead(range),
      tab = tabs,
      title = names(tabs)
    ),
    tab_buttons,
    id = id
  )
}
