
xc <- cbind(lg1 = rep(TRUE, nrow(cars)), cars, lg2 = rep(TRUE, nrow(cars)))

test_that("gui can be generated", {
  expect_equal(
    detect_control(xc),
    c("sliderInput", "sliderInput", "selectInput")
    )
  expect_snapshot(
    col_vals(xc$speed, "id", "speed", NULL, FALSE)
  )
  expect_snapshot(
    col_vals(xc$speed, "id", "speed", NULL, TRUE)
  )
  expect_snapshot(
    filter_ui("id", xc, c("Speed", "Distribution"))
  )
})


test_that("that server works" {
  input <- list()
  input$speed <- 1:10
  input$dist <- 21:50
  input$logi <- rep(TRUE, 10)
  hdl <- filter_ui(id = "id", data = xc, update = TRUE)
  purrr::imap(hdl, ~observe_filter(input[.y], xc, .x, !!!input))
})
