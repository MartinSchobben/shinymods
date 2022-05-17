test_that("multiplication works", {
  ft <- filterStore$new(original = cars, vars = reactiveValues(speed = NULL), ignore = "dist", shinyjs = TRUE, remove_na = FALSE)
  input <- list()
  input$speed <- c(4, 8)
  ft$filter(input)
  expect_equal(ft$original, cars)
  expect_equal(ft$augmented, dplyr::filter(cars, dplyr::between(.data$speed, 4, 8)))
  # additional filter
  x <- reactiveValues(dist = c(2, 4))
  input$dist <- c(2, 4)
  ft$addvars(x)
  ft$filter(input)
  expect_equal(
    ft$augmented,
    cars[cars$dist >= 2 & cars$dist <= 4 & cars$speed >= 4 & cars$speed <= 8, , drop = FALSE]
  )
})
