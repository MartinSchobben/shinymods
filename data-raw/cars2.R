## code to prepare `cars2` dataset goes here

cars2 <- cars %>%
  dplyr::mutate(
    high_speed = .data$speed > mean(.data$speed),
    .before = .data$speed
   ) %>%
  dplyr::mutate(
    high_dist = .data$dist > mean(.data$dist),
    .after = .data$dist
  )

usethis::use_data(cars2, overwrite = TRUE)
