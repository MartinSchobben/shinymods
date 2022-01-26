ls_GTS <- read.csv("https://paleobiodb.org/data1.2/intervals/list.txt?scale_id=1") %>%
  dplyr::select(interval_name, interval_no, parent_no, scale_level) %>%
  dplyr::group_by(scale_level) %>%
  dplyr::group_split(.keep = FALSE)

join_fun <- function(x, y) {
  dplyr::left_join(x, y, by = c("interval_no" = "parent_no")) %>%
    dplyr::mutate(interval_no = interval_no.y, .keep = "unused")
}

GTS <-purrr::reduce(ls_GTS, join_fun) %>%
  dplyr::select(-c(interval_no ,parent_no))

colnames(GTS) <- c("Eon", "Era", "Period", "Stage", "Substage")

usethis::use_data(GTS, overwrite = TRUE)
