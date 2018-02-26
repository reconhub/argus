recode_report <- function(label) {
  recode(label, "compReport" = "Completeness of weekly reporting", "timeReport" = "Timeliness of weekly reporting")
}

recode_review <- function(label) {
  recode(label, "compReview" = "Completeness of weekly reviewing", "timeReview" = "Timeliness of weekly reviewing")
}

round_ratio_to_perc <- function(x) round(x * 100, 2)

round_review_report <- function(data) {
  data %>%
    mutate_at(c("compReview", "timeReview", "compReport", "timeReport"), round_ratio_to_perc)
}
