recode_report <- function(label) {
  recode(label, "compReport" = "Report completeness", "timeReport" = "Report timeliness")
}

recode_review <- function(label) {
  recode(label, "compReview" = "Review completeness", "timeReview" = "Review timeliness")
}

round_ratio_to_perc <- function(x) round(x * 100, 2)

round_review_report <- function(data) {
  data %>%
    mutate_at(c("compReview", "timeReview", "compReport", "timeReport"), round_ratio_to_perc)
}
