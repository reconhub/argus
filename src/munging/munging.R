#### JG; put a header with the objective of the script

recode_report <- function(label) {
  recode(label, "compReport" = i18n$t("compReport"),
         "timeReport" = i18n$t("timeReport"))
}

recode_review <- function(label) {
  recode(label, "compReview" = i18n$t("compReview"),
         "timeReview" = i18n$t("timeReview"))
}

round_ratio_to_perc <- function(x) round(x * 100, 2)

round_review_report <- function(data) {
  data %>%
    mutate_at(c("compReview", "timeReview", "compReport", "timeReport"), round_ratio_to_perc)
}
