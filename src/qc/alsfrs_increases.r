library(dplyr)
library(tidyr)
library(writexl)

source("src/ext/alsfrs.r")

alsfrs_increases <- ext_alsfrs %>%
    group_by(id) %>%
    arrange(date_of_assessment, age_at_assessment, .by_group = TRUE) %>%
    mutate(
        prev_total_score = lag(total_score),
        prev_age_at_assessment = lag(age_at_assessment),
        prev_date_of_assessment = lag(date_of_assessment),
        total_score_diff = total_score - lag(total_score)
    ) %>%
    ungroup() %>%
    select(
        id, site,
        ends_with("total_score"),
        ends_with("date_of_assessment"),
        ends_with("age_at_assessment"),
        total_score_diff
    ) %>%
    drop_na(id, site, total_score_diff) %>%
    filter(total_score_diff > 0) %>%
    arrange(desc(total_score_diff))

dir.create("output/qc", recursive = TRUE, showWarnings = FALSE)
write_xlsx(alsfrs_increases, "output/qc/alsfrs-increases.xlsx")
