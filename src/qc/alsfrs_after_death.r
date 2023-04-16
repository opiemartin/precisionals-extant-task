library(writexl)

source("src/ext/main.r")
source("src/ext/alsfrs.r")

alsfrs_after_death_by_date <- ext_alsfrs %>%
    select(id, date_of_assessment) %>%
    group_by(id) %>%
    summarize(date_of_last_assessment = max(date_of_assessment)) %>%
    left_join(ext_main, by = "id") %>%
    filter(date_of_last_assessment > date_of_death) %>%
    select(id, site, date_of_death, date_of_last_assessment)

alsfrs_after_death_by_age <- ext_alsfrs %>%
    select(id, age_at_assessment) %>%
    group_by(id) %>%
    summarize(age_at_last_assessment = max(age_at_assessment)) %>%
    left_join(ext_main, by = "id") %>%
    filter(age_at_last_assessment > age_at_death) %>%
    select(id, site, age_at_death, age_at_last_assessment)

alsfrs_after_death <- alsfrs_after_death_by_date %>%
    full_join(alsfrs_after_death_by_age, by = c("id", "site"))

output_dir <- "output/qc"
output_path <- file.path(output_dir, "alsfrs-after-death.xlsx")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write_xlsx(alsfrs_after_death, output_path)
