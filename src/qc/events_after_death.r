library(writexl)

source("src/ext/main.r")

last_follow_up_after_death <- ext_main %>%
    select(
        site, id,
        date_of_last_follow_up, age_at_last_follow_up,
        date_of_death, age_at_death
    ) %>%
    filter(
        age_at_last_follow_up > age_at_death |
            date_of_last_follow_up > date_of_death
    )

diagnosis_after_death <- ext_main %>%
    select(
        site, id,
        date_of_diagnosis, age_at_diagnosis,
        date_of_death, age_at_death
    ) %>%
    filter(
        age_at_diagnosis > age_at_death |
            date_of_diagnosis > date_of_death
    )

gastrostomy_after_death <- ext_main %>%
    select(
        site, id,
        date_of_gastrostomy, age_at_gastrostomy,
        date_of_death, age_at_death
    ) %>%
    filter(
        age_at_gastrostomy > age_at_death |
            date_of_gastrostomy > date_of_death
    )

tracheostomy_after_death <- ext_main %>%
    select(
        site, id,
        date_of_tracheostomy, age_at_tracheostomy,
        date_of_death, age_at_death
    ) %>%
    filter(
        age_at_tracheostomy > age_at_death |
            date_of_tracheostomy > date_of_death
    )

niv_after_death <- ext_main %>%
    select(
        site, id,
        date_of_niv, age_at_niv,
        date_of_death, age_at_death
    ) %>%
    filter(
        age_at_niv > age_at_death |
            date_of_niv > date_of_death
    )

output_dir <- "output/qc"
output_path <- file.path(output_dir, "events-after-death.xlsx")
dir.create("output/qc", recursive = TRUE, showWarnings = FALSE)
write_xlsx(list(
    "Last follow-up" = last_follow_up_after_death,
    "Diagnosis" = diagnosis_after_death,
    "Gastrostomy" = gastrostomy_after_death,
    "NIV" = niv_after_death,
    "Tracheostomy" = tracheostomy_after_death
), output_path)
