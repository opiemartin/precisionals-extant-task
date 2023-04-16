library(writexl)

source("src/ext/main.r")

diagnosis_before_onset <- ext_main %>%
    select(
        site, id,
        date_of_diagnosis, age_at_diagnosis,
        date_of_onset, age_at_onset
    ) %>%
    filter(
        age_at_diagnosis < age_at_onset |
            date_of_diagnosis < date_of_onset
    )

gastrostomy_before_onset <- ext_main %>%
    select(
        site, id,
        date_of_gastrostomy, age_at_gastrostomy,
        date_of_onset, age_at_onset
    ) %>%
    filter(
        age_at_gastrostomy < age_at_onset | date_of_gastrostomy < date_of_onset
    )

tracheostomy_before_onset <- ext_main %>%
    select(
        site, id,
        date_of_tracheostomy, age_at_tracheostomy,
        date_of_onset, age_at_onset
    ) %>%
    filter(
        age_at_tracheostomy < age_at_onset | date_of_tracheostomy < date_of_onset
    )

niv_before_onset <- ext_main %>%
    select(
        site, id,
        date_of_niv, age_at_niv,
        date_of_onset, age_at_onset
    ) %>%
    filter(
        age_at_niv < age_at_onset | date_of_niv < date_of_onset
    )

output_dir <- "output/qc"
output_path <- file.path(output_dir, "events-before-onset.xlsx")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write_xlsx(list(
    "Diagnosis" = diagnosis_before_onset,
    "Gastrostomy" = gastrostomy_before_onset,
    "NIV" = niv_before_onset,
    "Tracheostomy" = tracheostomy_before_onset
), output_path)
