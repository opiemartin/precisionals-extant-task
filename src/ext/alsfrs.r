source("src/ext/common.r")

ext_alsfrs <- suppressWarnings(ext_load(
    "P-ALS_Ext_ALSFRS-R.xlsx",
    col_types = c(
        "text", # ID
        "text", # Site
        "date", # Date of Assessment
        "numeric", # Age of Assessment
        rep("numeric", times = 15) # ...
    )
)) %>%
    rename_with(~ str_replace(.x, "x(\\d+[abx]?)_", "q\\1_")) %>%
    rename_with(~ str_replace_all(.x, "hygine", "hygiene")) %>%
    rename(age_at_assessment = "age_of_assessment")

ext_alsfrs_progression_category <- function(x) {
    case_when(
        x < 0.8 ~ "SP",
        x %>% between(0.8, 1.35) ~ "NP",
        x > 1.35 ~ "FP"
    )
}
