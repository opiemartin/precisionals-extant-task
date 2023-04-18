library(readxl)
library(dplyr)
library(stringr)

ext_load_data <- function(path, ...) {
    data_dir <- Sys.getenv("PALS_EXTANT_DATADIR", unset = "./data")
    read_excel(file.path(data_dir, path), na = c(
        "Missing", "N/A", "NA", "Unknown"
    ), ...) %>% ext_normalize_names()
}

ext_normalize_names <- function(xs) {
    xs %>%
        rename_with(~ .x %>%
            str_to_lower() %>%
            str_replace_all("<", "lt_") %>%
            str_replace_all(">", "gt_") %>%
            str_replace_all("[^A-Za-z0-9]+", "_") %>%
            str_replace_all("(^_)|(_$)", "") %>%
            str_replace_all("^([^A-Za-z])", "x\\1"))
}

ext_parse_boolean <- function(x) {
    x %>% case_match(
        c("Yes", "yes") ~ TRUE,
        c("No", "no") ~ FALSE
    )
}
