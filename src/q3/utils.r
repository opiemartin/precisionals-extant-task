library(stringr)

q3_str_restore_allcaps <- function(s) {
    s %>%
        str_replace_all("Fus", "FUS") %>%
        str_replace_all("Mitos", "MiToS") %>%
        str_replace_all("Niv", "NIV") %>%
        str_replace_all("Sod1", "SOD1") %>%
        str_replace_all("Tardbp", "TARDBP")
}

q3_str_to_sentence <- function(s) {
    s %>%
        str_to_sentence() %>%
        q3_str_restore_allcaps()
}

q3_str_to_title <- function(s) {
    s %>%
        str_to_title() %>%
        q3_str_restore_allcaps()
}
