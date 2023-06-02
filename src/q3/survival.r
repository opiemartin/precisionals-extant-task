library(dplyr)
library(readxl)
library(stringr)
library(survival)
library(ggsurvfit)
library(magrittr)
library(xfun)

q3_filter_data <- function(data, event, origin, by) {
    data %<>%
        filter(event == .env$event, origin == .env$origin)

    if (by == "causal_gene") {
        filter(data, causal_gene != "Multiple")
    } else if (by == "site_of_onset") {
        filter(data, site_of_onset %in% c(
            "Bulbar", "Cognitive", "Respiratory", "Spinal"
        ))
    } else if (by == "sod1_status") {
        filter(data, sod1_status != "Unknown effect")
    } else {
        data
    }
}

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

q3_origins <- list(
    "birth" = "birth",
    "onset" = "onset",
    "diagnosis" = "diagnosis"
)

q3_events <- list(
    "onset" = "onset",
    "diagnosis" = "diagnosis",
    "walking support" = "walking_support",
    "respiratory onset" = "respiratory_onset",
    "NIV" = "niv",
    "NIV ≥23h" = "niv_23h",
    "tracheostomy" = "tracheostomy",
    "gastrostomy" = "gastrostomy",
    "death" = "death",
    "King's 1" = "kings_1",
    "King's 2" = "kings_2",
    "King's 3" = "kings_3",
    "King's 4" = "kings_4",
    "MiToS 1" = "mitos_1",
    "MiToS 2" = "mitos_2",
    "MiToS 3" = "mitos_3",
    "MiToS 4" = "mitos_4"
)

q3_subgroups <- list(
    "overall" = "*overall*",
    "site" = "site",
    "sex" = "sex",
    "age at onset" = "age_at_onset",
    "C9orf72 status" = "c9orf72_status",
    "SOD1 status" = "sod1_status",
    "TARDBP status" = "tardbp_status",
    "FUS status" = "fus_status",
    "causal gene" = "causal_gene",
    "site of onset" = "site_of_onset"
)

q3_data_path <- "output/q3/time-to-event.xlsx"
if (!file.exists(q3_data_path)) {
    source("src/q3/timetoevent.r")
}
q3_data <- read_excel(q3_data_path)

for (orig_label in names(q3_origins)) {
    orig_value <- q3_origins[[orig_label]]
    for (grp_label in names(q3_subgroups)) {
        grp_value <- q3_subgroups[[grp_label]]
        for (evt_label in names(q3_events)) {
            evt_value <- q3_events[[evt_label]]
            if (evt_value == orig_value) next
            if (evt_value == "onset" && orig_value == "diagnosis") next

            xlab <- str_glue("Time from {orig_label}, months")
            title <- str_glue("Time to {evt_label}") %>% q3_str_to_title()
            data <- q3_filter_data(q3_data, evt_value, orig_value, grp_value)
            if (grp_value == "*overall*") {
                output_name <- str_glue("time-from-{orig_value}-to-{evt_value}")
                km_fit <- survfit2(Surv(duration, status == "event") ~ 1, data)
                p <- ggsurvfit(km_fit) + add_quantile()
            } else {
                output_name <- str_glue("time-from-{orig_value}-to-{evt_value}-by-{grp_value}")
                km_fit <- survfit2(as.formula(
                    str_glue("Surv(duration, status == 'event') ~ {grp_value}")
                ), data)
                p <- ggsurvfit(km_fit) +
                    add_pvalue("annotation") +
                    add_legend_title(q3_str_to_sentence(grp_label))
            }

            p +
                scale_ggsurvfit() +
                add_confidence_interval() +
                labs(title = title, x = xlab)
            
            grp_dir <- file.path("output/q3", grp_value)
            dir.create(grp_dir, showWarnings = FALSE)
            ggsave(file.path(grp_dir, output_name %>% with_ext(".png")))
        }
    }
}
