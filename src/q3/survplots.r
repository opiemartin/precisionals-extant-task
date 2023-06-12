library(dplyr)
library(survival)
library(ggsurvfit)
library(lubridate)
library(magrittr)
library(progress)
library(rlang)
library(xfun)

source("src/q3/utils.r")

q3_survplots_output_width <- 7
q3_survplots_output_height <- 7
q3_survplots_output_dpi <- 300

q3_survplots_filter_data <- function(data, event, origin, by) {
    data %<>%
        filter(event == .env$event, origin == .env$origin)

    if (by == "causal_gene") {
        filter(data, causal_gene != "Multiple")
    } else if (by == "site_of_onset") {
        filter(data, site_of_onset %in% c(
            "Bulbar", "Respiratory", "Spinal"
        ))
    } else {
        data
    }
}

q3_survplots_skip_event <- function(event, origin) {
    if (event == origin) {
        return(TRUE)
    }
    if (event == "onset" && origin != "birth") {
        return(TRUE)
    }
    FALSE
}

q3_origins <- list(
    "birth" = "birth",
    "onset" = "onset",
    "diagnosis" = "diagnosis"
)

q3_epoch_units <- list(
    birth = "years",
    .otherwise = "months"
)

q3_events <- list(
    "onset" = "onset",
    "diagnosis" = "diagnosis",
    "walking support" = "walking_support",
    "respiratory onset" = "respiratory_onset",
    "ventilatory support" = "ventilatory_support",
    "NIV" = "niv",
    "NIV ≥23h" = "niv_23h",
    "tracheostomy" = "tracheostomy",
    "gastrostomy" = "gastrostomy",
    "King's 1" = "kings_1",
    "King's 2" = "kings_2",
    "King's 3" = "kings_3",
    "King's 4" = "kings_4",
    "King's 5" = "kings_5",
    "MiToS 1" = "mitos_1",
    "MiToS 2" = "mitos_2",
    "MiToS 3" = "mitos_3",
    "MiToS 4" = "mitos_4",
    "MiToS 5" = "mitos_5",
    "death" = "death"
)

q3_subgroups <- list(
    "overall" = "@overall",
    "site" = "site",
    "sex" = "sex",
    "age at onset" = "age_at_onset",
    "progression category" = "progression_category",
    "C9orf72 status" = "c9orf72_status",
    "SOD1 status" = "sod1_status",
    "TARDBP status" = "tardbp_status",
    "FUS status" = "fus_status",
    "causal gene" = "causal_gene",
    "site of onset" = "site_of_onset"
)

q3_data_path <- "output/q3/time-to-event.rds"
if (!file.exists(q3_data_path)) {
    source("src/q3/timetoevent.r")
}
q3_data <- readRDS(q3_data_path)

progress_bar <- progress::progress_bar$new(
    format = "exporting [:bar] :current/:total (:percent)",
    total = length(q3_origins) * length(q3_events) * length(q3_subgroups)
)

progress_bar$tick(0)
for (orig_label in names(q3_origins)) {
    orig_value <- q3_origins[[orig_label]]
    epoch_unit <- q3_epoch_units[[orig_value]] %||% q3_epoch_units$.otherwise
    for (grp_label in names(q3_subgroups)) {
        grp_value <- q3_subgroups[[grp_label]]
        for (evt_label in names(q3_events)) {
            evt_value <- q3_events[[evt_label]]
            if (q3_survplots_skip_event(evt_value, orig_value)) {
                progress_bar$tick()
                next
            }

            title <- q3_str_to_title(evt_label)
            xlab <- str_glue("Time from {orig_label}, {epoch_unit}")
            data <- q3_survplots_filter_data(q3_data, evt_value, orig_value, grp_value)
            data$duration <- data$duration / duration(1, epoch_unit)

            if (grp_value == "@overall") {
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
            output_path <- file.path(grp_dir, output_name %>% with_ext(".png"))
            ggsave(output_path,
                width = q3_survplots_output_width,
                height = q3_survplots_output_height,
                dpi = q3_survplots_output_dpi
            )
            progress_bar$tick()
        }
    }
}
