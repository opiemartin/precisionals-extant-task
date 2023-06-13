suppressPackageStartupMessages({
    library(dplyr)
    library(survival)
    library(ggsurvfit)
    library(lubridate)
    library(magrittr)
    library(progress)
    library(rlang)
    library(xfun)
})

source("src/ext/resp.r")
source("src/q3/common.r")

q3_survplots_output_width <- 7
q3_survplots_output_height <- 7
q3_survplots_output_dpi <- 300

q3_survplots_filter_data <- function(data, event, origin, by) {
    data %<>% filter(event == .env$event, origin == .env$origin)

    if (by == "causal_gene") {
        data %>% filter(causal_gene != "Multiple")
    } else if (by == "site_of_onset") {
        data %>% filter(site_of_onset %in% c(
            "Bulbar", "Respiratory", "Spinal"
        ))
    } else {
        data
    }
}

q3_origin_labels <- list(
    birth = "birth",
    onset = "onset",
    diagnosis = "diagnosis"
)

q3_event_labels <- list(
    onset = "onset",
    diagnosis = "diagnosis",
    walking_support = "walking support",
    respiratory_onset = "respiratory onset",
    vc_decline = "decline in vital capacity (<80%)",
    ventilatory_support = "ventilatory support",
    niv = "NIV",
    niv_23h = "NIV >23h",
    tracheostomy = "tracheostomy",
    gastrostomy = "gastrostomy",
    kings_1 = "King's 1",
    kings_2 = "King's 2",
    kings_3 = "King's 3",
    kings_4 = "King's 4",
    kings_5 = "King's 5",
    mitos_1 = "MiToS 1",
    mitos_2 = "MiToS 2",
    mitos_3 = "MiToS 3",
    mitos_4 = "MiToS 4",
    mitos_5 = "MiToS 5",
    death = "death"
)

q3_subgroup_labels <- list(
    "@overall" = "overall",
    site = "site",
    sex = "sex",
    age_at_onset = "age at onset",
    progression_category = "progression category",
    c9orf72_status = "C9orf72 status",
    sod1_status = "SOD1 status",
    tardbp_status = "TARDBP status",
    fus_status = "FUS status",
    causal_gene = "causal gene",
    site_of_onset = "site of onset"
)

q3_epoch_units <- list(
    birth = "years",
    .otherwise = "months"
)

q3_data_path <- "output/q3/time-to-event.rds"
if (!file.exists(q3_data_path)) {
    source("src/q3/timetoevent.r")
}
q3_data <- readRDS(q3_data_path)

progress_bar <- progress::progress_bar$new(
    format = "exporting [:bar] :current/:total (:percent)",
    total = length(q3_origin_labels) * length(q3_event_labels) * length(q3_subgroup_labels)
)

progress_bar$tick(0)
for (origin in names(q3_origin_labels)) {
    orig_label <- q3_origin_labels[[origin]]
    epoch_unit <- q3_epoch_units[[origin]] %||% q3_epoch_units$.otherwise
    for (group in names(q3_subgroup_labels)) {
        grp_label <- q3_subgroup_labels[[group]]
        for (event in names(q3_event_labels)) {
            evt_label <- q3_event_labels[[event]]
            if (!q3_is_valid_event_from_origin(event, origin)) {
                progress_bar$tick()
                next
            }

            title <- q3_str_to_title(evt_label)
            xlab <- str_glue("Time from {orig_label}, {epoch_unit}")
            data <- q3_data %>% q3_survplots_filter_data(
                event = event, origin = origin, by = group
            )
            data$duration <- data$duration / duration(1, epoch_unit)

            if (group == "@overall") {
                output_name <- str_glue("time-from-{origin}-to-{event}")
                km_fit <- survfit2(Surv(duration, status == "event") ~ 1, data)
                p <- ggsurvfit(km_fit) + add_quantile()
            } else {
                output_name <- str_glue("time-from-{origin}-to-{event}-by-{group}")
                km_fit <- survfit2(as.formula(
                    str_glue("Surv(duration, status == 'event') ~ {group}")
                ), data)
                p <- ggsurvfit(km_fit) +
                    add_pvalue("annotation") +
                    add_legend_title(q3_str_to_sentence(grp_label))
            }

            p +
                scale_ggsurvfit() +
                add_confidence_interval() +
                labs(title = title, x = xlab)

            grp_dir <- file.path("output/q3", group)
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
