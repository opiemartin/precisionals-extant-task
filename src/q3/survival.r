library(dplyr)
library(readxl)
library(stringr)
library(survival)
library(ggsurvfit)
library(xfun)

q3_origins <- list(
    "birth" = "birth",
    "onset" = "onset"
)

q3_events <- list(
    "walking support" = "walking_support",
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
} else {
    q3_data <- read_excel(q3_data_path)
}

q3_str_to_title <- function(s) {
    s %>%
        str_to_title() %>%
        str_replace_all("Fus", "FUS") %>%
        str_replace_all("Mitos", "MiToS") %>%
        str_replace_all("Niv", "NIV") %>%
        str_replace_all("Sod1", "SOD1") %>%
        str_replace_all("Tardbp", "TARDBP")
}

for (orig_label in names(q3_origins)) {
    orig_value <- q3_origins[[orig_label]]
    for (grp_label in names(q3_subgroups)) {
        grp_value <- q3_subgroups[[grp_label]]
        for (evt_label in names(q3_events)) {
            evt_value <- q3_events[[evt_label]]
            xlab <- str_glue("Time from {orig_label}, months")
            title <- str_glue("Time to {evt_label}") %>% q3_str_to_title()
            data <- q3_data %>% filter(origin == orig_value, event == evt_value)
            if (grp_value == "*overall*") {
                output_name <- str_glue("time-from-{orig_value}-to-{evt_value}")
                km_fit <- survfit2(Surv(duration, status == "event") ~ 1, data)
                p <- ggsurvfit(km_fit) + add_quantile()
            } else {
                output_name <- str_glue("time-from-{orig_value}-to-{evt_value}-by-{grp_value}")
                km_fit <- survfit2(as.formula(
                    str_glue("Surv(duration, status == 'event') ~ {grp_value}")
                ), data)
                p <- ggsurvfit(km_fit) + add_pvalue("annotation") + add_legend_title(grp_label)
            }

            p +
                scale_ggsurvfit() +
                add_confidence_interval() +
                labs(title = title, x = xlab)

            ggsave(file.path("output/q3/", evt_value, output_name %>% with_ext(".png")))
        }
    }
}
